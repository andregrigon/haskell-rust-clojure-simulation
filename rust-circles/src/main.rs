use gl;
use sdl2;
use std;
use std::f32::consts::PI;
use std::ffi::{CStr, CString};

fn current_time() -> Instant {
    Instant {
        secs: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    }
}

pub struct Interval {
    secs: f64,
}

pub struct Instant {
    secs: f64,
}

#[derive(Clone, Copy)]
pub struct Vec2(f64, f64);

impl Vec2 {
    fn plus(self: &Self, Vec2(x2, y2): &Vec2) -> Vec2 {
        let Vec2(x1, y1) = self;
        Vec2(x1 + x2, y1 + y2)
    }

    fn times(self: &Self, k: f64) -> Vec2 {
        let Vec2(x, y) = self;
        Vec2(k * x, k * y)
    }

    fn dot(self: &Self, Vec2(x2, y2): &Vec2) -> f64 {
        let Vec2(x1, y1) = self;
        (x1 * x2) + (y1 * y2)
    }

    fn distance_squared(self: &Self, Vec2(x2, y2): &Vec2) -> f64 {
        let Vec2(x1, y1) = self;
        (x1 - x2).powi(2) + (y1 - y2).powi(2)
    }

    fn length(self: &Self) -> f64 {
        f64::sqrt(self.distance_squared(&Vec2(0.0, 0.0)))
    }

    fn normalize(self: &Self) -> Vec2 {
        let vl = self.length();
        let vl2 = if vl < 0.01 { 0.01 } else { vl };
        self.times(1.0 / vl2)
    }

    fn reflect(self: &Self, n: &Vec2) -> Vec2 {
        let n2 = n.normalize();
        self.plus(&n2.times(-2.0 * (self.dot(&n2))))
    }
}

#[derive(Clone, Copy)]
pub struct Color(f64, f64, f64);

#[derive(Clone)]
pub struct Object {
    id: i64,
    radius: f64,
    position: Vec2,
    speed: Vec2,
    color: Color,
}

pub struct Bound {
    id: i64,
    radius: f64,
    position: Vec2,
}

type Visuals = (Vec<f32>, Vec<f32>);

pub struct Collision(Vec2);

const WALL: f64 = 10.0;

impl Bound {
    fn collides_with(self: &Self, other: &Bound) -> bool {
        self.id != other.id
            && self.position.distance_squared(&other.position)
                < (self.radius + other.radius).powi(2)
    }

    fn detect_collisions(self: &Self, bs: &Vec<&Bound>) -> Option<Collision> {
        let mut collisions = bs
            .into_iter()
            .filter(|b| self.collides_with(b))
            .map(|b| Collision(b.position.plus(&self.position.times(-1.0))));
        match collisions.next() {
            Some(c) => Some(c),
            None => None,
        }
    }
}

const SPLIT_FAC: f64 = 0.7;
const MIN_SPLIT_RAD: f64 = 0.1;
const GROWTH_PER_SEC: f64 = 0.02;
const MAX_GROWTH_RAD: f64 = 1.0;

impl Object {
    fn evolve(self: &Self, maybe_col: &Option<Collision>, interval: &Interval) -> Vec<Object> {
        fn outside_walls(a: &Object) -> bool {
            let Vec2(x, y) = a.position;
            x.abs() > WALL || y.abs() > WALL
        }
        if outside_walls(&self) {
            vec![]
        } else {
            let collided = maybe_col.is_some();
            let speed2 = match maybe_col {
                Some(Collision(dir)) => self.speed.reflect(dir),
                None => self.speed,
            };
            let boost = if collided { 3.0 } else { 1.0 };
            let position2 = self.position.plus(&speed2.times(interval.secs * boost));
            let radius2 = if self.radius < MAX_GROWTH_RAD {
                self.radius + (GROWTH_PER_SEC * interval.secs)
            } else {
                self.radius
            };
            if collided && self.radius > MIN_SPLIT_RAD {
                vec![
                    Object {
                        speed: speed2,
                        position: position2,
                        radius: self.radius * SPLIT_FAC,
                        ..self.clone()
                    },
                    Object {
                        speed: speed2.times(-1.0),
                        position: position2,
                        radius: self.radius * SPLIT_FAC,
                        ..self.clone()
                    },
                ]
            } else {
                vec![Object {
                    speed: speed2.clone(),
                    position: position2,
                    radius: radius2,
                    ..self.clone()
                }]
            }
        }
    }

    fn bound(self: &Self) -> Bound {
        Bound {
            id: self.id,
            radius: self.radius,
            position: self.position,
        }
    }

    fn draw(self: &Self) -> Visuals {
        circle(&self.color, self.radius, &self.position)
    }
}

struct World {
    objects: Vec<Object>,
    age: f64,
}

impl std::fmt::Display for World {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:.2} s: {} objects", self.age, self.objects.len())
    }
}

impl World {
    fn evolve(self: &Self, interval: &Interval) -> World {
        let bounds = &self.objects.iter().map(|x| x.bound()).collect();
        fn collide_evolve(a: &Object, bounds: &Vec<Bound>, interval: &Interval) -> Vec<Object> {
            let bound = a.bound();
            let Vec2(x, y) = bound.position;
            let top_wall = Bound {
                id: -1,
                radius: 0.0,
                position: Vec2(x, WALL),
            };
            let bottom_wall = Bound {
                id: -2,
                radius: 0.0,
                position: Vec2(x, -WALL),
            };
            let left_wall = Bound {
                id: -3,
                radius: 0.0,
                position: Vec2(-WALL, y),
            };
            let right_wall = Bound {
                id: -4,
                radius: 0.0,
                position: Vec2(WALL, y),
            };
            let mut walls_and_bounds = vec![&top_wall, &bottom_wall, &left_wall, &right_wall];
            let _ = &walls_and_bounds.extend(bounds);

            let cols = bound.detect_collisions(&walls_and_bounds);
            a.evolve(&cols, interval)
        }
        World {
            objects: self
                .objects
                .iter()
                .map(|a| collide_evolve(&a, &bounds, &interval))
                .flatten()
                .collect(),
            age: self.age + interval.secs,
        }
    }

    fn draw(self: &Self, program: &Program, window: &sdl2::video::Window) {
        let drawings = self.objects.iter().map(|o| o.draw()).collect();
        render(program, &drawings);
        window.gl_swap_window();
    }
}

fn simulate(program: &Program, window: &sdl2::video::Window, duration: f64, initial_world: World) {
    let mut start_time = current_time();
    let mut world = initial_world;
    let mut total = 0.0;
    let fps_window = 0.25;
    let mut fps_interval = 0.0;
    let mut fps_count = 0;
    loop {
        let Instant { secs: i } = start_time;
        world.draw(program, window);
        let Instant { secs: i2 } = current_time();
        let dt = i2 - i;
        world = world.evolve(&Interval { secs: dt });
        total += dt;
        if fps_interval >= fps_window {
            let fps = fps_count as f64 / fps_interval;
            println!(
                "{}{:.2} fps | {}",
                if fps < 55.0 { "! " } else { "" },
                fps,
                world
            );
            fps_interval = 0.0;
            fps_count = 0;
        } else {
            fps_interval += dt;
            fps_count += 1;
        }
        start_time = Instant { secs: i2 };
        if total >= duration {
            break;
        }
    }
}

fn render(program: &Program, vertices_colors: &Vec<(Vec<f32>, Vec<f32>)>) {
    let (width, height) = (WINDOW_WIDTH as i32, WINDOW_HEIGHT as i32);
    unsafe {
        gl::Viewport(0, 0, width, height);
        gl::ClearColor(0.7, 0.7, 0.7, 1.0);
        gl::Clear(gl::COLOR_BUFFER_BIT);
    }
    program.set_used();

    unsafe {
        for (vertices, colors) in vertices_colors {
            gl::EnableVertexAttribArray(0);
            gl::VertexAttribPointer(
                0,         // index of the generic vertex attribute ("layout (location = 0)")
                3,         // the number of components per generic vertex attribute
                gl::FLOAT, // data type
                gl::FALSE, // normalized (int-to-float conversion)
                (3 * std::mem::size_of::<f32>()) as gl::types::GLint, // stride (byte offset between consecutive attributes)
                vertices.as_ptr() as *const gl::types::GLvoid, // offset of the first component
            );
            gl::EnableVertexAttribArray(1);
            gl::VertexAttribPointer(
                1,         // index of the generic vertex attribute ("layout (location = 0)")
                3,         // the number of components per generic vertex attribute
                gl::FLOAT, // data type
                gl::FALSE, // normalized (int-to-float conversion)
                (3 * std::mem::size_of::<f32>()) as gl::types::GLint, // stride (byte offset between consecutive attributes)
                colors.as_ptr() as *const gl::types::GLvoid, // offset of the first component
            );
            gl::DrawArrays(gl::TRIANGLES, 0, vertices.len() as i32 / 3);
            gl::DisableVertexAttribArray(0);
            gl::DisableVertexAttribArray(1);
        }
    }
}

fn circle(Color(rd, gd, bd): &Color, radd: f64, Vec2(xd, yd): &Vec2) -> Visuals {
    let (r, g, b) = (*rd as f32, *gd as f32, *bd as f32);
    let rad = radd as f32;
    let x = *xd as f32;
    let y = *yd as f32;

    let vertex_count = TRIANGLES_PER_CIRCLE * 3;
    let coord_count = vertex_count * 3;
    let theta = 2.0 * PI / TRIANGLES_PER_CIRCLE as f32;
    let color = |i: u64| match i % 3 {
        0 => r,
        1 => g,
        2 => b,
        _ => panic!("impossible"),
    };
    let colors: Vec<f32> = (0..coord_count).map(color).collect();
    let vertex = |i: u64| {
        let n = i / 9;
        match i % 9 {
            0 => rad * (theta * (n as f32)).cos() + x,
            1 => rad * (theta * (n as f32)).sin() + y,
            2 => 0.0,
            3 => rad * (theta * ((n + 1) as f32)).cos() + x,
            4 => rad * (theta * ((n + 1) as f32)).sin() + y,
            5 => 0.0,
            6 => x,
            7 => y,
            8 => 0.0,
            _ => panic!("impossible"),
        }
    };
    let vertices: Vec<f32> = (0..coord_count).map(vertex).collect();

    (vertices, colors)
}

const RED: Color = Color(1.0, 0.0, 0.0);
const GREEN: Color = Color(0.0, 1.0, 0.0);
const BLUE: Color = Color(0.0, 0.0, 1.0);

pub struct Program {
    id: gl::types::GLuint,
}

impl Program {
    pub fn from_shaders(shaders: &[Shader]) -> Result<Program, String> {
        let program_id = unsafe { gl::CreateProgram() };

        for shader in shaders {
            unsafe {
                gl::AttachShader(program_id, shader.id());
            }
        }

        unsafe {
            gl::LinkProgram(program_id);
        }

        let mut success: gl::types::GLint = 1;
        unsafe {
            gl::GetProgramiv(program_id, gl::LINK_STATUS, &mut success);
        }

        if success == 0 {
            let mut len: gl::types::GLint = 0;
            unsafe {
                gl::GetProgramiv(program_id, gl::INFO_LOG_LENGTH, &mut len);
            }

            let error = create_whitespace_cstring_with_len(len as usize);

            unsafe {
                gl::GetProgramInfoLog(
                    program_id,
                    len,
                    std::ptr::null_mut(),
                    error.as_ptr() as *mut gl::types::GLchar,
                );
            }

            return Err(error.to_string_lossy().into_owned());
        }

        for shader in shaders {
            unsafe {
                gl::DetachShader(program_id, shader.id());
            }
        }

        Ok(Program { id: program_id })
    }

    pub fn id(&self) -> gl::types::GLuint {
        self.id
    }

    pub fn set_used(&self) {
        unsafe {
            gl::UseProgram(self.id);
        }
    }
}

impl Drop for Program {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteProgram(self.id);
        }
    }
}

pub struct Shader {
    id: gl::types::GLuint,
}

impl Shader {
    pub fn from_source(source: &CStr, kind: gl::types::GLenum) -> Result<Shader, String> {
        let id = shader_from_source(source, kind)?;
        Ok(Shader { id })
    }

    pub fn from_vert_source(source: &CStr) -> Result<Shader, String> {
        Shader::from_source(source, gl::VERTEX_SHADER)
    }

    pub fn from_frag_source(source: &CStr) -> Result<Shader, String> {
        Shader::from_source(source, gl::FRAGMENT_SHADER)
    }

    pub fn id(&self) -> gl::types::GLuint {
        self.id
    }
}

impl Drop for Shader {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteShader(self.id);
        }
    }
}

fn shader_from_source(source: &CStr, kind: gl::types::GLenum) -> Result<gl::types::GLuint, String> {
    let id = unsafe { gl::CreateShader(kind) };
    unsafe {
        gl::ShaderSource(id, 1, &source.as_ptr(), std::ptr::null());
        gl::CompileShader(id);
    }

    let mut success: gl::types::GLint = 1;
    unsafe {
        gl::GetShaderiv(id, gl::COMPILE_STATUS, &mut success);
    }

    if success == 0 {
        let mut len: gl::types::GLint = 0;
        unsafe {
            gl::GetShaderiv(id, gl::INFO_LOG_LENGTH, &mut len);
        }

        let error = create_whitespace_cstring_with_len(len as usize);

        unsafe {
            gl::GetShaderInfoLog(
                id,
                len,
                std::ptr::null_mut(),
                error.as_ptr() as *mut gl::types::GLchar,
            );
        }

        return Err(error.to_string_lossy().into_owned());
    }

    Ok(id)
}

fn create_whitespace_cstring_with_len(len: usize) -> CString {
    // allocate buffer of correct size
    let mut buffer: Vec<u8> = Vec::with_capacity(len + 1);
    // fill it with len spaces
    buffer.extend([b' '].iter().cycle().take(len));
    // convert buffer to CString
    unsafe { CString::from_vec_unchecked(buffer) }
}

fn init_resources() -> Program {
    let vert_shader =
        Shader::from_vert_source(&CString::new(include_str!("shader.vert")).unwrap()).unwrap();

    let frag_shader =
        Shader::from_frag_source(&CString::new(include_str!("shader.frag")).unwrap()).unwrap();

    let shader_program = Program::from_shaders(&[vert_shader, frag_shader]).unwrap();

    shader_program
}

const WINDOW_WIDTH: u32 = 900;
const WINDOW_HEIGHT: u32 = 900;
const TRIANGLES_PER_CIRCLE: u64 = 2048;

fn main() {
    let speed_factor = 5.0;
    let objs = vec![
        Object {
            id: 1,
            color: RED,
            radius: 1.0,
            position: Vec2(2.0, 0.0),
            speed: Vec2(0.0, 1.0).times(speed_factor),
        },
        Object {
            id: 2,
            color: BLUE,
            radius: 1.0,
            position: Vec2(-2.0, 0.0),
            speed: Vec2(-1.0, 1.0).times(speed_factor),
        },
        Object {
            id: 3,
            color: GREEN,
            radius: 1.0,
            position: Vec2(0.0, -1.0),
            speed: Vec2(1.0, 1.0).times(speed_factor),
        },
    ];
    let sdl = sdl2::init().unwrap();
    let video_subsystem = sdl.video().unwrap();
    let window = video_subsystem
        .window("Rust", WINDOW_WIDTH, WINDOW_HEIGHT)
        .opengl()
        .build()
        .unwrap();
    let _gl_context = window.gl_create_context().unwrap();
    let _gl =
        gl::load_with(|s| video_subsystem.gl_get_proc_address(s) as *const std::os::raw::c_void);
    let program = init_resources();
    simulate(
        &program,
        &window,
        10.0,
        World {
            objects: objs,
            age: 0.0,
        },
    );
}
