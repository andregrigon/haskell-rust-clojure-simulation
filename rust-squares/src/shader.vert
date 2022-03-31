attribute vec3 coord;
attribute vec3 color;
varying vec3 f_color;

void main(void) {
    float s = 0.1f;
    gl_Position = vec4(coord.x * s, coord.y * s, coord.z, 1);
    f_color = color;
}