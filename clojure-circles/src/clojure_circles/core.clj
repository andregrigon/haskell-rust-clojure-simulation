(ns clojure-circles.core
  (:require [clojure.math.numeric-tower :as math :refer [expt sqrt]])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.opengl GL GL11 GL20)
           (org.lwjgl.glfw GLFW))
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is based on the Haskell version,
;; see that for documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defrecord Interval [^double interval])

(defrecord Instant [^double instant])

(defn <d
  [^double a, ^double b]
  (< a b))

(defn >d
  [^double a, ^double b]
  (> a b))

(defn >=d
  [^double a, ^double b]
  (>= a b))

(defn +d ^double
  [^double a, ^double b]
  (+ a b))

(defn *d ^double
  [^double a, ^double b]
  (* a b))

(defn minusd ^double
  [^double a, ^double b]
  (- a b))

(defn divd ^double
  [^double a, ^double b]
  (/ a b))

(defn quoti
  [^long a, ^long b]
  (quot a b))

(defn modi
  [^long a, ^long b]
  (mod a b))

(defn +i
  [^long a, ^long b]
  (+ a b))

(defn *i
  [^long a, ^long b]
  (* a b))

(defn current-time
  []
  (Instant. (/ (System/currentTimeMillis) 1000.0)))

(defrecord Vec2 [^double x, ^double y])

(defn plus ^Vec2
  [{^double x1 :x, ^double y1 :y} {^double x2 :x, ^double y2 :y}]
  (Vec2. (+ x1 x2) (+ y1 y2)))

(defn times ^Vec2
  [^double k, {^double x :x, ^double y :y}]
  (Vec2. (* k x) (* k y)))

(defn dot ^double
  [{^double x1 :x, ^double y1 :y} {^double x2 :x, ^double y2 :y}]
  (+ ^double (* x1 x2) ^double (* y1 y2)))

(defn distance-squared ^double
  [{^double x1 :x, ^double y1 :y} {^double x2 :x, ^double y2 :y}]
  (+ ^double (expt (- x1 x2) 2) ^double (expt (- y1 y2) 2)))

(def ZERO (Vec2. 0 0))

(defn vec-length ^double
  [^Vec2 v]
  (sqrt (distance-squared v ZERO)))

(defn normalize ^Vec2
  [^Vec2 v]
  (let [vl (vec-length v)
        vl' (if (<d vl 0.01) 0.01 vl)]
    (times (/ 1.0 vl') v)))

(defn reflect
  [^Vec2 d, ^Vec2 n]
  (let [n' (normalize n)]
    (plus d (times (* -2.0 (dot d n')) n'))))

(defrecord Color [^Float r, ^Float g, ^Float b])

(defrecord Obj [^Integer id, ^double radius, ^Vec2 position, ^Vec2 speed, ^Color color])

(defrecord Bound [^Integer id, ^double radius, ^Vec2 position])

(defrecord Collision [^Vec2 dir])

(def ^Double WALL 10.0)
(def ^Double -WALL -10.0)

(defn outside-walls
  [{{x :x, y :y} :position}]
  (or (>d (math/abs x) WALL) (>d ^double (math/abs y) WALL)))

(defn evolve-object [^Collision col, ^Interval {dt :interval}, ^Obj old]
  (let [split-factor 0.7
        min-split-radius 0.1
        growth-per-sec 0.02
        max-growth-radius 1.0
        speed' (if col (reflect (:speed old) (:dir col)) (:speed old))
        collided (not (nil? col))
        boost (if collided 3.0 1.0)
        position' (plus (:position old) (times (* ^double dt ^double boost) speed'))
        radius' (if (<d (:radius old) max-growth-radius)
                  (+ ^double (:radius old) ^double (* ^double growth-per-sec ^double dt))
                  (:radius old))]
    (cond
      (outside-walls old)
      []
      (and collided (>d (:radius old) min-split-radius))
      [(assoc old :speed speed' :position position' :radius (* ^double (:radius old) ^double split-factor))
       (assoc old :speed (times -1.0 speed') :position position' :radius (* ^double (:radius old) ^double split-factor))]
      :else
      [(assoc old :speed speed' :position position' :radius radius')])))

(defn bound-object
  [{id :id, radius :radius, position :position}]
  (Bound. id radius position))

(defn generate-vec
  [f, ^long size]
  (loop [i 0
         b (BufferUtils/createFloatBuffer size)]
    (if (< ^long i size)
      (recur (inc i) (.put b ^Float (f i)))
      (.flip b))))

(def ^Long TRIANGLES-PER-CIRCLE 32)

(defn circle
  [{^double r :r, ^double g :g, ^double b :b} ^double rad {^double x :x, ^double y :y}]
  (let [vertex-count (*i TRIANGLES-PER-CIRCLE 3)
        coord-count (*i vertex-count 3)
        theta (divd (*d 2.0 Math/PI) TRIANGLES-PER-CIRCLE)]
    (letfn [(vertex [^Integer i]
              (let [n (quoti i 9)]
                (case (int (modi i 9))
                  0 (+d (*d rad (Math/cos (*d theta n))) x)
                  1 (+d (*d rad (Math/sin (*d theta n))) y)
                  2 0
                  3 (+d (*d rad (Math/cos (*d theta (+i n 1)))) x)
                  4 (+d (*d rad (Math/sin (*d theta (+i n 1)))) y)
                  5 0
                  6 x
                  7 y
                  8 0)))
            (color [i]
              (case (int (modi i 3))
                0 r
                1 g
                2 b))]
      [(generate-vec vertex coord-count)
       (generate-vec color coord-count)])))

(defn draw-object
  [{radius :radius, position :position, color :color}]
  (circle color radius position))

(defrecord World [objects age])

(defn find-first
  ([pred coll]
   (reduce (fn [_ x]
             (if (pred x)
               (reduced x)
               nil))
           nil coll)))

(defn detect-collisions
  [a bs]
  (letfn [(collides-with [{id1 :id, r1 :radius, p1 :position}
                          {id2 :id, r2 :radius, p2 :position}]
            (and (not= id1 id2)
                 (<d (distance-squared p1 p2) (expt (+d r1 r2) 2))))
          (mk-collision [{p1 :position}, {p2 :position}]
            (Collision. (plus p2 (times -1.0 p1))))]
    ;; Use find-first instead of (first (filter ...)) to avoid overcomputation
    ;; due to chunking
    (let [colliding (find-first #(collides-with a %) bs)]
      (if colliding
        (mk-collision a colliding)
        nil))))

(defn evolve-world
  [dt world]
  (let [bounds (map bound-object (:objects world))]
    (letfn [(collisions [a]
              (let [bound (bound-object a)
                    {{x :x, y :y} :position} bound
                    top (Bound. -1 0.0 (Vec2. x WALL))
                    bottom (Bound. -2 0.0 (Vec2. x -WALL))
                    left (Bound. -3 0.0 (Vec2. -WALL y))
                    right (Bound. -4 0.0 (Vec2. WALL y))
                    walls [top, bottom, left, right]]
                (detect-collisions bound (concat walls bounds))))]
      (assoc world
             :objects (mapcat #(evolve-object (collisions %) dt %) (:objects world))
             :age (+d (:age world) (:interval dt))))))

(def WINDOW-WIDTH 900)
(def WINDOW-HEIGHT 900)

(defn draw
  [p w vertices-colors]
  (GL11/glClearColor 0.7 0.7 0.7 1.0)
  (GL11/glViewport 0 0 WINDOW-WIDTH WINDOW-HEIGHT)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (GL20/glUseProgram p)

  (doseq [[^java.nio.FloatBuffer vertices-buffer, ^java.nio.FloatBuffer colors-buffer] vertices-colors]
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 vertices-buffer)
    (GL20/glEnableVertexAttribArray 1)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false 0 colors-buffer)
    (GL11/glDrawArrays GL11/GL_TRIANGLE_FAN 0 (/ (.remaining vertices-buffer) 3))
    (GL20/glDisableVertexAttribArray 0)
    (GL20/glDisableVertexAttribArray 1)))

(defn draw-world
  [program window world]
  (let [drawings (map draw-object (:objects world))]
    (draw program window drawings)
    (GLFW/glfwSwapBuffers window)))

(def RED (Color. 0.6 0.0 0.0))
(def GREEN (Color. 0.0 0.6 0.0))
(def BLUE (Color. 0.0 0.0 0.6))

(defn print-double
  [d]
  (format "%.2f" d))

(defn print-world
  [{objects :objects, age :age}]
  (str (print-double age) "s: " (count objects) " objects"))

(defn simulate
  [program window duration initial-world]
  (let [start-time (current-time)
        fps-window 0.25]
    (loop [[fps-interval, fps-count] [0.0, 0.0]
           total 0.0
           instant start-time
           world initial-world]
      (draw-world program window world)
      (let [i (:instant instant)
            instant' (current-time)
            i' (:instant instant')
            dt (minusd i' i)
            world' (evolve-world (Interval. dt) world)
            total' (+d total dt)
            fps (if (>=d fps-interval fps-window)
                  (let [fps (divd fps-count fps-interval)]
                    (println
                     (str (if (<d fps 55.0) "!" "")
                          (print-double fps) "fps | "
                          (print-world world)))
                    [0.0, 0.0])
                  [(+d fps-interval dt), (+i fps-count 1)])]
        (if (>=d total' duration)
          nil
          (recur fps total' instant' world'))))))

(def vs-shader
  (str "attribute vec3 coord;\n",
       "attribute vec3 color;\n",
       "varying vec3 f_color;\n",
       "\n",
       "void main(void) {\n",
       " float s = 0.1f;\n",
       " gl_Position = vec4(coord.x * s, coord.y * s, coord.z, 1);\n",
       " f_color = color;\n",
       "}\n"))

(def fs-shader
  (str "varying vec3 f_color;\n",
       "\n",
       "void main(void) {\n",
       " gl_FragColor = vec4(f_color, 1);\n",
       "}\n"))

(defn load-shader
  [shader-str shader-type]
  (let [shader-id (GL20/glCreateShader shader-type)
        _ (GL20/glShaderSource shader-id ^String shader-str)
        ;;_ (println "init-shaders glShaderSource errors?" (GL11/glGetError))
        _ (GL20/glCompileShader shader-id)
        ;;_ (println "init-shaders glCompileShader errors?" (GL11/glGetError))
        ]
    shader-id))

(defn init-resources
  []
  (let [_ (print "OpenGL version:" (GL11/glGetString GL11/GL_VERSION))
        _ (println ", renderer:" (GL11/glGetString GL11/GL_RENDERER))
        vs-id (load-shader vs-shader GL20/GL_VERTEX_SHADER)
        fs-id (load-shader fs-shader GL20/GL_FRAGMENT_SHADER)
        p-id (GL20/glCreateProgram)
        _ (GL20/glAttachShader p-id vs-id)
        _ (GL20/glAttachShader p-id fs-id)
        _ (GL20/glLinkProgram p-id)]
    p-id))

(def SPEED-FACTOR 5.0)
(def OBJS [(Obj. 1 1 (Vec2. 2.0 0.0) (times SPEED-FACTOR (Vec2. 0.0 1.0)) RED)
           (Obj. 2 1 (Vec2. -2.0 0.0) (times SPEED-FACTOR (Vec2. -1.0 1.0)) GREEN)
           (Obj. 3 1 (Vec2. 0.0 -1.0) (times SPEED-FACTOR (Vec2. 1.0 1.0)) BLUE)])

(defn -main
  [& args]
  (GLFW/glfwInit)
  (GLFW/glfwDefaultWindowHints)
  (let [window (GLFW/glfwCreateWindow ^Integer WINDOW-WIDTH ^Integer WINDOW-HEIGHT "clojure" 0 0)]
    (try
      (GLFW/glfwMakeContextCurrent window)
      ; (GLFW/glfwSwapInterval 1)
      (GLFW/glfwShowWindow window)
      (GL/createCapabilities)
      (let [program (init-resources)]
        (simulate program window 10 (World. OBJS 0.0)))
      (catch Throwable e
        (prn e))
      (finally
        (GLFW/glfwDestroyWindow window)
        (GLFW/glfwTerminate)))))