(defpackage :sdl2-opengl-shader
  (:use :cl :opengl-utils)
  (:export :main))

(in-package :sdl2-opengl-shader)

;; vertex array objects
(defparameter *vao* nil)
(defparameter *shader* nil)
(defun declare-shader ()
  ;; create vertex array
  (setf *vao* (make-vertex-array))

  (with-vertex-buffer-layout layout
    (add-to-layout layout :float 2) ;; rectangle vertex positions
    (add-to-layout layout :float 2) ;; texture coordinates
    (add-buffer *vao*
		(make-vertex-buffer #(-0.5 -0.5 0.0 0.0
				      0.5 -0.5 1.0 0.0
				      0.5 0.5 1.0 1.0
				      -0.5 0.5 0.0 1.0))
		layout))
  
  (make-index-buffer
     #(0 1 2 2 3 0))  


  (setf *shader* (make-shader
		  "
#version 330 core
layout(location=0) in vec4 position;
layout(location=1) in vec2 texCoord;
out vec2 v_TexCoord;
void main() {
gl_Position = position;
v_TexCoord = texCoord;
}
" "
#version 330 core
in vec2 v_TexCoord;
layout(location=0) out vec4 color;
uniform sampler2D u_Texture;
void main() {
vec4 texColor = texture(u_Texture, v_TexCoord);
color = texColor;
}
"))
  (gl-bind (make-texture "gold-dollar.png"))
  (gl:uniformi (uniform-loc *shader* "u_Texture") 0)

  (gl-unbind :vertex-array)
  (gl-unbind :vertex-buffer)
  (gl-unbind :shader)
  (gl-unbind :index-buffer)
  )

(define-symbol-macro rnd (random 1.0))


(defun main ()
  (sdl2:with-init (:everything)
    (format t "Using SDL library version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (window :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context window)
        (sdl2:gl-make-current window gl-context)
        (sdl2:gl-set-swap-interval 1)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
        ;; Clear to blue
        (gl:clear-color 0.0 0.0 1.0 1.0)
	
        (declare-shader)
	;; NOTE the declare-shader creates a context,
	;; including buffer and vertex array.
	(let ((red 0.0))
	  (declare (ignore red))
          (sdl2:with-event-loop (:method :poll)
            (:idle ()
		   (progn
		     (gl:clear :color-buffer-bit)
		     (gl-bind *vao*)
		     (gl-bind *shader*)

		     ;; (gl:uniformf
		     ;;  (gl:get-uniform-location
		     ;;   (gl:get-integer :current-program) "u_Color")
		     ;;  red 0.0 0.0 1.0)


		     (%gl:draw-elements :triangles 6 :unsigned-int 0)

		     ;;(gl:draw-arrays :triangles 0 6)
		     (gl:flush)
		     (sdl2:gl-swap-window window)))
            (:quit () t)))))))

(main)


(defun declare-shader1 ()
  ;; create vertex array
  (setf *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*)
  ;; create buffer objects

  (make-vertex-buffer #(-0.5 -0.5
			0.5 -0.5
			0.5 0.5
			-0.5 0.5))
 
  ;; bind the current array buffer to the *vao*
  (gl:vertex-attrib-pointer 0 2 :float :false 0 0)  
  (gl:enable-vertex-attrib-array 0)

  (make-index-buffer
   #(0 1 2 2 3 0))

  
  (let* ((vertex-shader
        "#version 330 core
layout(location=0) in vec4 position;
void main() {
gl_Position = position;
}")
	(fragment-shader
          "#version 330 core
layout(location=0) out vec4 color;
uniform vec4 u_Color;
void main() {
color = u_Color;
}")
	 (program (make-program-from-shader-src vertex-shader fragment-shader)))
    (gl:use-program program)
    ;; NOTE the unbind sequence is very important.
    ;; always unbind vertex array before unbinding the buffers.
    ;; because every buffer-binding operations are inside the vao state.
    ;; so unbinding the index buffer before vao will cause the vao captures
    ;; a null index buffer, causing a segmentation fault when exiting.
    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-buffer :element-array-buffer 0)
    ))
