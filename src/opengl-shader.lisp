(defpackage :sdl2-opengl-shader
  (:use :cl :opengl-utils)
  (:export :main))

(in-package :sdl2-opengl-shader)

(defparameter *quad* nil)
(defparameter *textures* nil)


(defun declare-shader ()

  (push (make-texture "gold-dollar.png") *textures*)
  (let* ((font (sdl2-ttf:open-font #p"C:/Users/niwtr/Downloads/source.ttf" 50))
         (surface (sdl2-ttf:render-utf8-blended font
                                                "你好，世界"
                                                255 ;R
                                                255 ;G
                                                0 ;B
                                                0 ;A
                                                )))
    (push (make-texture surface) *textures*))
  (push 
   (make-texture
    (sdl2-image:load-image #p"C:\\Users\\niwtr\\quicklisp\\local-projects\\learn-opengl\\assets\\container.jpg")
    :target :RGBA)
   *textures*)

  (setf *quad* (make-instance 'basic-quad))
  (set-position *quad* (vector -50.0 -50.0 50.0 -50.0 50.0 50.0 -50.0 50.0))
  (set-texcoord *quad* (vector 0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0))
  (set-texture *quad* (first *textures*))
)

(define-symbol-macro rnd (random 2.0))
(defun sinv nil (* rnd (* 100 (sin (/ (sdl2:get-ticks) 240)))))
(defun cosv nil (* rnd (* 200 (cos (/ (sdl2:get-ticks) 240)))))
(defparameter *g-value-1* 0.0)
(defparameter *g-value-2* 0.0)
(defparameter *g-value-3* 0.0)

(capi:define-interface my-scroll-bar ()
  ()
  (:panes 
   (scroll-bar-1
    capi:scroll-bar
    :title "brightness"
    :start 0
    :end 100
    :callback 'scroll-bar-1-callback)
   (scroll-bar-2
    capi:scroll-bar
    :title "y position"
    :start 0
    :end 600
    :callback 'scroll-bar-2-callback)
   (scroll-bar-3
    capi:scroll-bar
    :title "x position"
    :start 0
    :end 800
    :callback 'scroll-bar-3-callback)
   ))

(defun scroll-bar-1-callback (interface self how where)
  (declare (ignore self how interface))
  (setf *g-value-1* (float (/ where 100))))
(defun scroll-bar-2-callback (interface self how where)
  (declare (ignore self how interface))
  (setf *g-value-2* (float where)))
(defun scroll-bar-3-callback (interface self how where)
  (declare (ignore self how interface))
  (setf *g-value-3* (float where)))

(defun render-main (window)
  (gl:clear :color-buffer-bit)
  (gl-bind *quad*)  
  
  (let* ((proj (3d-matrices:mortho 0 800 0 600 -1 1))
         (eye (3d-matrices:meye 4))
         (trvec (3d-vectors:vec 0 0 0))
         (view (3d-matrices:nmtranslate eye trvec))
         (trans (3d-vectors:vec *g-value-3* *g-value-2* 0))
         (model (3d-matrices:nmtranslate (3d-matrices:meye 4) trans))
         (mvp (3d-matrices:m* (3d-matrices:m* proj view) model)))

    ;; (set-texcoord *quad* (vector 0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0))
    ;; (set-texture *quad* (second *textures*))
    ;; (set-mvp *quad* (3d-matrices:marr mvp))
    ;; (draw *quad*)

    ;;(set-texcoord *quad* (vector 0.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0))
    ;;(set-texture *quad* (second *textures*))
    (loop for i from 0 to 10
          do
            (progn
              (setf trans (3d-vectors:vec (+ *g-value-3* (sinv)) (+ *g-value-2* (cosv)) 0))
              (setf model (3d-matrices:nmtranslate (3d-matrices:meye 4) trans))
              (setf mvp (3d-matrices:m* (3d-matrices:m* proj view) model))

              
              (set-mvp *quad* (3d-matrices:marr mvp))
              (draw *quad*)))
    #|
    (setf trans (3d-vectors:vec (+ *g-value-3* (cosv)) (+ *g-value-2* (sinv)) 0))
    (setf model (3d-matrices:nmtranslate (3d-matrices:meye 4) trans))
    (setf mvp (3d-matrices:m* (3d-matrices:m* proj view) model))
    (set-texture *quad* (first *textures*))
    (set-mvp *quad* (3d-matrices:marr mvp))
    (draw *quad*)
    |#
    
    (gl:flush)
    (sdl2:gl-swap-window window)))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2-ttf:init)
    (sdl2-image:init '(:png :jpg))
    (format t "Using SDL library version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (window :flags '(:shown :opengl))
      ;; on windows there's a bug with sdl window showing.
      ;; see https://github.com/lispgames/cl-sdl2/issues/23.
      (sdl2:hide-window window)
      (sdl2:show-window window)
      (sdl2:with-gl-context (gl-context window)
        (sdl2:gl-make-current window gl-context)
        (sdl2:gl-set-swap-interval 0)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
        ;; Clear to blue
        (gl:clear-color 0.0 0.0 1.0 1.0)
	
        (declare-shader)
        
        ;; (capi:display (make-instance 'my-scroll-bar))
	;; NOTE the declare-shader creates a context,
	;; including buffer and vertex array.
        (format t "Shader compiled, let the hack begin!~%")
	(let ((red 0.0))
	  (declare (ignore red))
          (sdl2:with-event-loop (:method :poll)
            (:idle ()
		   (render-main window))
            (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
			  (declare (ignore xrel yrel state))
			  (setf *g-value-3* x)
			  (setf *g-value-2* (- 600 y))
			  )

            (:quit () t)))))))

