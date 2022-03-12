
(defpackage :sdl2-opengl
  (:use :cl)
  (:export :run))

(in-package :sdl2-opengl)

(defconstant *screen-width* 640)
(defconstant *screen-height* 480)

(defun debug-log (msg &rest args)
  "Output and flush MSG to STDOUT with arguments ARGS"
  (apply #'format t msg args)
  ;; Flush to standard out
  (finish-output))


(defun init-gl ()
  (gl:enable :depth-test)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:clear-color 0 0 0 1))

(defun render-triangle (theta)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)
  (gl:rotate theta 0.0 0.0 1.0)
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex -0.5 -0.5 -2.0)
  (gl:color 1.0 1.0 0.0)
  (gl:vertex 0.5 -0.5 -2.0)
  (gl:color 0.0 0.0 1.0)
  (gl:vertex 0.0 0.5 -2.0)
  (gl:end)
  (gl:flush))

  


(defun main ()
  (sdl2:with-init (:everything)
    (debug-log "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (window :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context window)
        (let ((theta 0))
          (sdl2:gl-make-current window gl-context)
          (init-gl)
          (sdl2:with-event-loop (:method :poll)
            (:idle ()
             (progn
               (render-triangle theta)
               (incf theta 1)
               (sdl2:gl-swap-window window)
               ))
            (:quit () t)))))))
(main)


