(defpackage :sdl2-opengl-shader-3d
  (:use :cl :opengl-utils)
  (:export :main))

(in-package :sdl2-opengl-shader-3d)

(defparameter *cube* nil)
(defparameter *textures* nil)


(defparameter *cube-positions* (list 
				(3d-vectors:vec 0.0  0.0  0.0)
				(3d-vectors:vec 2.0  5.0 -15.0)
				(3d-vectors:vec -1.5 -2.2 -2.5)
				(3d-vectors:vec -3.8 -2.0 -12.3)
				(3d-vectors:vec 2.4 -0.4 -3.5)
				(3d-vectors:vec -1.7  3.0 -7.5)
				(3d-vectors:vec 1.3 -2.0 -2.5)
				(3d-vectors:vec 1.5  2.0 -2.5)
				(3d-vectors:vec 1.5  0.2 -1.5)
				(3d-vectors:vec -1.3  1.0 -1.5)))

(defparameter *bullets* nil)

(defclass bullet ()
  (
   (position :initarg :pos :initform (error "no pos provided!") :accessor pos)
   (direction :initarg :dir :initform (error "no dir provided!") :accessor dir)))

(defun declare-shader ()
  (push (make-texture "gold.jpg" :target :RGB) *textures*)

  ;; (let* ((font (sdl2-ttf:open-font #p"C:/Users/niwtr/Downloads/source.ttf" 50))
  ;;        (surface (sdl2-ttf:render-utf8-blended font
  ;;                                               "你好，世界"
  ;;                                               255 ;R
  ;;                                               255 ;G
  ;;                                               0 ;B
  ;;                                               0 ;A
  ;;                                               )))
  ;;   (push (make-texture surface) *textures*))

  (push (make-texture "dirt.jpg" :target :RGB) *textures*)
 
  (setf *cube* (make-instance 'basic-cube))
  (set-texture *cube* (first *textures*))
  )

(defparameter *camera-up* (3d-vectors:vec 0.0 1.0 0.0))
(defparameter *camera-pos* (3d-vectors:vec 0.0 0.0 0.0))
(defparameter *camera-front* (3d-vectors:vec 0.0 0.0 -1.0))
(defparameter *camera-speed* 0.1)
(defparameter *pitch* 0.0)
(defparameter *yaw* 0.0)
(defparameter *bullet-speed* 1.0)



(defun render-main (window)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl-bind *cube*)  
  
  (let* ((proj (3d-matrices:mperspective 45 (/ 800 600) 0.1 100.0))
         ;(eye (3d-matrices:meye 4))
         ;;(trvec (3d-vectors:vec 0.0 0.0 -3.0))
         ;;(view (3d-matrices:nmtranslate eye trvec))

	 (view (3d-matrices:mlookat *camera-pos*
				    (3d-vectors:v+ *camera-pos* *camera-front*)
				    *camera-up*))
	 (model)
	 (mvp))

    (set-texture *cube* (first *textures*))
    (loop for trvec in *cube-positions*
	  for i from 0
	  do
      (progn
	(setf model (3d-matrices:nmtranslate (3d-matrices:meye 4) trvec))
        (setf model (3d-matrices:nmrotate model (3d-vectors:vec 1.0 0.3 0.5) (/ (sdl2:get-ticks) 1000)))
        (setf mvp (3d-matrices:m* (3d-matrices:m* proj view) model))
	(set-mvp *cube* (3d-matrices:marr mvp))
	(draw *cube*)))
    
    (set-texture *cube* (second *textures*))
    (loop for blt in *bullets*
	  do (progn
	       (setf model (3d-matrices:nmtranslate (3d-matrices:meye 4) (pos blt)))
	       (setf model (3d-matrices:nmrotate model (3d-vectors:vec 1.0 0.3 0.5) (/ (sdl2:get-ticks) 100)))
	       (setf mvp (3d-matrices:m* (3d-matrices:m* proj view) model))
	       (set-mvp *cube* (3d-matrices:marr mvp))
	       (draw *cube*)
	       (3d-vectors:nv+ (pos blt) (3d-vectors:v* (dir blt) *bullet-speed*))))

    ;; remove bad bullets.
    (delete-if (lambda (blt) (> (3d-vectors:v2norm (pos blt)) 100)) *bullets*)
    
    (gl:flush)
    (sdl2:gl-swap-window window)))

(defparameter *mouse-sensitiity* 0.5)
(defun d2r (d)
  (* d 0.0174532925))

(defun process-mouse-event (xrel yrel)
  (let* ((xoff xrel)
	 (yoff (- yrel)))
    (setf xoff (* xoff *mouse-sensitiity*))
    (setf yoff (* yoff *mouse-sensitiity*))
    (incf *yaw* xoff)
    (incf *pitch* yoff)
    (when (> *pitch* 89.0)
      (setf *pitch* 89.0))
    (when (< *pitch* -89.0)
      (setf *pitch* -89.0))
    
    (setf *camera-front*
	  (3d-vectors:vunit
	   (3d-vectors:vec
	    (* (cos (d2r *yaw*)) (cos (d2r *pitch*)))
	    (sin (d2r *pitch*))
	    (* (sin (d2r *yaw*)) (cos (d2r *pitch*))))))))

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
      (sdl2:set-relative-mouse-mode :true)
      (sdl2:with-gl-context (gl-context window)
        (sdl2:gl-make-current window gl-context)
        (sdl2:gl-set-swap-interval 1)
	(gl:enable :blend)
        (gl:enable :depth-test)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
        ;; Clear to blue
        (gl:clear-color 0.2 0.3 0.3 1.0)
	
        (declare-shader)
        
        ;;(capi:display (make-instance 'my-scroll-bar))
	;; NOTE the declare-shader creates a context,
	;; including buffer and vertex array.
        (format t "Shader compiled, let the hack begin!~%")
	(let ((red 0.0))
	  (declare (ignore red))
          (sdl2:with-event-loop (:method :poll)
            (:idle ()
		   (render-main window))
            (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
			  ;;(declare (ignore xrel yrel state))
			  (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                                  x xrel y yrel state)
			  (process-mouse-event xrel yrel))
	    (:mousebuttondown (:button key)
			      (push
			       (make-instance 'bullet
					      :pos (3d-vectors:v+ (3d-vectors:vcopy *camera-pos*)
								  (3d-vectors:v* *camera-front* 3))
					      :dir (3d-vectors:vcopy *camera-front*))
			        *bullets*))
	    (:keydown (:keysym keysym)
		      (let ((scancode (sdl2:scancode-value keysym))
			    (sym (sdl2:sym-value keysym))
			    (mod-value (sdl2:mod-value keysym)))
			(format t "Key sym: ~a, code: ~a, mod: ~a~%"
                                sym
                                scancode
                                mod-value)
			(cond
			  ((sdl2:scancode= scancode :scancode-w)
			   (3d-vectors:nv+ *camera-pos* (3d-vectors:v* *camera-front* *camera-speed*)))
			  ((sdl2:scancode= scancode :scancode-s)
			   (3d-vectors:nv- *camera-pos* (3d-vectors:v* *camera-front* *camera-speed*)))
			  ((sdl2:scancode= scancode :scancode-a)
			   (3d-vectors:nv-
			    *camera-pos*
			    (3d-vectors:v*
			     (3d-vectors:vunit (3d-vectors:vc *camera-front* *camera-up*)) *camera-speed*)))
			  ((sdl2:scancode= scancode :scancode-d)
			   (3d-vectors:nv+
			    *camera-pos*
			    (3d-vectors:v*
			     (3d-vectors:vunit (3d-vectors:vc *camera-front* *camera-up*)) *camera-speed*))))))

            (:quit () t)))))))

;;(main)

