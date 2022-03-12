(defpackage :sdl2-lazyfoo-04
  (:use :cl)
  (:export :run)
  (:import-from :learn-opengl-utils asset load-image))

(in-package :sdl2-lazyfoo-04)

(defconstant *screen-width* 640)
(defconstant *screen-height* 480)

(defun run ()
  (sdl2:init :video)
  (let* ((window (sdl2:create-window :title "04 tutorial"
				     :w *screen-width*
				     :h *screen-height*))
	 (surface (sdl2:get-window-surface window))
	 (image-names '((:up "04/up.bmp")
			(:down "04/down.bmp")
			(:left "04/left.bmp")
			(:right "04/right.bmp")
			(:press "04/press.bmp")))
	 ;; well I regret using hash table, sorry.
	 (images (make-hash-table :test 'eq))
	 (cur-image nil))
    (loop for (isymb iname) in image-names
	  for image = (load-image iname)
	  do (setf (gethash isymb images) image))
    (setf cur-image (gethash :press images))
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:keydown (:keysym key)
		(setf cur-image (gethash
				 (case (sdl2:scancode key)
				   (:scancode-up :up)
				   (:scancode-down :down)
				   (:scancode-right :right)
				   (:scancode-left :left)
				   (t :press))
				 images)))
      (:idle ()
	     (sdl2:blit-surface cur-image nil surface nil)
	     (sdl2:update-window window)
	     (sdl2:delay 20)))
    
    ;; quit
    (loop for -image being the hash-value in images
	  do (sdl2:free-surface -image))
    (sdl2:destroy-window window)
    (sdl2:quit)
    (princ "quit")))







