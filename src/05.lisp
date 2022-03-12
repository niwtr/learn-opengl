(defpackage :sdl2-lazyfoo-05
  (:use :cl)
  (:export :run)
  (:import-from :learn-opengl-utils asset load-image convert-surface))

(in-package :sdl2-lazyfoo-05)

(defconstant *screen-width* 640)
(defconstant *screen-height* 480)


(defun run ()
  (sdl2:init :video)
  (let* ((window (sdl2:create-window :title "05 tutorial"
				     :w *screen-width*
				     :h *screen-height*))
	 (surface (sdl2:get-window-surface window))
	 (image (convert-surface
		 (load-image "05/stretch.bmp")
		 (sdl2:surface-format surface)))
	 (rect (sdl2:make-rect 0 0 *screen-width* *screen-height*)))
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:idle ()
	     (sdl2:blit-scaled image nil
			       surface rect)
	     (sdl2:update-window window)))
    (sdl2:free-surface image)
    (sdl2:destroy-window window)
    (sdl2:quit)))


