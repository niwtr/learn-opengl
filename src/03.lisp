(defpackage :sdl2-lazyfoo-03
  (:use :cl)
  (:export :run)
  (:import-from :learn-opengl-utils asset))

(in-package :sdl2-lazyfoo-03)

(defconstant *screen-width* 640)
(defconstant *screen-height* 480)

(defun load-image (fname)
  (let ((image (sdl2:load-bmp (asset fname))))
    (if (autowrap:wrapper-null-p image)
	(error "cannot load image ~a." image)
	image)))

(defun run ()
  (sdl2:init :video)
  (let* ((window (sdl2:create-window :title "03 tutorial"
				     :w *screen-width*
				     :h *screen-height*))
	 (surface (sdl2:get-window-surface window))
	 (image (load-image "03/x.bmp")))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
	     (print "quit pressed!"))
      (:idle ()
	     (sdl2:blit-surface image nil surface nil)
	     (sdl2:update-window window)
	     (sdl2:delay 20)))
    (sdl2:free-surface image)
    (sdl2:destroy-window window)
    (sdl2:quit)))
