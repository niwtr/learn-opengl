(defpackage :sdl2-lazyfoo-06
  (:use :cl)
  (:export :run)
  (:import-from :learn-opengl-utils asset load-image convert-surface))

(in-package :sdl2-lazyfoo-06)

(defconstant *screen-width* 640)
(defconstant *screen-height* 480)


(defun run ()
  (sdl2:init :video)
  (sdl2-image:init '(:png))
  (let* ((window (sdl2:create-window :title "06 tutorial"
				     :w *screen-width*
				     :h *screen-height*))
	 (surface (sdl2:get-window-surface window))
	 (image (convert-surface
		 (load-image "06/loaded.png"
			     :loader #'sdl2-image:load-image)
		 (sdl2:surface-format surface))))
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:idle ()
	     (sdl2:blit-surface image nil
				surface nil)
	     (sdl2:update-window window)))
    (sdl2:free-surface image)
    (sdl2:destroy-window window)
    (sdl2:quit)))


