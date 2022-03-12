(defpackage :sdl2-lazyfoo-01
  (:use :cl)
  (:export :run))


(defconstant *screen-width* 800)
(defconstant *screen-height* 600)

(defun run ()
  (sdl2:init :video)
  (let* ((window
           (sdl2:create-window  :title "learn sdl2"
				:x 0
				:y 0
				:w *screen-width*
				:h *screen-height*))
	 (surface (sdl2:get-window-surface window)))
    
    (sdl2:fill-rect surface nil (sdl2:map-rgb (sdl2:surface-format surface)
					      #xFF
					      #xFF
					      #xFF))
    (sdl2:update-window window)
    (sdl2:delay 2000)

    (sdl2:destroy-window window)
    (sdl2:quit)
    )
  (print "done!"))
