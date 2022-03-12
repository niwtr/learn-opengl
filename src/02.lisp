(defpackage :sdl2-lazyfoo-02
  (:use :cl)
  (:export :run)
  (:import-from :learn-opengl-utils :asset))

(in-package :sdl2-lazyfoo-02)

(defconstant *screen-width* 640)
(defconstant *screen-height* 480)


(defun load-image (fname)
  (let ((image (sdl2:load-bmp (asset fname))))
    (if (autowrap:wrapper-null-p image)
	(error "cannot load image ~a." image)
	image)))

(defun run ()
  (print "init")
  (sdl2:init :video)
  (let* ((window (sdl2:create-window :title "tutorial 02" :w *screen-width* :h *screen-height*))
	 (surface (sdl2:get-window-surface window))
	 (image (load-image "02/hello_world.bmp")))
    (sdl2:blit-surface image nil surface nil)
    (sdl2:update-window window)
    (sdl2:delay 2000)
    (sdl2:free-surface image)
    (sdl2:destroy-window window)
    (sdl2:quit)
    (print "quit")))


