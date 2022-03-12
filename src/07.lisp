(defpackage :sdl2-lazyfoo-07
  (:use :cl)
  (:export :run)
  (:import-from :learn-opengl-utils asset load-image convert-surface))

(in-package :sdl2-lazyfoo-07)

(defconstant *screen-width* 640)
(defconstant *screen-height* 480)

(defun load-texture (image-surface renderer)
  (let ((texture 
	  (sdl2:create-texture-from-surface renderer image-surface)))
    (if (autowrap:wrapper-null-p texture)
	(error "cannot create surface!")
	(sdl2:free-surface image-surface))
    texture))

(defun run ()
  (sdl2:with-init (:video)
    (let* ((window (sdl2:create-window :title "07 tutorial"
				       :w *screen-width*
				       :h *screen-height*
				       :flags '(:shown)))
	   (renderer (sdl2:create-renderer window -1 '(:accelerated)))
	   (texture (load-texture
		     (load-image "07/texture1.png"
				 :loader #'sdl2-image:load-image)
		     renderer)))
      (sdl2-image:init '(:png))
      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
      (sdl2:with-event-loop (:method :poll)
	(:quit () t)
	(:idle ()
	       (sdl2:render-clear renderer)
	       (sdl2:render-copy renderer texture)
	       (sdl2:render-present renderer)))
      (sdl2:destroy-texture texture)
      (sdl2:destroy-renderer renderer)
      (sdl2:destroy-window window))))




