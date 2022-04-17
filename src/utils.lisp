(defpackage :learn-opengl-utils
  (:use cl)
  (:export asset load-image symb mkstr flatten)
  )
(in-package :learn-opengl-utils)
(defun asset (pathname)
  "Return an absolute filename for a given PATHNAME relative to
`:sdl2-tutorial' asdf system directory.
This function doesn't prepend 'assets/' to the PATHNAME to not
interfere with your editor filename completion."
  (merge-pathnames pathname
		   (merge-pathnames "assets/" (asdf:system-source-directory :learn-opengl))))

(defun load-image (fname &key (loader #'sdl2:load-bmp))
  (let ((image (funcall loader (asset fname))))
    (if (autowrap:wrapper-null-p image)
	(error "cannot load image ~a." image)
	image)))

(defun convert-surface (surface format)
  (let ((new-surface (sdl2:convert-surface surface format)))
    (sdl2:free-surface surface)
    new-surface))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))

