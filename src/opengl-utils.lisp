(defpackage :opengl-utils
  (:use cl)
  (:import-from :learn-opengl-utils asset)
  (:export with-program
	   send-lisp-array-to-buffer-data
	   make-program-from-shader-src
	   with-bind-buffer
	   make-vertex-buffer
	   make-index-buffer
	   gl-bind
	   gl-unbind
	   make-vertex-buffer-layout
	   with-vertex-buffer-layout
	   add-to-layout
	   add-buffer
	   make-vertex-array
	   make-shader
	   uniform-loc
	   make-texture
	   ))

(in-package :opengl-utils)

(defmacro with-program (id &body body)
  `(prog2
       (gl:use-program ,id)
       (progn ,@body)
     (gl:use-program 0)))

(defmacro with-bind-buffer (type &body body)
  `(let ((-buffer (first (gl:gen-buffers 1))))
     (gl:bind-buffer ,type -buffer)
     (progn ,@body)))

(defun send-lisp-array-to-buffer-data (target usage lisp-array &key buffer-id (type :float))
  (when buffer-id ;; if buffer is specified, bind the buffer, otherwise, inject into current context.
    (gl:bind-buffer target buffer-id))

  (let* ((alen (length lisp-array))
	 (glarray (gl:alloc-gl-array type alen)))
    (dotimes (nth alen)
      (setf (gl:glaref glarray nth) (aref lisp-array nth)))

    (gl:buffer-data target usage glarray)
    (gl:free-gl-array glarray)) ;; TODO add unwind protect.

  ;; (when buffer-id
  ;;   (gl:bind-buffer target 0))
  )


(defun make-program-from-shader-src (v-src f-src)
  (let ((v-shader (gl:create-shader :vertex-shader))
	(f-shader (gl:create-shader :fragment-shader))
	(program (gl:create-program)))
    (gl:shader-source v-shader v-src)
    (gl:compile-shader v-shader)
    (gl:shader-source f-shader f-src)
    (gl:compile-shader f-shader)
    (gl:attach-shader program v-shader)
    (gl:attach-shader program f-shader)
    (gl:link-program program)
    (gl:validate-program program)
    (values program v-shader f-shader)))

;;; all gl object with id.
(defgeneric gl-bind (gl-object)
  (:documentation "bind gl object, like vertex buffer, vao, shader, etc."))
(defgeneric gl-unbind (gl-object)
  (:documentation "unbind gl object. "))

(defclass gl-object ()
  (
   (-id :initarg :id :initform 0 :accessor id)))

;;; vertex buffer.
(defclass gl-buffer (gl-object)
  (
   (-type :initarg :type :initform :array-buffer :accessor gl-buffer-type)))

(defclass vertex-buffer (gl-buffer) ())
(defclass index-buffer (gl-buffer) ())

(defmethod make-gl-buffer ((data array) &key (type :array-buffer) (array-type :float))
  (let ((id (first (gl:gen-buffers 1))))
    (gl:bind-buffer type id)
    (send-lisp-array-to-buffer-data type :static-draw data :type array-type)
    (make-instance 'vertex-buffer :type type :id id)))

(defmethod gl-bind ((gl-object gl-buffer))
  (gl:bind-buffer (gl-buffer-type gl-object) (id gl-object)))
(defmethod gl-unbind ((gl-object gl-buffer))
  (gl:bind-buffer (gl-buffer-type gl-object) 0))

(defmacro defunbind (eql-symbol &body body)
  ;; TODO gensym
  `(defmethod gl-unbind ((symb (eql ,eql-symbol)))
     (declare (ignore symb))
     ,@body))

(defunbind :vertex-buffer (gl:bind-buffer :array-buffer 0))
(defunbind :index-buffer (gl:bind-buffer :element-array-buffer 0))


(defmethod make-index-buffer ((data array))
  (make-gl-buffer data :type :element-array-buffer :array-type :unsigned-int))
(defmethod make-vertex-buffer ((data array))
  (make-gl-buffer data :type :array-buffer))


;;; vertex buffer layout abstraction.
;; just a struct.
(defclass vertex-buffer-element ()
  (
   (-type :initarg :type :accessor vertex-buffer-element-type)
   (-count :initarg :count :accessor vertex-buffer-element-count)
   (-normalized :initarg :normalized :initform :false :accessor vertex-buffer-element-normalized)))

(defclass vertex-buffer-layout (gl-object)
  (
   (-stride :initarg :stride :initform 0 :accessor stride)
   (-elements :initarg :layouts :initform nil :accessor elements)))

(defmethod make-vertex-buffer-layout ()
  (make-instance 'vertex-buffer-layout))

(defmacro with-vertex-buffer-layout (name &body body)
  `(let ((,name (make-vertex-buffer-layout)))
     ,@body))

(defmethod add-to-layout ((layout vertex-buffer-layout)
			(type symbol)
			(count integer)
			&key (normalized :false))
  (incf (stride layout) (* count (cffi:foreign-type-size type)))
  (push (make-instance 'vertex-buffer-element
		       :type type
		       :count count
		       :normalized normalized)
	(elements layout)))


;;; vertex array
(defclass vertex-array (gl-object) ())

(defmethod gl-bind ((vao vertex-array))
  (gl:bind-vertex-array (id vao)))
(defmethod gl-unbind ((vao vertex-array))
  (declare (ignore vao))
  (gl:bind-vertex-array 0))
(defunbind :vertex-array (gl:bind-vertex-array 0))

(defmethod add-buffer ((target-vao vertex-array)
		       (vb vertex-buffer)
		       (vl vertex-buffer-layout))
  (gl-bind target-vao)
  (gl-bind vb)
  (loop with offset = 0
	for nth from 0
	for element in (elements vl) do
	  (progn
	    (gl:enable-vertex-attrib-array nth)
	    (gl:vertex-attrib-pointer
	     nth
	     (vertex-buffer-element-count element)
	     (vertex-buffer-element-type element)
	     (vertex-buffer-element-normalized element)
	     (stride vl)
	     offset)
	    (incf offset (* (vertex-buffer-element-count element)
			    (cffi:foreign-type-size (vertex-buffer-element-type element)))))))

(defmethod make-vertex-array ()
  (make-instance 'vertex-array :id (gl:gen-vertex-array)))


;;; shader
(defclass shader (gl-object)
  (
   (-vertex-shader-source :initarg :vertex-src :accessor vertex-src)
   (-fragment-shader-source :initarg :fragment-src :accessor fragment-src)))

(defmethod gl-bind ((s shader))
  (gl:use-program (id s)))

(defmethod gl-unbind ((s shader))
  (declare (ignore s))
  (gl:use-program 0))
(defunbind :shader (gl:use-program 0))

(defmethod make-shader (v-src f-src)
  (let* ((v-shader (gl:create-shader :vertex-shader))
	 (f-shader (gl:create-shader :fragment-shader))
	 (program (gl:create-program))
	 (obj (make-instance 'shader :id program :vertex-src v-src :fragment-src f-src)))
    (gl:shader-source v-shader v-src)
    (gl:compile-shader v-shader)
    (gl:shader-source f-shader f-src)
    (gl:compile-shader f-shader)
    (gl:attach-shader program v-shader)
    (gl:attach-shader program f-shader)
    (gl:link-program program)
    (gl:validate-program program)
    (gl:delete-shader v-shader)
    (gl:delete-shader f-shader)
    ;; finally, bind the shader.
    (gl:use-program program)
    obj))

(defmethod uniform-loc ((s shader) (target string))
  (gl:get-uniform-location (id s) target))

(defclass texture (gl-object)
  (
   (filepath :initarg :path :initform nil :type pathname :accessor path)
   (local-buffer :initarg :local-buffer :initform nil :accessor local-buffer)
   (width :initarg :width :initform 0 :accessor width)
   (height :initarg :height :initform 0 :accessor height)
   (bits-per-pixel :initarg :bpp :initform 0 :accessor bpp)))

(defmethod gl-bind ((tex texture))
  (gl:active-texture :texture0) ;; TODO add more options.
  (gl:bind-texture :texture-2d (id tex)))
(defmethod gl-unbind ((tex texture))
  (declare (ignore tex))
  (gl:bind-texture :texture-2d 0))
(defunbind :texture (gl:bind-texture :texture-2d 0))


(defmethod make-texture (path)
  (pngload:with-png-in-static-vector (png (asset (pathname path)) :decode t :flip-y t)
    (let* ((data-pointer (static-vectors:static-vector-pointer (pngload:data png)))
	   (width (pngload:width png))
	   (height (pngload:height png))
	   (bpp (pngload:bit-depth png))
	   (tex-id (gl:gen-texture))
	   (obj (make-instance 'texture
			       :id tex-id
			       :path path
			       :local-buffer png
			       :width width
			       :height height
			       :bpp bpp)))
      (gl:bind-texture :texture-2d tex-id)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:tex-image-2d :texture-2d 0 :RGBA8 width height 0 :RGBA :unsigned-byte data-pointer)
      (gl-unbind obj)
      obj)))

