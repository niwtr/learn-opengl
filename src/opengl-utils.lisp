(defpackage :opengl-utils
  (:use cl)
  (:import-from :learn-opengl-utils asset)
  (:export with-program
	   send-lisp-array-to-buffer-data
	   make-program-from-shader-src
	   with-bind-buffer
	   make-vertex-buffer
	   make-index-buffer
           buffer-data
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
           make-texture-from-ttf-surface
	   active-texture
	   
           ;; new apis
	   basic-object
	   basic-quad
           set-color
           set-position
           set-texcoord
           set-texture
           set-mvp
           draw
	   set-buffer
           basic-cube

	   ;; new accessors
	   shader
	   vao
	   position-buffer

	   
	   ))

(in-package :opengl-utils)

(defun allocate-or-assign-gl-array (lisp-array gl-array &key
                                                 (type :float)
                                                 (verbose nil))
  (let* ((alen (length lisp-array)))
    (when (and verbose gl-array)
      (format t "allocate-or-assign-gl-array: reusing old gl-array~%"))
    (unless gl-array
      (and verbose (format t "allocate-or-assign-gl-array: allocating new gl-array~%"))
      (setf gl-array (gl:alloc-gl-array type (length lisp-array))))
    (assert (= alen (gl::gl-array-size gl-array)))
    (dotimes (nth alen)
      (setf (gl:glaref gl-array nth) (aref lisp-array nth)))
      gl-array))

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
   (-type :initarg :type :initform :array-buffer :accessor gl-buffer-type)
   (-data :initarg :gl-array-pointer :initform nil :accessor gl-array-pointer)))

(defclass vertex-buffer (gl-buffer) ())
(defclass index-buffer (gl-buffer) ())

(defmethod buffer-data ((buffer gl-buffer) (data array)
                        &key (type :float)
                        (target :array-buffer)
                        (usage :static-draw)
                        (free-gl-array nil))
  (gl-bind buffer)
  ;; assign data. alloc data and store pointer if needed.
  (setf (gl-array-pointer buffer)
        (allocate-or-assign-gl-array data (gl-array-pointer buffer) :type type))
  ;; feed gl-array data to gpu.
  (gl:buffer-data target usage (gl-array-pointer buffer))
  (when free-gl-array
    (gl:free-gl-array (gl-array-pointer buffer))
    (setf (gl-array-pointer buffer) nil))
  (gl-array-pointer buffer))

(defmethod make-gl-buffer ((data array)
                           &key (data-target :array-buffer) ;
                           (data-type :float)
                           (data-usage :static-draw)
                           (free-gl-array nil))
  (let ((id (first (gl:gen-buffers 1)))
        (buffer nil))
    (gl:bind-buffer data-target id)
    (setf buffer (make-instance 'vertex-buffer :type data-target :id id :gl-array-pointer nil))
    (buffer-data buffer data :type data-type :target data-target :usage data-usage :free-gl-array free-gl-array)
    buffer))

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
  (make-gl-buffer data :data-target :element-array-buffer :data-type :unsigned-int))
(defmethod make-vertex-buffer ((data array))
  (make-gl-buffer data :data-target :array-buffer))


;;; vertex buffer layout abstraction.
;; just a struct.
(defclass vertex-buffer-element ()
  (
   ;; attribute name defined in your shader source.
   (-name :type string
          :initform (error "Please specify a name for this layout.")
          :initarg :name :accessor name)
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
                          (name string)
                          (type symbol)
                          (count integer)
                          &key (normalized :false))
  (incf (stride layout) (* count (cffi:foreign-type-size type)))
  (push (make-instance 'vertex-buffer-element
                       :name name
		       :type type
		       :count count
		       :normalized normalized)
	(elements layout)))

;;; vertex array
(defclass vertex-array (gl-object) 
  (
   (vertex-buffers :initform nil :accessor vertex-buffers)
   (vertex-buffer-layouts :initform nil :accessor vertex-buffer-layouts)
   (layout-next-index :initform 0 :accessor layout-next-index)))

(defmethod gl-bind ((vao vertex-array))
  (gl:bind-vertex-array (id vao)))
(defmethod gl-unbind ((vao vertex-array))
  (declare (ignore vao))
  (gl:bind-vertex-array 0))
(defunbind :vertex-array (gl:bind-vertex-array 0))

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
(defmethod attrib-loc ((s shader) (target string))
  (gl:get-attrib-location (id s) target))

(defmethod set-buffer ((target-shader shader)
                       (target-vao vertex-array)
		       (vb vertex-buffer)
		       (vl vertex-buffer-layout))
  (gl-bind target-vao)
  (gl-bind vb)
  ;; generate vertex-attrib-pointer.
  (loop with offset = 0
	;;for nth from (layout-next-index target-vao)
	for element in (reverse (elements vl))
        for index = (attrib-loc target-shader (name element))
          if (= index -1)
          do (error (format nil "Attribute name ~A is not defined in shader, or it's optimized out." (name element)))
          else do
	  (progn
            (format t "attribute ~A loc: ~A offset: ~A~%" (name element) index offset)
	    (gl:enable-vertex-attrib-array index)
	    (gl:vertex-attrib-pointer
             index
	     (vertex-buffer-element-count element)
	     (vertex-buffer-element-type element)
	     (vertex-buffer-element-normalized element)
	     (stride vl)
	     offset)
            (incf (layout-next-index target-vao))
	    (incf offset (* (vertex-buffer-element-count element)
			    (cffi:foreign-type-size (vertex-buffer-element-type element))))))
  ;; register buffer id and layout information to vao.
  (push vb (vertex-buffers target-vao))
  (push vl (vertex-buffer-layouts target-vao)))

(defclass texture (gl-object)
  (
   (filepath :initarg :path :initform nil :type pathname :accessor path)
   (local-buffer :initarg :local-buffer :initform nil :accessor local-buffer)
   (width :initarg :width :initform 0 :accessor width)
   (height :initarg :height :initform 0 :accessor height)
   (bits-per-pixel :initarg :bpp :initform 0 :accessor bpp)))

(defparameter *texture-keywords*
  #(
    :texture0
    :texture1
    :texture2
    :texture3
    :texture4
    :texture5
    :texture6
    :texture7
    :texture8
    :texture9
    :texture10
    :texture11
    :texture12
    :texture13
    :texture14
    :texture15
    ))


(defun get-texture-keyword (n)
  (aref *texture-keywords* n))

(defmethod active-texture (n)
  (gl:active-texture (get-texture-keyword n)))

(defmethod gl-bind ((tex texture))
  (gl:bind-texture :texture-2d (id tex)))
(defmethod gl-unbind ((tex texture))
  (declare (ignore tex))
  (gl:bind-texture :texture-2d 0))
(defunbind :texture (gl:bind-texture :texture-2d 0))

(defmethod make-texture-pngload ((path string) &key (target :RGBA))
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
      ;; the second param is `level`, the level-of-detail number.
      (gl:tex-image-2d :texture-2d 0 :RGBA8 width height 0 target :unsigned-byte data-pointer)
      (gl-unbind obj)
      obj)))

(defmethod make-texture ((path string) &key (target :RGBA))
  (let ((surface (sdl2-image::load-image (asset (pathname path)))))
    (make-texture surface :target target)))

;;; 以SDL surface为参数，用于font rendering。
(defmethod make-texture ((surface sdl2-ffi:sdl-surface) &key (target :BGRA))
  (let* ((tex-id (gl:gen-texture))
         (width (sdl2:surface-width surface) ;;(/ (sdl2:surface-pitch surface) 4)
	   )
         (height (sdl2:surface-height surface))
         (ptr (sdl2:surface-pixels surface))
         (obj (make-instance 'texture
                             :id tex-id
                             :path nil
                             :local-buffer ptr
                             :width width
                             :height height
                             :bpp -1)))
    (gl:bind-texture :texture-2d tex-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    ;; the second param is `level`, the level-of-detail number.
    (gl:tex-image-2d :texture-2d 0 :RGBA width height 0 target :unsigned-byte ptr)
;;;     (print width)
;;;     (print (sdl2:surface-pitch surface))
;;;     (print (sdl2:surface-height surface))
;;;     (print (sdl2:surface-width surface))
    (gl-unbind obj)
    obj))

(defmethod make-texture-from-ttf ((surface sdl2-ffi:sdl-surface) &key (target :BGRA))
  (let* ((tex-id (gl:gen-texture))
         (width (/ (sdl2:surface-pitch surface) 4))
         (height (sdl2:surface-height surface))
         (ptr (sdl2:surface-pixels surface))
         (obj (make-instance 'texture
                             :id tex-id
                             :path nil
                             :local-buffer ptr
                             :width width
                             :height height
                             :bpp -1)))
    (gl:bind-texture :texture-2d tex-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    ;; the second param is `level`, the level-of-detail number.
    (gl:tex-image-2d :texture-2d 0 :RGBA width height 0 target :unsigned-byte ptr)
;;;     (print width)
;;;     (print (sdl2:surface-pitch surface))
;;;     (print (sdl2:surface-height surface))
;;;     (print (sdl2:surface-width surface))
    (gl-unbind obj)
    obj))

;; TODOs:
;; 1. make a normal quad controller.

;; basic render controller object, a skeleton, universal object.
(defclass basic-object ()
  (
   (-shader :type shader :initform nil :initarg :shader :accessor shader)
   (-vao :type vertex-array :initform (make-vertex-array) :initarg :vao :accessor vao)
   (-texture :type list ;; list of all textures.
             :initform nil :initarg :texture :accessor texture)
   (-position-buffer :type vertex-buffer :initform nil :accessor position-buffer)
   (-color-buffer :type vertex-buffer :initform nil :accessor color-buffer)
   (-texcoord-buffer :type vertex-buffer :initform nil :accessor texcoord-buffer)
   (-default-index-buffer-data :type array :initform nil :accessor default-index-buffer-data)
   (-index-buffer :type index-buffer :initform nil :accessor index-buffer)
;;; todo add mouse events.
   ))

(defmethod set-shader ((object basic-object)
		       (shader shader))
  (setf (shader object) shader))
;; todo refactor, symbolize.
(defmethod set-color ((object basic-object)
                      (color-array array))
  (buffer-data (color-buffer object) color-array))

(defmethod set-position ((object basic-object)
                         (position-array array))
  (buffer-data (position-buffer object) position-array))

(defmethod set-texcoord ((object basic-object)
                         (texcoord-array array))
  (buffer-data (texcoord-buffer object) texcoord-array))

(defmethod set-texture ((object basic-object)
                        (tex texture))
  (setf (texture object) tex)
  ;; 目前只支持一个材质绑定一个object，所以就这样写吧。
  (active-texture 0)
  (gl-bind tex)
  (gl:uniformi (uniform-loc (shader object) "u_Texture") 0))

(defmethod set-index-buffer-data ((object basic-object)
				  &optional (data nil))
  (gl-bind object)
  (let ((data (or data (default-index-buffer-data object))))
    (if data (setf (index-buffer object) (make-index-buffer data))
	(error "no data provided!"))))

;; currently, mvp is not bound to a object object.
(defmethod set-mvp ((object basic-object)
                    mvp)
  (gl:uniform-matrix-4fv (uniform-loc (shader object) "u_MVP")
                         mvp))

(defmethod gl-bind ((object basic-object))
  (gl-bind (shader object))
  (gl-bind (vao object)))


(defclass basic-quad (basic-object)
  (
   (-shader :initform
            (make-shader
		  "
#version 330 core
in vec4 position;
in vec3 color;
in vec2 texCoord;

out vec4 v_Color;
out vec2 v_TexCoord;

uniform mat4 u_MVP;
void main() {
gl_Position = u_MVP * position;
v_TexCoord = texCoord;
v_Color = vec4(color, 1.0);
}
" "
#version 330 core
in vec4 v_Color;
in vec2 v_TexCoord;
layout(location=0) out vec4 color;
uniform sampler2D u_Texture;

void main() {
color = texture(u_Texture, v_TexCoord) * v_Color;
}
"))
   (-position-buffer :initform (make-vertex-buffer (make-array 8 :initial-element 0.0)))
   (-color-buffer :initform (make-vertex-buffer (make-array 12 :initial-element 1.0)))
   (-texcoord-buffer :initform (make-vertex-buffer (make-array 8 :initial-element 0.0)))
   (-default-index-buffer-data :initform #(0 1 2 2 3 0))
))


(defmethod initialize-instance :after ((quad basic-quad) &key)
  (gl-bind quad)
  (with-vertex-buffer-layout layout 
    (add-to-layout layout "position" :float 2)
    (set-buffer (shader quad) (vao quad) (position-buffer quad) layout))
  (with-vertex-buffer-layout layout 
    (add-to-layout layout "color" :float 3)
    (set-buffer (shader quad) (vao quad) (color-buffer quad) layout))
  (with-vertex-buffer-layout layout 
    (add-to-layout layout "texCoord" :float 2)
    (set-buffer (shader quad) (vao quad) (texcoord-buffer quad) layout))
  (set-index-buffer-data quad))

(defmethod draw ((quad basic-quad))
  (gl-bind quad)
  (%gl:draw-elements :triangles 6 :unsigned-int 0))


(defclass basic-cube (opengl-utils:basic-object)
  (
   (-shader :initform
            (make-shader
		  "
#version 330 core
in vec4 position;
in vec3 color;
in vec2 texCoord;

out vec4 v_Color;
out vec2 v_TexCoord;

uniform mat4 u_MVP;
void main() {
gl_Position = u_MVP * position;
v_TexCoord = texCoord;
v_Color = vec4(color, 1.0);
}
" "
#version 330 core
in vec4 v_Color;
in vec2 v_TexCoord;
layout(location=0) out vec4 color;
uniform sampler2D u_Texture;

void main() {
color = texture(u_Texture, v_TexCoord);

}
"))
   (-position-buffer :initform (make-vertex-buffer (vector
 -0.5 -0.5 -0.5  0.0 0.0
 0.5 -0.5 -0.5  1.0 0.0
 0.5  0.5 -0.5  1.0 1.0
 0.5  0.5 -0.5  1.0 1.0
 -0.5  0.5 -0.5  0.0 1.0
 -0.5 -0.5 -0.5  0.0 0.0

 -0.5 -0.5  0.5  0.0 0.0
 0.5 -0.5  0.5  1.0 0.0
 0.5  0.5  0.5  1.0 1.0
 0.5  0.5  0.5  1.0 1.0
 -0.5  0.5  0.5  0.0 1.0
 -0.5 -0.5  0.5  0.0 0.0

 -0.5  0.5  0.5  1.0 0.0
 -0.5  0.5 -0.5  1.0 1.0
 -0.5 -0.5 -0.5  0.0 1.0
 -0.5 -0.5 -0.5  0.0 1.0
 -0.5 -0.5  0.5  0.0 0.0
 -0.5  0.5  0.5  1.0 0.0

 0.5  0.5  0.5  1.0 0.0
 0.5  0.5 -0.5  1.0 1.0
 0.5 -0.5 -0.5  0.0 1.0
 0.5 -0.5 -0.5  0.0 1.0
 0.5 -0.5  0.5  0.0 0.0
 0.5  0.5  0.5  1.0 0.0

 -0.5 -0.5 -0.5  0.0 1.0
 0.5 -0.5 -0.5  1.0 1.0
 0.5 -0.5  0.5  1.0 0.0
 0.5 -0.5  0.5  1.0 0.0
 -0.5 -0.5  0.5  0.0 0.0
 -0.5 -0.5 -0.5  0.0 1.0

 -0.5  0.5 -0.5  0.0 1.0
 0.5  0.5 -0.5  1.0 1.0
 0.5  0.5  0.5  1.0 0.0
 0.5  0.5  0.5  1.0 0.0
 -0.5  0.5  0.5  0.0 0.0
 -0.5  0.5 -0.5  0.0 1.0))
   )))

(defmethod draw ((quad basic-cube))
  (gl-bind quad)
  (%gl:draw-arrays :triangles 0 36))

(defmethod initialize-instance :after ((cube basic-cube) &key)
  (gl-bind cube)
  (with-vertex-buffer-layout layout
    (add-to-layout layout "position" :float 3)
    (add-to-layout layout "texCoord" :float 2)
    (set-buffer (shader cube) (vao cube) (position-buffer cube) layout)))
