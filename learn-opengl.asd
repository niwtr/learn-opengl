(defsystem "learn-opengl"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-opengl
               :cl-glu
               :cl-glut
               :sdl2
	       :sdl2-image
               )
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "utils")
		 (:file "opengl-utils")
		 (:file "01")
                 (:file "02")
                 (:file "03")
                 (:file "04")
		 (:file "05")
		 (:file "06")
		 (:file "07")
                 (:file "opengl")
		 (:file "opengl-shader")
		 )))
  :description ""
  :in-order-to ((test-op (test-op "learn-opengl/tests"))))

(defsystem "learn-opengl/tests"
  :author ""
  :license ""
  :depends-on ("learn-opengl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for learn-opengl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
