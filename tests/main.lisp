(defpackage learn-opengl/tests/main
  (:use :cl
        :learn-opengl
        :rove))
(in-package :learn-opengl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :learn-opengl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
