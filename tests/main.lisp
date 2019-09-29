(defpackage rgit/tests/main
  (:use :cl
        :rgit
        :rove))
(in-package :rgit/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :rgit)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
