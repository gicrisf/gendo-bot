(defpackage listener-bot/tests/main
  (:use :cl
        :listener-bot
        :rove))
(in-package :listener-bot/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :listener-bot)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
