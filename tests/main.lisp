(defpackage gendo-bot/tests/main
  (:use :cl
        :gendo-bot
        :rove))
(in-package :gendo-bot/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :gendo-bot)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
