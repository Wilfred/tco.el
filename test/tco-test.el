;;; tco-test.el --- Tests for tco.el -*- lexical-binding: t -*-

;;; Code:
(require 'ert)
(require 'tco)

(defun-tco sum (n &optional accum)
  (setq accum (or accum 0))
  (if (zerop n)
      accum
    (sum (1- n) (+ accum n))))

(ert-deftest tco-readme-test ()
  "Ensure the example in the readme works as claimed."
  (let ((num 1000))
    (should (> num max-lisp-eval-depth))
    (sum num)))

;;; tco-test.el ends here
