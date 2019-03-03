;;; tco-test.el --- Tests for tco.el -*- lexical-binding: t -*-

;;; Code:
(require 'ert)
(require 'tco)

(defun-tco tco-sum (n &optional accum)
  "Sum up the numbers in N, adding to ACCUM."
  (setq accum (or accum 0))
  (if (zerop n)
      accum
    (tco-sum (1- n) (+ accum n))))

(ert-deftest tco-readme-test ()
  "Ensure the example in the readme works as claimed."
  (let ((num 2000))
    (should (> num max-lisp-eval-depth))
    ;; We'd get a stack overflow here without TCO.
    (tco-sum num)))

(defun-tco tco-map (items fn &optional accum)
  "Apply FN to every item in ITEMS, and return the result."
  (if (null items)
      (nreverse accum)
    (tco-map
     (cdr items) fn (cons (funcall fn (car items)) accum))))

(ert-deftest tco-map-test ()
  (let ((nums '(1 2 3 4)))
    (should
     (equal
      (tco-map nums #'1+)
      '(2 3 4 5)))))

;;; tco-test.el ends here
