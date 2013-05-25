# tco.el
## Tail call optimisation for Emacs lisp

Example usage:

    (require 'tco)
    
    (defun-tco fact (x &optional accum)
      (setq accum (or accum 1))
      (if (eql x 1) accum
        (fact (1- x) (* accum x))))

    ;; values greater than `max-lisp-eval-depth'
    ;; would cause stack overflow here:
    (fact 700)

## Todo

* Better example than factorial, since 32-bit Emacs overflows so 32!
  is 0.

