# tco.el
## Tail call optimisation for Emacs lisp

Example usage:

```lisp
(require 'tco)
(setq lexical-binding 't)

(defun-tco sum (n &optional accum)
  (setq accum (or accum 0))
  (if (zerop n)
      accum
    (sum (1- n) (+ accum n))))

;; values greater than `max-lisp-eval-depth'
;; would cause stack overflow here:
(sum 700)
```

## Todo

* Unit tests
