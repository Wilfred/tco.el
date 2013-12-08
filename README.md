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

;; Without TCO, values greater than `max-lisp-eval-depth' (usually
;; 600) would cause stack overflow here:
(sum 700)
```

## Todo

* Unit tests
