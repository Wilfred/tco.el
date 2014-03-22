# tco.el
** Tail call optimisation for Emacs lisp **

tco.el provides tail-call optimisation for functions in elisp that
call themselves in tail-position. Mutually recursive functions are
unchanged.

It works by replacing each self-call with a thunk, and wrapping the
function body in a loop that repeatedly evaluates the thunk. Roughly
speaking, a function `foo`:

```lisp
(defun-tco foo (...)
  (...)
  (foo (...)))
```

Is rewritten as follows:

```lisp
(defun foo (...)
   (flet (foo-thunk (...)
               (...)
               (lambda () (foo-thunk (...))))
     (let ((result (apply foo-thunk (...))))
       (while (functionp result)
         (setq result (funcall result)))
       result)))
```

## Example

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
