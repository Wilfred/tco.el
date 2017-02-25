# tco.el
**Tail call optimisation for Emacs lisp**

[![Build Status](https://travis-ci.org/Wilfred/tco.el.svg?branch=master)](https://travis-ci.org/Wilfred/tco.el)

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
;; -*- lexical-binding: t -*-
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

## Known issues

Due to
[a bug in cl-letf in Emacs 24.3.1](http://emacs.stackexchange.com/questions/3450/whats-the-correct-replacement-for-flet-on-new-emacsen#comment5015_3452),
this package will not work on Emacs 24.3.1.

## Other Projects

[recur](https://github.com/VincentToups/recur) also offers TCO for
self-tail recursion.
