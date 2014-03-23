;;; tco.el --- tail-call optimisation for Emacs lisp -*- lexical-binding: t -*-

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.2
;; Package-Requires: ((dash "1.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; tco.el provides tail-call optimisation for functions in elisp that
;; call themselves in tail-position.

;; It works by replacing each self-call with a thunk, and wrapping the
;; function body in a loop that repeatedly evaluates the thunk.  Roughly
;; speaking, a function `foo`:

;; \(defun-tco foo (...)
;;   (...)
;;   (foo (...)))

;; Is rewritten as follows:

;; \(defun foo (...)
;;    (flet (foo-thunk (...)
;;                (...)
;;                (lambda () (foo-thunk (...))))
;;      (let ((result (apply foo-thunk (...))))
;;        (while (functionp result)
;;          (setq result (funcall result)))
;;        result)))

;;; Code:

(require 'dash)
(eval-when-compile (require 'cl))

(setq lexical-binding t)

(defun tco-add-trampoline (fun-name new-name form)
  "Given quoted soure FORM, replace calls to FUN-NAME (a symbol)
with a lambda expression that returns the result of the FUN-NAME call."
  (--map
   (cond
    ((consp it)
     (if (eq (car it) fun-name)
         `(lambda () (,new-name ,@(cdr it)))
       (tco-add-trampoline fun-name new-name it)))
    ('t it))
   form))

;; todo: error if not in tail position
;; todo: macro-expand function body first
;; todo: preserve function arity to improve byte-compiler warnings
;; todo: docstring support
(defmacro defun-tco (function-name args &rest body)
  "Defines a function FUNCTION-NAME with self-tail-call optimisation.
BODY must contain calls to FUNCTION-NAME in the tail position."
  (let* ((name (make-symbol "trampolined-function"))
         (trampolined
          (tco-add-trampoline function-name name body))
         (fun-args (make-symbol "outer-fun-args"))
         (result (make-symbol "trampolined-result")))
    `(defun ,function-name (&rest ,fun-args)
       (flet ((,name ,args ,@trampolined))
         (let ((,result (apply ',name ,fun-args)))
           (while (functionp ,result)
             (setq ,result (funcall ,result)))
           ,result)))))

(provide 'tco)
;;; tco.el ends here

