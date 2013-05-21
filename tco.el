;; tco.el --- tail-call optimisation -*- lexical-binding: t -*-
(require 'dash)
(eval-when-compile (require 'cl))

(setq lexical-binding 't)

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

;; example usage
(defun-tco fact (x &optional accum)
  (setq accum (or accum 1))
  (if (eql x 1) accum
    (fact (1- x) (* accum x))))

