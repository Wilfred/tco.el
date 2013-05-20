;; tco.el --- tail-call optimisation -*- lexical-binding: t -*-
(require 'dash)
(eval-when-compile (require 'cl))

(defun tco-add-trampoline (fun-name new-name form)
  "Given quoted soure FORM, replace calls to FUN-NAME (a symbol)
with a lambda expression that returns the result of the FUN-NAME call."
  (--map
   (cond
    ((consp it)
     (if (eq (car it) fun-name)
         `(lambda () (,new-name ,(cdr it)))
       (tco-add-trampoline fun-name new-name it)))
    ('t it))
   form))

(tco-add-trampoline 'foo 'foo-inner '(progn (baz) (foo 1 2) (bar)))

;; todo: error if not in tail position
;; todo: macro-expand function body first
(defmacro defun-tco (function-name args &rest body)
  (let* ((name (make-symbol "trampolined-function"))
         (trampolined
          (tco-add-trampoline function-name name body)))
    `(defun ,function-name ,args
       ;; todo: args needs to be fresh variables, see fact3
       (flet ((,name ,args ,@trampolined))
         (let ((result (,name ,@args)))
           (while (functionp result)
             (setq result (funcall result)))
           result)))))

(defun-tco fact (x &optional accum)
  (setq accum (or accum 1))
  (if (eql x 1) accum
    (fact (1- x) (* accum x))))

(defun-tco fact2 (x accum)
  (if (eql x 1) accum
    (fact (1- x) (* accum x))))

;; (fact2 5 1)
(setq lexical-binding 't)

(defun fact (x accum)
  (if (eql x 1) accum
    (lambda () (fact (1- x) (* accum x)))))

(defun fact3 (x accum)
  (flet ((fact3_
          (x_ accum_)
          (if (eql x_ 1) accum_
            (lambda () (fact3_ (1- x_) (* accum_ x_))))))
    (let ((result (fact3_ x accum)))
      (while (functionp result)
        (setq result (funcall result)))
      result)))

(fact3 5 1)
