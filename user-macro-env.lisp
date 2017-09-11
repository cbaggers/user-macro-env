(in-package #:user-macro-env)

;;--------------------------------------------------
;; Magic by stassats

(defconstant +env-var-name+ 'user-env)

(defun macro-var-get (name env)
  (getf (macroexpand-1 '(declaration-macro) env) name))

(defmacro declaration-macro () nil)

(defmacro let-macro-var ((name value) &body body &environment env)
  `(macrolet ((declaration-macro ()
                '(,name ,value ,@(macroexpand-1 '(declaration-macro) env))))
     ,@body))

;;--------------------------------------------------

(defvar *standard-declarations*
  '(dynamic-extent  ignore     optimize
    ftype           inline     special
    ignorable       notinline  type))

;;--------------------------------------------------

(defclass user-macro-env ()
  ((bindings :initarg :bindings)
   (parent :initarg :parent)))

(defun env! (&optional bindings parent)
  (make-instance 'user-macro-env
                 :bindings (or bindings (make-hash-table))
                 :parent parent))

(defun wrap-env (env)
  (make-instance 'user-macro-env
                 :bindings (make-hash-table)
                 :parent env))

(defmethod make-load-form ((s-env user-macro-env) &optional environment)
  (declare (ignore environment))
  (with-slots (bindings parent) s-env
    `(env! ,bindings ,parent)))

(defmethod bindings ((env user-macro-env))
  (with-slots (bindings parent) env
    (append bindings (when parent (bindings parent)))))

(defun user-env-get (env)
  (or (macro-var-get +env-var-name+ env) (env!)))

(defun get-thang (x env)
  (with-slots (bindings parent) env
    (multiple-value-bind (val has) (gethash x bindings)
      (if has
          val
          (when parent
            (get-thang x parent))))))

(defun %do-foo (x y user-env)
  (format t "~%ooh: ~s" (get-thang x user-env))
  (with-slots (bindings) user-env
    (setf (gethash x bindings) y)))

(defmacro inject-env-func-wrappers (&body body)
  `(macrolet (;; the macros that mutate the env
              (do-foo (x y)
                (list '%do-foo x y +env-var-name+)))
     ,@body))

(defmacro with-user-env (cl-env &body body)
  (let ((last-env (gensym "LAST-ENV")))
    `(let* ((,last-env (user-env-get ,cl-env))
            (,+env-var-name+ (wrap-env ,last-env))
            (their-code (inject-env-func-wrappers ,@body)))
       (append
        (if (and ,+env-var-name+
                 (> (hash-table-count (slot-value ,+env-var-name+ 'bindings))
                    0))
            (list 'let-macro-var (list ',+env-var-name+ ,+env-var-name+))
            '(progn))
        (list (list 'locally their-code))))))

(defmacro my-test (&environment env x y)
  (with-user-env env
    (do-foo x 20)
    `(+ ,x ,y)))

#+nil
(defun boo ()
  (my-test 1 (my-test 3 (my-test 1 2))))

;; to really implement the extensible environments things we would need to
;; shadow locally,let,flet,etc in order to capture the bindings.
;;
;;
