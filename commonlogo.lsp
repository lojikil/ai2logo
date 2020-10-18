;; simple Logo test...
(defun add-env! (sym val env)
  (setf (gethash sym env) val))
(defun get-env (sym env)
  (multiple-value-bind (val status) (gethash sym env) (list status val)))
(defun @list-eval (expr env)
  (cond
   ((typep expr 'null) nil)
   ((numberp (car expr)) (cons (car expr) (@list-eval (cdr expr) env)))
   (t (cons (eval-commonlogo (car expr) env) (@list-eval (cdr expr) env)))))
(defun @apply-form (form args)
  (cond
   ((eq form 'sum) (apply #'+ args))
   ((eq form 'sub) (apply #'- args))
   ((eq form 'mul) (apply #'* args))
   ((eq form 'div) (apply #'/ args))
   ((eq form 'sin) (apply #'sin args))
   ((eq form 'cos) (apply #'cos args))
   ((eq form 'tan) (apply #'tan args))
   ((eq form 'mod) (apply #'mod args))
   ((eq form 'rem) (apply #'rem args))
   ((eq form 'quote) args)
   ((eq form 'first) (car args))
   ((eq form 'butfirst) (cdr args))))
(defun eval-commonlogo (expr env)
  "evaluate \"compiled\" commonlogo forms that have been built in read-commonlogo; you can also do (eval-commonlogo '(sum 34 56) some-hash-table)..."
  (cond
   ((numberp expr) expr)
   (t (@apply-form (car expr) (@list-eval (cdr expr) env)))))
(defun read-commonlogo ()
  "read & \"compile\" commonlogo forms"
)
(defun load-commonlogo (file env)
  "attempt to load a commonlogo file from disk into the current env"
  ;...
  (list file env)
)
(defun commonlogo ()
  (loop
    with e = (make-hash-table)
    for i = (read-commonlogo)
    while (not (eq i 'quit))
    do
    (progn
     (print (eval-commonlogo i e)))))
