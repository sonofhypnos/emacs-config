;;; prompts/test.el -*- lexical-binding: t; -*-
(defmacro my-lexical-bound-p (var)
  "Returns t if VAR is going to work as a lexically bound variable. Nil otherwise."
  `(let ((,var nil)
         (f (let ((,var t)) (lambda () ,var))))
     (funcall f)))
(my-lexical-bound-p case-fold-search)

(defun bob-repeat (n func)
  "Calls FUNC repeatedly, N times."
  (dotimes (i n)
    (funcall func)))
(defun alice-insert-stuff ()
  (interactive)
  ;; inserts "1111111111\n2222222222\n3333333333" to current buffer
  (dolist (i '("a" "b" "c"))
    (bob-repeat 10
               (lambda ()
                 (insert i)))
    (insert "\n")))
;; the reason it didn't work before was because

(let ((x 12))
       (lambda (y)
         (* x y)))

(my-print-safe

(defmacro my-print-safe (object)
  "Prints OBJECT. If error, prints error."
  `(progn
     (condition-case err
         (progn
           (print ,object))
       (error
        (terpri)
        (princ (format "Eval of %S resulted in error %S"
                       ',object err))
        (terpri)))))
