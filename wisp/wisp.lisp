;; > (defun foo (x) x)
;; '(set-symbol-function foo (lambda (x) x))

(set-symbol-function
 (quote defun)
 (macro
  (name params body)
  (cons (quote set-symbol-function)
        (cons name
              (cons (cons (quote lambda)
                          (cons params
                                (cons body nil))) nil)))))
