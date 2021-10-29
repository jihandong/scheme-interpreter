#lang scheme

;;; display expression to show the evaluating sequence.
(define (eval exp env) (display exp))

;;; left to right: evaluate head expression by `let car-value`.
(define (list-of-values-left exps env)
  (cond ((null? exps) '())
        (else (let ((car-value (eval (car exps) env)))
                (cons car-value
                      (list-of-values-left (cdr exps) env))))))

;;; right to left: evaluate tail expressions by `let cdr-value `.
(define (list-of-values-right exps env)
  (cond ((null? exps) '())
        (else (let ((cdr-value (list-of-values-right (cdr exps) env)))
                (cons (eval (car exps) env)
                      cdr-value)))))
;;; examples
(list-of-values-left  '(1 2 3 4) '())
(list-of-values-right '(1 2 3 4) '())