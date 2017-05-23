#lang br/quicklang

(provide read-syntax handle-args handle-args-jan + *)

; Reader

(define (read-syntax path port)
;  (printf "Reader called with path ~a, and port ~a\n" path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define module-datum
    `(module stacker-mod "funstacker.rkt" (handle-args-jan ,@src-datums)))
  (datum->syntax #f module-datum))

; Expander

(define-macro (funstacker-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     (display (first HANDLE-ARGS-EXPR))))

(provide (rename-out [funstacker-module-begin #%module-begin]))

(define (handle-args . args)
  (for/fold ([stack-acc empty])
            ([arg (filter-not void? args)])
    (printf "Stack-acc: ~a\n" stack-acc)
    (cond
      [(number? arg) (cons arg stack-acc)]
      [(or (equal? * arg) (equal? + arg))
       (define op-result
         (arg (first stack-acc) (second stack-acc)))
       (cons op-result (drop stack-acc 2))])))

(define (handle-args-jan . args)
  (define (iter result rest)
    (printf "Result: ~a, Rest: ~a\n" result rest)
    (cond
      ((null? rest) result)
      ((number? (car rest))
       (iter (cons (car rest) result) (cdr rest)))
      ((or (equal? + (car rest)) (equal? * (car rest)))
       (iter (cons ((car rest) (car result) (cadr result)) (cddr result)) (cdr rest)))
      ))
  (iter '() (filter-not void? args)))






