#lang br/quicklang

(provide read-syntax handle + * $)

; Reader

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define module-datum
    `(module stacker-mod "stacker.rkt" ,@src-datums))
  (datum->syntax #f module-datum))

; Expander

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (car stack))))

(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack '())

(define (pop-stack!)
  (define item (car stack))
  (set! stack (cdr stack))
  item)

(define (push-stack! item)
  (set! stack (cons item stack)))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg) (equal? $ arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))

(define ($ a b) (+ a b))



