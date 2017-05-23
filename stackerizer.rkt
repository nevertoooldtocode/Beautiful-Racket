#lang br/quicklang
(provide +
         *
         (rename-out [stackerizer-mb #%module-begin]))

(define-macro (stackerizer-mb EXPR ...)
  #'(#%module-begin
;     (printf "~a\n" EXPR ...)
;     EXPR ...
     (for-each displayln (reverse (flatten EXPR ...)))
     )
  )


; (define-macro-cases +
;        [(+ FIRST) #'FIRST]
;        [(+ FIRST NEXT ...) #'(list '+ FIRST (+ NEXT ...))]
;        )


(define-macro (define-op OP)
  #'(define-macro-cases OP
      [(OP FIRST) #'FIRST]
      [(OP FIRST NEXT (... ...)) #'(list 'OP FIRST (OP NEXT (... ...)))]
      )
  )

(define-macro (define-ops OP ...)
  #'(begin
      (define-macro-cases OP
        [(OP FIRST) #'FIRST]
        [(OP FIRST NEXT (... ...)) #'(list 'OP FIRST (OP NEXT (... ...)))]
        )
      ...)
  )

;(define-op +)
(define-ops + *)
