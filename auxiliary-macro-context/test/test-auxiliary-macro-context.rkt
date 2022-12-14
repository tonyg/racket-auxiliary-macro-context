#lang racket

(provide (all-defined-out))

(require (for-syntax racket))
(require "../main.rkt")

(define-for-syntax orig-insp
  (variable-reference->module-declaration-inspector (#%variable-reference)))

(define-auxiliary-macro-context
  #:context-name test-expander
  #:prop-name prop:test-expander
  #:prop-predicate-name test-expander?
  #:prop-accessor-name test-expander-proc
  #:macro-definer-name define-test-expander
  #:introducer-parameter-name current-test-expander-introducer
  #:local-introduce-name syntax-local-test-introduce
  #:expander-form-predicate-name test-expander-form?
  #:expander-transform-name test-expander-transform)

(define-for-syntax (parse-test stx)
  (define disarmed-stx (syntax-disarm stx orig-insp))
  (syntax-case disarmed-stx ()
    ["foo"
     #'foo]
    ["bar"
     #'bar]
    ["zot"
     "zot"]
    [expander
     (test-expander-form? #'expander)
     (test-expander-transform disarmed-stx
                              (lambda (result) (parse-test (syntax-rearm result stx))))]
    [(piece ...)
     (quasisyntax/loc stx
       (#,@(map parse-test (syntax->list #'(piece ...)))))]))

(define-test-expander quux
  (lambda (stx)
    (syntax-case stx ()
      [(_) #'("foo" "bar" "foo")]
      [id (identifier? #'id) #'("zot")]))
  (syntax-rules ()
    [(_) "i am a quux"]))

(define-syntax (do-test stx)
  (syntax-case stx ()
    [(_ p)
     (quasisyntax/loc stx
       (quote #,(parse-test #'p)))]))

(module+ test
  (require rackunit)
  (check-equal? (quux) "i am a quux")
  (check-equal? (do-test ("bar" "bar" (quux) "bar" "bar")) '(bar bar (foo bar foo) bar bar))
  (check-equal? (do-test ("bar" "bar" quux "bar" "bar")) '(bar bar ("zot") bar bar)))
