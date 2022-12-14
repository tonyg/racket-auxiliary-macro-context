#lang racket

(require "test-auxiliary-macro-context.rkt")

(define-test-expander zot
  (lambda (stx)
    (syntax-case stx ()
      [(_) #'("bar" (quux) "bar")]
      [_ #'("bar" (zot) "zot")]))
  (syntax-rules ()
    [(_) "i am a zot"]))

(module+ test
  (require rackunit)
  (check-equal? (list (quux) (zot)) (list "i am a quux" "i am a zot"))
  (check-equal? (do-test ("foo" zot "foo")) '(foo (bar (bar (foo bar foo) bar) "zot") foo))
  (check-equal? (do-test ("foo" (zot) "foo")) '(foo (bar (foo bar foo) bar) foo)))
