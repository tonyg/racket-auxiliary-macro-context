#lang racket

(require "test-auxiliary-macro-context.rkt")

(define-test-expander zot
  (syntax-rules ()
    [(_) ("bar" (quux) "bar")])
  (syntax-rules ()
    [(_) "i am a zot"]))

(module+ test
  (require rackunit)
  (check-equal? (list (quux) (zot)) (list "i am a quux" "i am a zot"))
  (check-equal? (do-test ("foo" (zot) "foo")) '(foo (bar (foo bar foo) bar) foo)))
