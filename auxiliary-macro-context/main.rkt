#lang racket/base
;; Generalization of define-match-expander.

(provide define-auxiliary-macro-context)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(define-syntax (define-auxiliary-macro-context stx)
  (syntax-case stx ()
    [(_ #:context-name expander-name
        #:prop-name prop:expander
        #:prop-predicate-name expander?
        #:prop-accessor-name expander-proc
        #:macro-definer-name define-expander
        #:introducer-parameter-name current-expander-introducer
        #:local-introduce-name syntax-local-expander-introduce
        #:expander-form-predicate-name expander-form
        #:expander-transform-name expander-transform)
     (quasisyntax/loc stx
       (begin
         (begin-for-syntax
           (define-values (prop:expander expander? expander-proc)
             (make-struct-type-property 'expander-name
                                        (lambda (field-index struct-info)
                                          (define accessor (list-ref struct-info 3))
                                          (lambda (the-expander stx)
                                            ((accessor the-expander field-index) stx)))))

           (define make-expander
             (let ()
               (define-struct expander-name (expander-proc expression-proc)
                 #:property prop:set!-transformer
                 (lambda (the-expander stx)
                   (define xf
                     (#,(format-id #'expander-name "~a-~a" #'expander-name #'expression-proc)
                      the-expander))
                   (define proc
                     (cond [(rename-transformer? xf)
                            (lambda (x)
                              (define target (rename-transformer-target xf))
                              (syntax-case stx (set!)
                                [(set! id args (... ...))
                                 #`(set! #,target args (... ...))]
                                [(id args (... ...))
                                 (datum->syntax stx
                                                `(,target ,@(syntax->list #'(args (... ...))))
                                                stx stx)]
                                [_ (rename-transformer-target xf)]))]
                           [(set!-transformer? xf)
                            (set!-transformer-procedure xf)]
                           [(procedure? xf)
                            (lambda (stx)
                              (syntax-case stx (set!)
                                [(set! . _)
                                 (raise-syntax-error #f "cannot mutate syntax identifier" stx)]
                                [_ (xf stx)]))]
                           [else (raise-syntax-error
                                  #f
                                  (format "not a procedure for ~a transformer" 'expander-name)
                                  stx)]))
                   (proc stx))
                 #:property prop:expander
                 (struct-field-index expander-proc))
               expander-name))

           (define (expander-id? stx)
             (and (identifier? stx)
                  (expander? (syntax-local-value stx (lambda () #f)))))

           (define (expander-form stx)
             (syntax-case stx ()
               [(expander args (... ...)) (expander-id? #'expander) #'expander]
               [expander (expander-id? #'expander) #'expander]
               [_ #f]))

           (define current-expander-introducer
             (make-parameter
              (lambda (x)
                (error 'current-expander-introducer
                       "not expanding ~a form"
                       'expander-name))))

           (define (syntax-local-expander-introduce x)
             (unless (syntax? x)
               (raise-argument-error 'syntax-local-expander-introduce "syntax?" x))
             ((current-expander-introducer) x))

           (define (expander-transform disarmed-stx k)
             (let* ((expander-stx (expander-form disarmed-stx))
                    (expander* (syntax-local-value expander-stx))
                    (transformer (expander-proc expander*))
                    (transformer (if (set!-transformer? transformer)
                                     (set!-transformer-procedure transformer)
                                     transformer)))
               (define introducer (make-syntax-introducer))
               (parameterize ((current-expander-introducer introducer))
                 (let* ((mstx (introducer (syntax-local-introduce disarmed-stx)))
                        (mresult (if (procedure-arity-includes? transformer 2)
                                     (transformer expander* mstx)
                                     (transformer mstx)))
                        (result (syntax-local-introduce (introducer mresult))))
                   (k result))))))

         (define-syntax (define-expander stx)
           (syntax-case stx ()
             [(_ id ep xp)
              (if (identifier? #'xp)
                  (syntax/loc stx
                    (define-syntax id
                      (make-expander
                       ep
                       (lambda (stx)
                         (syntax-case stx (set!)
                           [(nm . args) #'(xp . args)]
                           [nm (identifier? #'nm) #'xp]
                           [(set! . _)
                            (raise-syntax-error #f
                                                (format "~a cannot be target of a set!"
                                                        'expander-name)
                                                stx)])))))
                  (syntax/loc stx
                    (define-syntax id
                      (make-expander ep xp))))]
             [(_ id ep)
              (syntax/loc stx
                (define-expander id ep (lambda (stx)
                                         (raise-syntax-error
                                          #f
                                          (format "this ~a cannot be used in expression position"
                                                  'expander-name)
                                          stx))))]))))]))
