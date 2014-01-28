#lang racket

(provide provide rename-out struct-out check-error)

(require rackunit)
; (require rackunit/text-ui)
(require (only-in rackunit/text-ui [run-tests run-tests-orig]))

(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ code . msg) #`(check-exn exn:fail? (lambda () code) . msg)]))

(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_ suite-name)
     #`(begin
         (printf 'suite-name "~d~%")
         (run-tests-orig suite-name))]))
