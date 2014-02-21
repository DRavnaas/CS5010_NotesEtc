#lang racket

(provide provide rename-out struct-out check-error)

(provide display)

(require rackunit)
(require rackunit/text-ui)

(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ code . msg) #`(check-exn exn:fail? (lambda () code) . msg)]))

;; for Problem Set 6, render-expr problem

;; ListOf<String> -> Void
;; GIVEN: A List of Strings
;; EFFECT: displays the strings in separate lines
;; RETURNS: the empty string
(define (display-strings! strs)
  (if (empty? strs) (void)
      (let* ((d1 (display (first strs)))
             (d2 (display "\n")))
        (display-strings! (rest strs)))))

(provide display-strings!)

