;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require rackunit)
(require rackunit/text-ui)

;; DATA DEFINITION:
;; a TrafficLightState (TLState) is one of
;; -- "red"
;; -- "yellow" 
;; -- "green"

;; TEMPLATE
;; tls-fn : TLState -> ??
;(define (tls-fn a-tls)
;  (cond
;    [(string=? a-tls "red")    
;     ...]
;    [(string=? a-tls "yellow")
;     ...]
;    [(string=? a-tls "green")  
;     ...]))    

;; next-state : TLState -> TLState
;; GIVEN: a TLState
;; RETURNS: the TLState that follows the given TLState
;; (next-state "red") = "green"
;; (next-state "yellow") = "red"
;; (next-state "green") = "yellow"
;; STRATEGY: structural decomposition [TLState]

(define (next-state a-tls) ...)

(define-test-suite next-state-tests
  "next-state-tests"
  (check-equal? (next-state "green") "yellow"
    "next state after green should be yellow")

  (check-equal? (next-state "yellow") "red"
    "next state after yellow should be red")

  (check-equal? (next-state "red") "green"
    "next state after red should be green"))

(run-tests next-state-tests)

;; two-states-later : TLState -> TLState
;; given a TLState, produces the state that should occur two steps later
;; (two-states-later "green") = "red", etc.
;; design strategy: FUNCTIONAL COMPOSITION
(define (two-states-later tls1)
  (next-state (next-state tls1)))

;; previous-state : TLState -> TLState
;; GIVEN: a TLState
;; RETURNS: the state that preceded it
;; (previous-state "red") = "yellow"
;; (previous-state "yellow") = "green"
;; (previous-state "green") = "red"
;; STRATEGY: Structural Decomposition [TLState]
(define (previous-state a-tls) ...)

(define-test-suite previous-state-tests
  (check-equal? (previous-state "red") "yellow"
    "red should be preceded by yellow")

  (check-equal? (previous-state "yellow") "green"
    "yellow should be preceded green")

  (check-equal? (previous-state "green") "red"
    "green should be preceded by red"))

(run-tests previous-state-tests)

