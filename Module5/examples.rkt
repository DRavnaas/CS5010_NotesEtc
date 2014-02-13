;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define loi (list 5 4 3 2 1))
;;(extract < loi 2)

; [List-of Number] -> Number 
; compute the product of  
; the numbers on l 
;(define (product l) 
;  (cond 
;    [(empty? l) 1] 
;    [else 
;     (* (first l) 
;        (product (rest l)))])) 
;
;; [List-of Number] -> Number 
;; compute the sum of  
;; the numbers on l 
;(define (sum l) 
;  (cond 
;    [(empty? l) 0] 
;    [else 
;     (+ (first l) 
;        (sum (rest l)))])) 
;
;(sum loi)
;(product loi)


;(define (product-n x y) (* x y))
;(define (sum-n x y) (+ x y))

;(define (abstract-op R empty-val l) 
;  (cond 
;    [(empty? l) empty-val] 
;    [else 
;     (R (first l) 
;        (abstract-op R empty-val (rest l)))]))

;(abstract-op product-n 1 loi)
;(abstract-op sum-n 0 loi)


; [X Y] [List-of X] Y (X  Y -> Y) -> Y 
(define (reduce l base combine) 
  (cond 
    [(empty? l) base] 
    [else (combine (first l) 
                   (reduce (rest l) base combine))])) 

; [List-of Number] -> Number 
(define (sum lon) 
  (reduce lon 0 +)) 

; [List-of Number] -> Number 
(define (product lon) 
  (reduce lon 1 *)) 

; [List-of Posn] -> [List-of Posn] 
; add 3 to each x coordinate on the given list  
  
(check-expect (add-3-to-all (list (make-posn 30 10) (make-posn 0 0))) 
              (list (make-posn 33 10) (make-posn 3 0))) 
  
(define (add-3-to-all lop) 
  (local (; Posn -> Posn 
          ; add 3 to the x coordinate of the given Posn 
          (define (add-3-to-one p) 
            (make-posn (+ (posn-x p) 3) (posn-y p)))) 
    (map add-3-to-one lop))) 

;(lambda (variable-1 ... variable-N) expression)

(map 
 (lambda (n) (+ 5 n)) 
 (list 1 2 3))

(define (add-x-to-each x lon)
  (map 
   (lambda (n)  (+ x n)) lon))

(add-x-to-each 5 (list 1 2 3))

(define (intersect l1 l2)
  (intersect-helper l1 l2 empty))

(define (intersect-helper l1 l2 answer-so-far)
  (if (empty? l1)
      answer-so-far
      ; else
       (if (member? (first l1) l2)
           (intersect-helper (rest l1) (remove-first (first l1) l2)
                             (cons (first l1) answer-so-far))
           (intersect-helper (rest l1) l2 answer-so-far))))
       
      
(define (remove-first item list)
  (cond 
    [(empty? list) empty]
    [(equal? item (first list)) (rest list)]
    [else (cons (first list) (remove-first item (rest list)))]))
  
;
(define (has-match-in-same-position? l1 l2 fn)
  (cond 
    [(empty? l1) false]
    [(empty? l2) false]
    [(fn (first l1) (first l2)) (first l1)]
    [else (has-match-in-same-position? (rest l1) (rest l2) fn)]))

(has-match-in-same-position? '(1 2 3) '(2 5 9) (lambda(x y) (= x (* y y))))