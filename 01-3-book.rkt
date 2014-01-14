;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-4-books) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(define-struct book (author title on-hand price))

;; A Book is a 
;;  (make-book String String Number Number)
;; Interpretation:
;;   author is the author’s name
;;   title is the title
;;   on-hand is the number of copies on hand
;;   price is the price in USD

;; book-fn : Book -> ??
(define (book-fn b)
  (...
    (book-author b)
    (book-title b)
    (book-on-hand b)
    (book-price b)))

