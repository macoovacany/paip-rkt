#lang racket

;;


(define (mappend fn lst)
  (apply append (map fn lst))) 


(define (self-and-double x) (list x (* 2 x)))
(self-and-double 2)  ; '(2 4)
(self-and-double 4.56) ; '(4.56 9.12)
(map self-and-double '(1 20 34))  ; '((1 2) (20 40) (34 68))
(mappend self-and-double '(1 20 34)) ; '(1 2 20 40 34 68)


