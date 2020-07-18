#lang racket

;; used for demonstration purposes
;; some of the functions are called to see the results in the top-level debugger


; used to demonstrate tracing in racket
(require racket/trace)

; basic grammar constructions
(define (sentence) (append (noun-phrase) (verb-phrase)))
(define (noun-phrase) (append (article) (noun)))
(define (verb-phrase) (append (verb) (noun-phrase)))

(define (article)
  (one-of '(the a)))

(define (noun)
  (one-of '(man ball woman table)))

(define (verb)
  (one-of '(hit took saw liked)))

; utility functions to create a sentence
;; note that we have split up the one-of function beacause the random-elt function
;; is useful in the X* functions
(define (one-of lst)
  (list (random-elt lst)))

(define (random-elt lst)
  (list-ref lst (random (length lst))))

(sentence)
(sentence)
(sentence)
(noun-phrase)
(verb-phrase)

(trace sentence noun-phrase verb-phrase article noun verb)

(sentence)

(untrace sentence noun-phrase verb-phrase article noun verb)


;; extension to use prepositional phrases

(define (sentence/2) (append (noun-phrase/2) (verb-phrase)))
(define (noun-phrase/2) (append (article) (adj*) (noun) (pp*)))
(define (pp)
  (append (prep) (noun-phrase/2)))

(define (prep)
  (one-of '(to in by with on)))
(define (adj)
  (one-of '(big little blue green adiabatic copacetic)))

; kleene star for adjectives and prepositional phrases
(define (adj*)
  ; I've decided to use 'case' instead of 'if' beacuse I can
  (case (random 2)
    [(0) '()]
    [(1) (append (adj) (adj*))]))

(define (pp*)
  (if (random-elt '(#true #false))
      (append (pp) (pp*))
      '()))

;(adj*)(adj*)(adj*)(adj*)(adj*)(adj*)(adj*)
(sentence/2)


