#lang racket

;; Chapter 2.5 - using a rules based solution
;;   to create sentences with a more complex grammar

;; TODO: import functions properly instead of writing it here like a chump
; 1.7.rkt
(define (mappend fn lst)
  (apply append (map fn lst))) 

; 2.2.rkt
(define (random-elt lst)
  (list-ref lst (random (length lst))))

; 2.3.rkt
(define (find-rule lhs)
  (assoc lhs *grammar*))

(define (rule-lhs rule)
  (car rule))

(define (rule-rhs rule)
  (cddr rule))

; updated rewrites ruls from PAIP to address
; the nil != #false
(define (rewrites rule)
    (let [(r (find-rule rule))]
      (if r
          (rule-rhs r)
          '())))


(define (generate phrase)
  (cond
    [(list? phrase) (mappend generate phrase)]
    [(find-rule phrase) ; #1
     (generate (random-elt (rewrites phrase)))]
    [else (list phrase)]))


; ****************************************
(define bigger-grammar
  '([sentence → (noun-phrase verb-phrase)]
    [noun-phrase → (article adj* noun pp*) (name) (pronoun)]
    [verb-phrase → (verb noun-phrase pp*)]
    [pp* → '() (pp pp*)]
    [pp → (prep noun-phrase)]
    [prep → to in by with on]
    [adj* → () (adj adj*)]
    [adj → big little blue green exothermic]
    [name → adam bob charlie dan]
    [article → the a]
    [noun → man ball woman table]
    [verb → hit took saw liked]
    [pronoun → he she it these those that]))

; define and set the appropriate grammar 
(define *grammar* '())
(set! *grammar* bigger-grammar)

(writeln "Checking with a bigger grammar!")         
(print "sentence: ") (generate 'sentence)