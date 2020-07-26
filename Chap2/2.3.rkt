#lang racket

;; Chapter 2.3 - using a rules based solution
;;   to create sentences with a simple grammar

;; TODO: import functions properly instead of writing it here like a chump
; 1.7.rkt
(define (mappend fn lst)
  (apply append (map fn lst))) 
; 2.2.rkt
(define (random-elt lst)
  (list-ref lst (random (length lst))))

; ****************************************




(define simple-grammar
  '([sentence → (noun-phrase verb-phrase)]
    [noun-phrase → (article noun)]
    [verb-phrase → (verb noun-phrase)]
    [article → the a]
    [noun → man ball woman table]
    [verb → hit took saw liked]))

; probably try and skip the defvar / defparameter mess.
; however, all the following functions will implicitly assumme
; the existance of the variable *current-grammar* at the
; top level

(define *grammar* '())

; set the current grammar to be our example *simple-grammar*
; in the future we will update the grammar definition
(set! *grammar* simple-grammar)

(define (find-rule lhs)
  (assoc lhs *grammar*))

(define (rule-lhs rule)
  (car rule))

(define (rule-rhs rule)
  (cddr rule))

(define (rewrites rule)
  (rule-rhs (find-rule rule)))

; check top-level works
(writeln "Cheking find-rule, rule-lhs, rule-rhs...")
(find-rule 'sentence)
(find-rule 'verb-phrase)
(find-rule 'noun)

(rule-lhs (find-rule 'noun-phrase))
(rule-rhs (find-rule 'verb-phrase)) ; note- a list of a list
(rule-rhs (find-rule 'verb)) ; note- just a list
(rule-rhs (find-rule 'noun))

;; based on the first generate function in PAIP
(define (generate phrase)
  (cond
    [(list? phrase) (mappend generate phrase)]
    [(find-rule phrase) ; #1
     (generate (random-elt (rewrites phrase)))]
    [else (list phrase)]))

; #1: there is a change of checking condition in the second case. This version
;     uses (find-rule phrase) instead of (rewrites phrase)
;     When the phrase degenerates to a terminal, such as 'the or 'a,
;    (rewrites 'a) will throw an error  "cddr: contract violation."
;    (find-rule 'a) will return false.

; top level checks
(writeln "Checking the generate function")
(print "sentence: ") (generate 'sentence)
(print "sentence: ") (generate 'sentence)
(print "sentence: ") (generate 'sentence)
(print "article: ") (generate 'article)
(print "noun-phrase: ") (generate 'noun-phrase)



(define (generate/2 phrase)
  (if (list? phrase)
      (mappend generate/2 phrase)
      ; Bugfix: this function is not meant to call generate, but generate/2
      ; making this fix breaks this function as per #1.
      (let ([choices (rewrites/2 phrase)])
        (if (null? choices)
            (list phrase)
            (generate/2 (random-elt choices))))))

;; bug fixes to address #1
(define (rewrites/2 rule)
    (let [(r (find-rule rule))]
      (if r
          (rule-rhs r)
          '())))


(writeln "Checking the generate/2 function")
(print "sentence: ") (generate/2 'sentence)
(print "sentence: ") (generate/2 'sentence)
(print "sentence: ") (generate/2 'sentence)
(print "article: ") (generate/2 'article)
(print "noun-phrase: ") (generate/2 'noun-phrase)

;; exercise 2.1 - version of 'generate' that uses cond, but avoids
;; calling rewrite twice
;(define (generate/3 phrase))
; Actually already accomplished at #1. 

;; PAIP Answers exercise 2.1
;; not that the 
(define (generate/paip/2.1 phrase)
  (let ([choices '()])
    (cond
      [(list? phrase) (mappend generate/paip/2.1 phrase)]
      [(set! choices (rewrites/2 phrase))  ; #1
       (if (null? choices)
           (list phrase)
           (generate/paip/2.1 (random-elt choices)))])))

(writeln "Checking the generate function, answer from PAIP 2.1")
(print "sentence: ") (generate/paip/2.1 'sentence)
(print "sentence: ") (generate/paip/2.1 'sentence)
(print "sentence: ") (generate/paip/2.1 'sentence)
(print "article: ") (generate/paip/2.1 'article)
(print "noun-phrase: ") (generate/paip/2.1 'noun-phrase)

         
;; exercise 2.2 'generate' that explicitly differentiates between
;; terminal and non-terminal symbols. Already accomplished by note #1

;; PAIP Answers exercise 2.2
(define (generate/paip/2.2 phrase)
  (define (non-terminal? category)
    (not (null? (rewrites/2 category))))
  (cond
    [(list? phrase) (mappend generate/paip/2.2 phrase)]
    [(non-terminal? phrase)
     (generate/paip/2.2 (random-elt (rewrites/2 phrase)))]
    [else (list phrase)]))

(writeln "Checking the generate function, answer from PAIP 2.2")
(print "sentence: ") (generate/paip/2.2 'sentence)
(print "sentence: ") (generate/paip/2.2 'sentence)
(print "sentence: ") (generate/paip/2.2 'sentence)
(print "article: ") (generate/paip/2.2 'article)
(print "noun-phrase: ") (generate/paip/2.2 'noun-phrase)
    

