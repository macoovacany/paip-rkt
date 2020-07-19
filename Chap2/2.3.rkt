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



;; alternatative way to write generate
(define (generate/2 phrase)
  (if (list? phrase)
      (mappend generate phrase)
      (let ([choices (rewrites phrase)])
        (if (null? choices)
            (list phrase)
            (generate (random-elt choices))))))

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

          
;; exercise 2.2 'generate' that explicitly differentiates between
;; terminal and non-terminal symbols
(define (generate/4 phrase)
  (define (terminals-only? phrase)
    (list? phrase))
  ; check to see if the phrase represents a rule
  ; and then check if it's a terminal, or subrule
  (if (find-rule phrase) ; rule found
      (if (terminals-only? phrase)
          (mappend generate phrase) ; write out terminals
          ; otherwise the phrase represents a rule that can be rewrriten
          ; e.g. noun-phrase. recurse back into generate until terminals only
          (generate (random-elt (rewrites phrase))))
      ; rule not found, therefore phrase is a terminal, and just return it
      (list phrase)))
      
(writeln "Checking the generate/4 function")
(print "sentence: ") (generate/4 'sentence)
(print "sentence: ") (generate/4 'sentence)
(print "sentence: ") (generate/4 'sentence)
(print "article: ") (generate/4 'article)
(print "noun-phrase: ") (generate/4 'noun-phrase)


    


