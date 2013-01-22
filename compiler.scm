#lang racket
;; a very stupid betablocker compiler for scheme

;; tags for scheme types in 8 bit
;; means that fixnums go from 0 -> 127!
(define fixnum-tag (string->number "00000000" 2))
(define fixnum-mask (string->number "00000001" 2))
;                            values  ^^^^^^^
(define fixnum-shift 1)

(define boolean-tag (string->number "00000011" 2))
(define boolean-mask (string->number "00000011" 2))
;                            value         ^
(define boolean-shift 2)

(define empty-list-tag (string->number "00000101" 2))

(define (emit . args)
  (printf "   ")
  (apply printf args)
  (printf "~n"))

(define (emit-label label)
  (printf "~a:~n" label))

;; make labels for jmps
(define next-label-id 0)
(define (unique-label)
  (set! next-label-id (add1 next-label-id))
  (string-append "label-" (number->string next-label-id)))

(define (immediate? x)
  (or (integer? x) (boolean? x)))

(define (immediate-rep x)
  (cond
    ((integer? x) (arithmetic-shift x fixnum-shift))
    ((boolean? x) (arithmetic-shift (if x 1 0) boolean-shift))))

(define (primcall? x)
  (and (list? x) (not (null? x)) (symbol? (car x))))

;; convert 1/0 on the stack top to scheme boolean type
(define (emit-top-to-boolean)
  (let ((label (unique-label))
        (out (unique-label)))
    (emit "jmpz ~a" label)
    (emit "psh ~a" (immediate-rep #t))
    (emit "jmp ~a" out)
    (emit-label label)
    (emit "psh ~a" (immediate-rep #f))
    (emit-label out)))
    
(define (emit-unary-procedure x)
  (cond
    ((eq? (car x) 'add1)
     (emit-expr (cadr x))
     (emit "pshl ~a" (immediate-rep 1))
     (emit "add" ))
    ((eq? (car x) 'sub1)
     (emit-expr (cadr x))
     (emit "pshl ~a" (immediate-rep 1))
     (emit "sub" ))
    ((eq? (car x) 'null?)
     (emit-expr (cadr x))
     (emit "pshl ~a" empty-list-tag)
     (emit "equ" )
     (emit-top-to-boolean))
    ((eq? (car x) 'zero?)
     (emit-expr (cadr x))
     (emit "pshl 0")
     (emit "equ" )
     (emit-top-to-boolean))
    ((eq? (car x) 'not)
     (emit-expr (cadr x))
     (emit "not")
     (emit-top-to-boolean))
    ((eq? (car x) 'integer?)
     (emit-expr (cadr x))
     (emit "pshl ~a" fixnum-mask)
     (emit "and")
     (emit "pshl ~a" fixnum-tag)
     (emit "equ" )
     (emit-top-to-boolean))
    ((eq? (car x) 'boolean?)
     (emit-expr (cadr x))
     (emit "pshl ~a" boolean-mask)
     (emit "and")
     (emit "pshl ~a" boolean-tag)
     (emit "equ" )
     (emit-top-to-boolean))        
    (else
     (printf "procedure ~a not understood" (car x)))))

;; just uses betablocker's internal stack
(define (emit-procedure x)
  (cond
    ((eq? (car x) '+)
     (emit-expr (cadr x))
     (emit-expr (caddr x))
     (emit "add" ))
    ((eq? (car x) '-)
     (emit-expr (cadr x))
     (emit-expr (caddr x))
     (emit "sub" ))
    ((eq? (car x) 'and)
     (emit-expr (cadr x))
     (emit-expr (caddr x))
     (emit "and" )
     (emit-top-to-boolean))
    ((eq? (car x) 'or)
     (emit-expr (cadr x))
     (emit-expr (caddr x))
     (emit "or" )
     (emit-top-to-boolean))))

(define (emit-expr x)
  (cond 
    ((immediate? x)
     (emit "pshl ~a" (immediate-rep x)))
    ((primcall? x)
     (cond 
       ((eq? (length x) 2) (emit-unary-procedure x))
       (else (emit-procedure x))))
    ((and (list? x) (null? x)) ;; null list
     (emit "pshl ~a" empty-list-tag))
    (else 
     (printf "don't understand ~a~n" x))))

(define (compile-program x)
  (emit-expr x))

(compile-program '(and
                   (zero? (- (+ 12 4) (+ 43 (add1 34))))
                   (boolean? #f)))