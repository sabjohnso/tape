#lang typed/racket

(require optional/typed)

(provide
 Tape
 tape?
 empty-tape
 tape-empty? tape-front? tape-back?
 tape-position tape-remaining tape-length
 tape-fwd tape-bwd tape-move-by tape-move-to)

(: rappend (∀ (a) ((Listof a) (Listof a) . -> . (Listof a))))
(define (rappend xs ys)
  (if (null? xs) ys
      (rappend (cdr xs) (cons (car xs) ys))))

(: head (∀ (a) ((Listof a) . -> . (Optional a))))
(define (head xs)
  (match xs
    [(list x _ ...) (some x)]
    [(list) (none)]))

(: tail (∀ (a) ((Listof a) . -> . (Listof a))))
(define (tail xs)
  (match xs
    [(list _ xs ...) xs]
    [(list) '()]))

(: push (∀ (a) ((Listof a) (Optional a) . -> . (Listof a))))
(define (push xs x)
  (match x
    [(some x) (cons x xs)]
    [_ xs]))

(struct (a) tape ([data : (Listof a)] [context : (Listof a)]))
(define-type Tape (∀ (a) (tape a)))

(define empty-tape (tape '() '()))

(: make-tape (∀ (a) (() #:rest a . ->* . (Tape a))))
(define (make-tape . xs)
  (tape xs '()))

(: tape-empty? (∀ (a) ((Tape a) . -> . Boolean)))
(define (tape-empty? xs)
  (equal? empty-tape xs))

(: tape-position (∀ (a) ((Tape a) . -> . Natural)))
(define (tape-position xs)
  (length (tape-context xs)))

(: tape-remaining (∀ (a) ((Tape a) . -> . Natural)))
(define (tape-remaining xs)
  (length (tape-data xs)))

(: tape-length (∀ (a) ((Tape a) . -> . Natural)))
(define (tape-length xs)
  (+ (tape-remaining xs) (tape-position xs)))

(: tape-front? (∀ (a) ((Tape a) . -> . Boolean)))
(define (tape-front? xs)
  (null? (tape-context xs)))

(: tape-back? (∀ (a) ((Tape a) . -> . Boolean)))
(define (tape-back? xs)
  (null? (tape-data xs)))

(: tape-read (∀ (a) ((Tape a) . -> . (Optional a))))
(define (tape-read xs)
  (head (tape-data xs)))

(: tape-insert (∀ (a) ((Tape a) a . -> . (Tape a))))
(define (tape-insert xs x)
  (tape (cons x (tape-data xs))
        (tape-context xs)))

(: tape-remove (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-remove xs)
  (tape (tail (tape-data xs))
        (tape-context xs)))

(: tape-write (∀ (a) ((Tape a) a . -> . (Tape a))))
(define (tape-write xs x)
  (tape-insert (tape-remove xs) x))

(: tape-fwd (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-fwd xs)
  (tape (tail (tape-data xs))
        (push (tape-context xs) (head (tape-data xs)))))

(: tape-bwd (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-bwd xs)
  (tape (push (tape-data xs) (head (tape-context xs)))
        (tail (tape-context xs))))

(: tape-move-by (∀ (a) ((Tape a) Integer . -> . (Tape a))))
(define (tape-move-by xs n)
  (cond
   [(> n 0) (tape-move-by (tape-fwd xs) (sub1 n))]
   [(< n 0) (tape-move-by (tape-bwd xs) (add1 n))]
   [(zero? n) xs]))

(: tape-move-to (∀ (a) ((Tape a) Integer . -> . (Tape a))))
(define (tape-move-to xs n)
  (tape-move-by xs (- n (tape-position xs))))

(: tape-front (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-front xs)
  (tape (rappend (tape-context xs) (tape-data xs)) '()))

(: tape-back (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-back xs)
  (tape '() (rappend (tape-data xs) (tape-context xs))))

(: tape-splice (∀ (a) ((Tape a) (Tape a) . -> . (Tape a))))
(define (tape-splice xs ys)
  (tape (append (tape-data xs) (tape-data ys))
        (append (tape-context xs) (tape-context ys))))

(: tape-map/f (∀ (a b) ((a . -> . b) (Tape a) . -> . (Tape b))))
(define (tape-map/f f xs)
  (tape (map f (tape-data xs)) (map f (tape-context xs))))



(module+ test
  (require typed/rackunit))

