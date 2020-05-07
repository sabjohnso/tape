#lang typed/racket

(require optional/typed)

(provide
 Tape tape?
 make-tape build-tape
 empty-tape tape-empty?
 tape-front? tape-back?
 tape-position tape-remaining tape-length
 tape-read tape-remove tape-insert tape-write
 tape-insert-optional tape-write-optional
 tape-fwd tape-bwd tape-move-by tape-move-to tape-front tape-back
 tape-drop tape-swap tape-dup tape-over tape-rot tape-nip tape-tuck
 tape-2drop tape-2swap tape-2dup
 tape-exec tape-2exec)

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

(: tape-equal? (∀ (a) ((Tape a) (Tape a) . -> . Boolean)))
(define (tape-equal? xs ys)
  (and (equal? (tape-data xs) (tape-data ys))
       (equal? (tape-context xs) (tape-context ys))))

(: tape-congruent? (∀ (a) ((Tape a) (Tape a) . -> . Boolean)))
(define (tape-congruent? xs ys)
  (or (tape-equal? xs ys)
      (equal? (tape->list xs)
              (tape->list ys))))

(define empty-tape (tape '() '()))

(: make-tape (∀ (a) (() #:rest a . ->* . (Tape a))))
(define (make-tape . xs)
  (tape xs '()))

(: build-tape (∀ (a) (Positive-Fixnum (Index . -> . a) . -> . (Tape a))))
(define (build-tape n proc)
  (tape (build-list n proc) '()))

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

(: tape-force-read (∀ (a) ((Tape a) a . -> . a)))
(define (tape-force-read xs  x-default)
  (match (tape-read xs)
    [(some x) x]
    [_ x-default]))

(: tape-insert (∀ (a) ((Tape a) a . -> . (Tape a))))
(define (tape-insert xs x)
  (tape (cons x (tape-data xs))
        (tape-context xs)))

(: tape-insert-optional (∀ (a) ((Tape a) (Optional a) . -> . (Tape a))))
(define (tape-insert-optional xs mx)
  (match mx
    [(some x) (tape-insert xs x)]
    [_ xs]))

(: tape-remove (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-remove xs)
  (tape (tail (tape-data xs))
        (tape-context xs)))

(: tape-write (∀ (a) ((Tape a) a . -> . (Tape a))))
(define (tape-write xs x)
  (tape-insert (tape-remove xs) x))

(: tape-write-optional (∀ (a) ((Tape a) (Optional a) . -> . (Tape a))))
(define (tape-write-optional xs mx)
  (match mx
    [(some x) (tape-insert (tape-remove xs) x)]
    [_ xs]))

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

(: tape-take-remaining (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-take-remaining xs)
  (tape (tape-data xs) '()))

(: tape-drop-remaining (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-drop-remaining xs)
  (tape '() (tape-context xs)))

(: tape-splice (∀ (a) ((Tape a) (Tape a) . -> . (Tape a))))
(define (tape-splice xs ys)
  (tape (append (tape-data xs) (tape-data ys))
        (append (tape-context xs) (tape-context ys))))

(: tape-append (∀ (a) ((Tape a) (Tape a) . -> . (Tape a))))
(define (tape-append xs ys)
  (tape (append (tape-data xs) (tape-data (tape-front ys)))
        (tape-context xs)))

(: tape-map/f (∀ (a b) ((a . -> . b) (Tape a) . -> . (Tape b))))
(define (tape-map/f f xs)
  (tape (map f (tape-data xs)) (map f (tape-context xs))))

(: tape-return (∀ (a) (a . -> . (Tape a))))
(define (tape-return x)
  (tape (list x) '()))

(: tape->list (∀ (a) ((Tape a) . -> . (Listof a))))
(define (tape->list xs)
  (tape-data (tape-front xs)))

(: list->tape (∀ (a) ((Listof a) . -> . (Tape a) )))
(define (list->tape xs)
  (tape xs '()))

(: tape->vector (∀ (a) ((Tape a) . -> . (Vectorof a))))
(define (tape->vector xs)
  (list->vector (tape->list xs)))

(: in-tape (∀ (a) ((Tape a) . -> . (Sequenceof a))))
(define (in-tape xs)
  (in-list (tape->list xs)))

(: tape-dup (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-dup xs)
  (tape-insert-optional xs (tape-read xs)))

(: tape-drop (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-drop xs)
  (tape-remove xs))

(: tape-swap (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-swap xs)
  (let ([ys (tape-fwd xs)])
    (tape-insert-optional (tape-bwd (tape-remove ys)) (tape-read ys))))

(: tape-rot (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-rot xs)
  (tape-insert-optional (tape-move-by (tape-remove (tape-move-by xs 2)) -2)
               (tape-read (tape-move-by xs 2))))

(: tape-over (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-over xs)
  (tape-insert-optional xs (tape-read (tape-fwd xs))))

(: tape-nip (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-nip xs)
  (tape-bwd (tape-remove (tape-fwd xs))))

(: tape-tuck (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-tuck xs)
  (tape-move-by (tape-insert-optional (tape-move-by xs 2) (tape-read xs)) -3))

(: tape-2dup (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-2dup xs)
  (tape-insert-optional  (tape-insert-optional xs (tape-read (tape-drop xs))) (tape-read xs)))

(: tape-2drop (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-2drop xs)
  (tape-drop (tape-drop xs)))

(: tape-3drop (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-3drop xs)
  (tape-drop (tape-2drop xs)))

(: tape-2swap (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-2swap xs)
  (tape-insert-optional
   (tape-insert-optional
    (tape-move-by (tape-2drop (tape-move-by xs 2)) -2)
    (tape-read (tape-3drop xs)))
   (tape-read (tape-2drop xs))))

(: tape-2over (∀ (a) ((Tape a) . -> . (Tape a))))
(define (tape-2over xs)
  (tape-insert-optional
   (tape-insert-optional xs (tape-read (tape-3drop xs)))
   (tape-read (tape-2drop xs))))

(: tape-exec (∀ (a) ((a . -> . a) (Tape a) . -> . (Tape a))))
(define (tape-exec f xs)
  (tape-write-optional xs (optional-map/f f (tape-read xs))))

(: tape-2exec (∀ (a) ((a a . -> . a) (Tape a) . -> . (Tape a))))
(define (tape-2exec f xs)
  (tape-insert-optional (tape-2drop xs)
   (let/m-optional ([x : a (tape-read xs)]
                    [y : a (tape-read (tape-drop xs))])
                   (optional-return (f x y)))))


(module+ test
  (require typed/rackunit)

  (check-true
   (tape-equal?
    (list->tape '(a b))
    (list->tape '(a b))))

  (check-true
   (tape-empty? empty-tape))

  (check-true (tape-empty? empty-tape))

  (check-true
   (tape-equal?
    (list->tape '(a b c d))
    (make-tape 'a 'b 'c 'd)))

  (let ([xs (list->tape '(a b c d))])
    (check-equal?
     (tape->list (build-tape 4 (λ ([index : Index]) (tape-force-read (tape-move-to xs index) 'a))))
     (tape->list xs))
    (check-equal? (tape->list (tape-drop xs)) '(b c d))
    (check-equal? (tape->list (tape-nip xs))  '(a c d))
    (check-equal? (tape->list (tape-swap xs)) '(b a c d))
    (check-equal? (tape->list (tape-rot xs))  '(c a b d))
    (check-equal? (tape->list (tape-over xs)) '(b a b c d))
    (check-equal? (tape->list (tape-tuck xs)) '(a b a c d))

    (check-equal? (tape->list (tape-2drop xs)) '(c d))
    (check-equal? (tape->list (tape-2swap xs)) '(c d a b))
    (check-equal? (tape->list (tape-2over xs)) '(c d a b c d)))

  (let ([xs (list->tape '(1 2 3 4 5))])
    (check-equal? (tape->list (tape-2drop xs))
                  '(3 4 5))
    (check-equal?
     (tape->list (tape-exec (λ ([n : Number]) (+ n n)) xs))
     '(2 2 3 4 5))

    (check-equal?
     (tape->list (tape-2exec - xs))
     '(-1 3 4 5))))
