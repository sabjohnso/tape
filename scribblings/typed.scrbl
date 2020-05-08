#lang scribble/manual
@require[@for-label[tape/typed typed/racket/base]]
@title{Typed Tapes}

@defmodule[tape/typed]

@defform[(Tape a)]{
  A tape data structure}
  
@defproc[(tape? (x Any)) Boolean]

@defthing[empty-tape (List a)]


@defproc[(make-tape (x a) ...) (Tape a)]
@defproc[(build-tape (n Positive-Fixnum) (-> Index a)) (Tape a)]
@defproc[(list->tape (xs (Listof a))) (Tape a)]
@defproc[(tape->list (xs (Tape a))) (Listof a)]
@defproc[(tape-empty? (xs (Tape a))) Boolean]
@defproc[(tape-front? (xs (Tape a))) Boolean]
@defproc[(tape-back?  (xs (Tape a))) Boolean]
@defproc[(tape-position (xs (Tape a))) Index]
@defproc[(tape-remaining (xs (Tape a))) Index]
@defproc[(tape-length (xs (Tape a))) Index]
@defproc[(tape-read (xs (Tape a))) a]
@defproc[(tape-remove (xs (Tape a))) (Tape a)]
@defproc[(tape-insert (xs (Tape a)) (x a)) (Tape a)]
@defproc[(tape-write (xs (Tape a)) (x a)) (Tape a)]
@defproc[(tape-insert-optional (xs (Tape a)) (Optional a)) (Tape a)]
@defproc[(tape-write-optional (xs (Tape a)) (Optional a)) (Tape a)]
@defproc[(tape-fwd (xs (Tape a))) (Tape a)]
@defproc[(tape-bwd (xs (Tape a))) (Tape a)]
@defproc[(tape-move-by (xs (Tape a)) (n Positive-Fixnum)) (Tape a)]
@defproc[(tape-move-to (xs (Tape a)) (n Positive-Fixnum)) (Tape a)]
@defproc[(tape-front (xs (Tape a))) (Tape a)]
@defproc[(tape-back (xs (Tape a))) (Tape a)]
@defproc[(tape-drop (xs (Tape a))) (Tape a)]
@defproc[(tape-swap (xs (Tape a))) (Tape a)]
@defproc[(tape-dup (xs (Tape a))) (Tape a)]
@defproc[(tape-over (xs (Tape a))) (Tape a)]
@defproc[(tape-rot (xs (Tape a))) (Tape a)]
@defproc[(tape-nip (xs (Tape a))) (Tape a)]
@defproc[(tape-tuck (xs (Tape a))) (Tape a)]
@defproc[(tape-2drop (xs (Tape a))) (Tape a)]
@defproc[(tape-2swap (xs (Tape a))) (Tape a)]
@defproc[(tape-2dup (xs (Tape a))) (Tape a)]
@defproc[(tape-exec (xs (Tape a)) (f (-> (Tape a) (Tape a)))) (Tape a)]
@defproc[(tape-2exec (xs (Tape a)) (f (-> (Tape a) (Tape a) (Tape a)))) (Tape a)]
