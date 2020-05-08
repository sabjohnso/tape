#lang scribble/manual
@require[@for-label[tape tape/typed racket/base]]

@title{Tape}
@author{@author+email["Samuel B. Johnson" "sabjohnso.dev@gmail.com"]}

A tape data structure (A.K.A a zipper).

@table-of-contents[]

@defmodule[tape]

@defmodule[tape/typed]
 Tape
 tape?
 make-tape
 build-tape
 list->tape
 tape->list
 empty-tape
 tape-empty?
 tape-front?
 tape-back?
 tape-position
 tape-remaining
 tape-length
 tape-read
 tape-remove
 tape-insert
 tape-write
 tape-insert-optional
 tape-write-optional
 tape-fwd tape-bwd tape-move-by tape-move-to tape-front tape-back

@defproc[tape-drop
tape-swap 
tape-dup tape-over tape-rot tape-nip tape-tuck
 tape-2drop tape-2swap tape-2dup
  tape-exec 
  tape-2exec



