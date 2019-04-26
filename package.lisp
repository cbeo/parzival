;;;; package.lisp

(defpackage #:parzival
  (:use #:cl #:replay-streams)
  (:export
   #:<<result
   #:<fail<
   #:<item<
   #:<~item<
   #:<eof<
   #:<<if
   #:<<when
   #:<<plus
   #:<<or
   #:<<~
   #:<<?
   #:<<~def
   #:<<bind
   #:<<and-then
   #:<<and
   #:<<sat
   #:<<~sat
   #:<<char
   #:<<~char
   #:<<char-equal
   #:<<~char-equal
   #:<uppercase<
   #:<~uppercase<
   #:<lowercase<
   #:<~lowercase<
   #:<alphanum<
   #:<~alphanum<
   #:<space<
   #:<~space<
   #:<newline<
   #:<~newline<
   #:<digit<
   #:<~digit<
   #:<<map
   #:<<map-cons
   #:<<map-cons?
   #:<<cons
   #:<<*
   #:<<+
   #:<<times
   #:<<min-times
   #:<<max-times
   #:<<sep-by
   #:<<string
   #:<<~string
   #:<<to-string
   #:<word<
   #:<~word<
   #:<nat<
   #:<~nat<
   #:<int<
   #:<~int<
   ))


