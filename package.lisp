;;;; package.lisp

(defpackage #:parzival
  (:use #:cl #:replay-streams)
  (:export
   #:parse
   #:<<result
   #:<fail<
   #:<peek<
   #:<item<
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
   #:<<ending
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
   #:<letter<
   #:<~letter<
   #:<space<
   #:<~space<
   #:<newline<
   #:<~newline<
   #:<digit<
   #:<~digit<
   #:<<map
   #:<<map-cons
   #:<<map-cons?
   #:<<cons-map
   #:<<list-map
   #:<<map-list
   #:<<map-append
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


