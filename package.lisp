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
   #:<<asat
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
   #:<tab<
   #:<~tab<
   #:<return<
   #:<~return<
   #:<linefeed<
   #:<~linefeed<
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
   #:<<brackets
   #:<<string
   #:<<~string
   #:<<to-string
   #:<word<
   #:<nat<
   #:<int<
   #:<real<
   #:<whitespace<
   #:<<strip
   ))


 
