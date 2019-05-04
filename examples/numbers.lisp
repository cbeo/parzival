
(defpackage "parzival-numbers"
  (:use :cl :parzival))

(in-package "parzival-numbers")

(defun <<map-to (parser value)
  (<<map (lambda (x) value) parser))

(defvar <ones<
  (<<or (<<map-to (<<string "one") 1)
        (<<map-to (<<string "two") 2)
        (<<map-to (<<string "three") 3)
        (<<map-to (<<string "four") 4)
        (<<map-to (<<string "five") 5)
        (<<map-to (<<string "six") 6)
        (<<map-to (<<string "seven") 7)
        (<<map-to (<<string "eight") 8)
        (<<map-to (<<string "nine") 9)))

(defvar <teens<
  (<<or (<<map-to (<<string "ten") 10)
        (<<map-to (<<string "eleven") 11)
        (<<map-to (<<string "twelve") 12)
        (<<map-to (<<string "thirteen") 13)
        (<<map-to (<<string "fourteen") 14)
        (<<map-to (<<string "fifteen") 15)
        (<<map-to (<<string "sixteen") 16)
        (<<map-to (<<string "seventeen") 17)
        (<<map-to (<<string "eighteen") 18)
        (<<map-to (<<string "nineteen") 19)))

(defvar <tens<
  (<<or (<<map-to (<<string "twenty") 20)
        (<<map-to (<<string "thirty") 30)
        (<<map-to (<<string "forty") 40)
        (<<map-to (<<string "fifty") 50)
        (<<map-to (<<string "sixty") 60)
        (<<map-to (<<string "seventy") 70)
        (<<map-to (<<string "eighty") 80)
        (<<map-to (<<string "ninety") 90)))

(defvar <20-to-99<
  (<<bind <tens<
          (lambda (tens)
            (<<map (lambda (ones) (+ tens ones))
                   (<<and (<<char #\-) <ones<)))))

(defvar <1-to-99<
  (<<or <20-to-99< <tens< <teens< <ones<))


(defvar <one-hundreds<
  (<<bind <ones<
          (lambda (num)
            (<<map (lambda (ignore) (* num 100))
                   (<<and (<<+ <space<) (<<string "hundred"))))))

(defvar <in-hundreds<
  (<<bind <one-hundreds<
          (lambda (hundreds)
            (<<map (lambda (num) (+ hundreds num))
                   (<<and (<<+ <space<) <1-to-99<)))))

(defvar <all-hundreds<
  (<<plus <in-hundreds< <one-hundreds<))


(defun <<magnitude-order (name factor)
  (<<bind (<<or <all-hundreds< <1-to-99<)
          (lambda (val)
            (<<map (lambda (ignore) (* val factor))
                   (<<and (<<+ <space<) (<<string name))))))

(defvar <thousands< (<<magnitude-order "thousand" 1000))


(defvar <millions< (<<magnitude-order "million" 1000000))

(defvar <billions< (<<magnitude-order "billion" 1000000000))

(defvar <trillions< (<<magnitude-order "trillion" 1000000000000))

(defvar <quadrillions< (<<magnitude-order "quadrillion" 1000000000000000))

(defvar <number<
  (<<map (lambda (ls) (apply #'+ ls))
         (apply #'parzival::<<list
                (mapcar (lambda (p) (<<or (<<strip p) (<<result 0)))
                        (list <quadrillions< <trillions< <billions<
                              <millions< <thousands<
                              <all-hundreds< <1-to-99<)))))


(defun parse-number (str)
  (parse str <number< t))



;; three plus forty-seven thousand plus two hundred million sixty-five
(defun <calc< (stream)
  (funcall <calc< stream))

(defvar <op< (<<strip (<<or (<<string "plus")
                            (<<string "minus"))))

(defvar <calc<
  (<<plus
   (<<bind <number<
           (lambda (number)
             (<<map (lambda (op-calc)
                      (if (equal (car op-calc) "plus")
                          (+ number (cdr op-calc))
                          (- number (cdr op-calc))))
                    (<<cons <op< #'<calc<))))
   <number<))


(defun natural-language-calc ()
  (loop named goof-calc
        for line = (read-line)
        do
        (if (equal line "quit")
            (return-from goof-calc "OK")
            (let ((parsed (parse line <calc< t)))
              (if parsed
                  (format t "EQUALS ~R~%> " parsed)
                  (format t "No no no.. all wrong...~%> "))))))
