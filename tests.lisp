
(defpackage "parzival-tests"
  (:use :cl :parzival :prove))

(in-package "parzival-tests")

(setf prove:*enable-colors* nil)

(defmacro test-with ((var input-string) &rest tests)
  `(subtest (format nil "With the input ~s ..." ,input-string)
     (let ((,var (make-instance 'replay-streams:static-text-replay-stream :text ,input-string)))
       ,@tests)))

(defmacro results (stream expr val)
  (let ((res (gensym))
        (ok? (gensym))
        (stream2 (gensym)))
    `(multiple-value-bind (,res ,ok? ,stream2) (parse ,stream ,expr)
       (is (and ,ok? ,res) ,val (format nil "Parsing with ~s results in ~s" ',expr ',val)))))

(defmacro fails (stream expr)
  (let ((res (gensym))
        (ok? (gensym))
        (stream2 (gensym)))
    `(multiple-value-bind (,res ,ok? ,stream2) (parse ,stream ,expr)
       (unless ,res ; doing this to get rid of warning about unused variable
         (is ,ok? nil (format nil "Parsing with ~s should fail." ',expr))))))

(test-with (input "hey")
           (results input (<<result t) t)
           (results input (<<result 'foo) 'foo))


(test-with (input "xxx")
           (results input <item< #\x)
           (results input <item< #\x)
           (results input <item< #\x)
           (fails input <item<))

(subtest "Testing <<plus"
  (test-with (input "hello")
             (results input (<<plus <nat< <word<) "hello")
             (results input <eof< t))

  (test-with (input "hello31")
             (results input (<<plus <word< <nat<) "hello")
             (results input (<<plus <word< <nat<) 31))

  (test-with (input "31hello")
             (results input (<<plus <nat< <word<) 31)
             (results input (<<plus <nat< <word<) "hello"))

  (test-with (input "hello31")
             (results input (<<plus <nat< <word<) "hello")
             (results input (<<plus <nat< <word<) 31))

  (test-with (input "abcd")
             (fails input (<<~ (<<string "abXd")))
             (results input (<<string "abcd") "abcd")
             (results input <eof< t)))

(subtest "Testing <<and and <<bind"
  (test-with (input "abcd1234")
             (results input (<<and <word< (<<ending <nat<)) 1234)))

(subtest "Testing <<*"
  (test-with (input "abcd1234")
             (results input (<<* (<<char #\z)) '())
             (fails input  (<<~ (<<ending <word<)))
             (results input <word< "abcd")
             (results input (<<ending (<<* <digit<)) '(#\1 #\2 #\3 #\4))))





