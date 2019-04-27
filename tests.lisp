
(defpackage "parzival-tests"
  (:use :cl :parzival :prove))

(in-package "parzival-tests")

(setf prove:*enable-colors* nil)

(subtest "Testing <<result"
  (ok (parse "" (<<result t) t))
  (let* ((stream (make-string-input-stream "hey")))
    (is (parse stream (<<result 'foo)) 'foo)
    (is (read-char stream) #\h)))

(subtest "Testing <peek<"
  (let ((stream (make-string-input-stream "X")))
    ;; showing that the input isn't being consumed
    (is (parse stream <peek<) #\X)
    (is (parse stream <peek<) #\X)))

(subtest "Testing <item<"
  (is (parse "xxx" <item< t) #\x)
  (isnt (parse "" <item< t) t))

(subtest "Testing <eof<"
  (ok (parse "" <eof< t))
  (isnt (parse "MORE CONTENT" <eof< t) t)

  (let ((stream (make-string-input-stream "abc")))
    ;; read all the input then test that we're at the end
    (read-char stream)
    (read-char stream)
    (read-char stream)
    (ok (parse stream <eof<))))

(subtest "Basic <<plus tests"
  (is (parse "hello" (<<plus <nat< <word<) t) "hello")
  (is (parse "31hello" (<<plus <nat< <word<) t) 31)
  (is (parse "33" (<<or <word< <nat< <int<) t) 33)
  (is (parse "-33" (<<or <word< <nat< <int<) t) -33)
  (let ((stream1 (make-string-input-stream "abcd")))
    (multiple-value-bind (res ok? stream2) (parse stream1 (<<~ (<<string "abXd")))
      (is res nil)            ; The result is NIL, but b/c the parse failed.
      (is ok? nil)            ; See, the parse should have failed.
      (isnt stream1 stream2)  ; The streams should now be different objects in memory.
      ;; Notice that even though "abXd" could have consumed all our input, we're not at the end.
      (isnt (parse stream2 <eof<) t)
      ;; But we should still be able to parse "abcd" from STREAM2.
      (is (parse stream2 (<<string "abcd")) "abcd")
      ;; Moreover, we should be at the end of the input.
      (is (parse stream2 <eof<) t))))

