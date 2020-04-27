(defpackage :org-toc
  (:use :cl :parzival))

(in-package :org-toc)

(<<def <toc-line< 
       (<<bind (<<counting+ (<<char #\*))
               (lambda (count) 
                 (<<map (lambda (matched) 
                          (let ((toc-entry (concatenate 'string matched)))
                            (format nil "~a- [[~a][~a]]" 
                                    (make-string (* 2 count) :initial-element #\Space)
                                    toc-entry 
                                    toc-entry)))
                        (<<and <whitespace<+ (<<+ <item<)))))
       "Accepts a valid Org Mode heading and results in an org mode
       list item with a link to that heading.")

(defun print-toc-for (file &optional (output *standard-output*))
  "Prints a table of contents for a given org file to the supplied stream."
  (with-open-file (input file)
    (loop
       :for line = (read-line input nil nil)
       :while line
       :do (multiple-value-bind (entry success-p) (parse line <toc-line< t)
             (when success-p  (format output "~a~%" entry))))))
