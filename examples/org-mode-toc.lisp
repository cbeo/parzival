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
                        (<<and <whitespace+< (<<+ <item<)))))
       "Accepts a valid Org Mode heading and results in an org mode
       list item with a link to that heading.")

(<<def <gitnub-toc-line<
       (labels ((make-href-text (chars)
                  (string-downcase 
                   (concatenate 'string (loop :for char :in chars
                                           :when (alphanumericp char)
                                           :collect char
                                           :when (eql #\Space char)
                                           :collect #\-))))
                (format-entry (depth)
                  (lambda (matched)
                    (let ((anchor-text (concatenate 'string matched))
                          (href-text (make-href-text matched))
                          (indent (make-string (* 2 (1- depth)) :initial-element #\Space)))
                      (format nil "~a- [[#~a][~a]]"
                              indent
                              href-text
                              anchor-text)))))
         (<<bind (<<counting+ (<<char #\*))
                 (lambda (depth)
                   (<<map (format-entry depth)
                          (<<and <whitespace+< (<<+ <item<)))))))

(defun print-toc-for (file &key (output *standard-output*) for-github)
  "Prints a table of contents for a given org file to the supplied stream."
  (with-open-file (input file)
    (loop
       :for line = (read-line input nil nil)
       :while line
       :do (multiple-value-bind (entry success-p)
               (parse line
                      (if for-github <gitnub-toc-line< <toc-line<)
                      t)
             (when success-p  (format output "~a~%" entry))))))
