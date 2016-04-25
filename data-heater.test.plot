#! /data/data12/zhengxin_pro/Heater/BIN/acl10/mlisp -#!

(require :asdf)
(require :util-string)
#|
Fri Apr 22 15:23:39 2016: 10000 Lines of people
; cpu time (non-gc) 12.424111 sec user, 0.794878 sec system
; cpu time (gc)     1.117830 sec user, 0.004000 sec system
; cpu time (total)  13.541941 sec user, 0.798878 sec system
; real time  12.067763 sec (118.8%)
; space allocation:
; 29,196,228 cons cells, 671,052,784 other bytes, 0 static bytes")
; Page Faults: major: 0 (gc: 632), minor: 229254 (gc: 632)
|#

(in-package #:user)
(use-package :util.string)

(defvar *current-pathname*
  (uiop:pathname-directory-pathname (uiop:current-lisp-file-pathname)))

(defparameter *labels*
  '(count cpuTime_no_gc cpuTime_gc cpuTime_total realTime cons_cells otherBytes))

(defun fetch-count (line)
  (let* ((colon (position #\: line :test 'char= :from-end t))
         (substr (subseq line (+ colon 2)))
         (space (position #\Space substr :test 'char=)))
    (subseq substr 0 space)))

(defun fetch-time (line &optional (index 3))
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\; #\Space)
    (set-syntax-from-char #\, #\Space)
    (with-input-from-string (in line)
      (loop for i from 0 to index
         with res = nil
         do (setf res (read in nil nil))
         finally (return (string+ (write-to-string res)))))))

(defun fetch-space (line)
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\; #\Space)
    (set-syntax-from-char #\, #\Space)
    (let* ((lst (with-input-from-string (in line)
                  (loop for token = (read in nil nil)
                     while token collect token)))
           (cons-index (position 'cons lst))
           (other-index (position 'other lst)))
      (values (format nil "~{~A~^,~}" (subseq lst 0 cons-index))
              (format nil "~{~A~^,~}" (subseq lst (+ cons-index 2) other-index))))))
      
(defun fetch-info-on-file (filename)
  (with-open-file (in filename)
    (loop with res = nil
       for index = 0 then (+ index 1)
       for line = (read-line in nil nil)
       while line do (cond ((= index 0) (push (cons 'count (fetch-count line)) res))
                           ((= index 1) (push (cons 'cpuTime_no_gc (fetch-time line)) res))
                           ((= index 2) (push (cons 'cpuTime_gc (fetch-time line)) res))
                           ((= index 3) (push (cons 'cpuTime_total (fetch-time line)) res))
                           ((= index 4) (push (cons 'realTime (fetch-time line 2)) res))
                           ((= index 6) (multiple-value-bind (cons other) (fetch-space line)
                                          (push (cons 'cons_cells cons) res)
                                          (push (cons 'otherBytes other) res))))
       finally (return res))))

(defun write-csv-line (lst stream)
  (write-sequence
   (format nil "~{~A~^    ~}~%" lst) stream))

(defun compare-string-number (str1 str2)
  (< (parse-integer str1) (parse-integer str2)))

(defun generate-stats
    (&optional (directory (merge-pathnames "data/data-heater.test/" *current-pathname*)))
  (with-open-file (out (merge-pathnames "data/data-heater.test/plot.csv" *current-pathname*)
                       :direction :output
                       :if-does-not-exist :create :if-exists :supersede)
    (write-csv-line *labels* out)
    (let ((values-list nil))
      (dolist (file (uiop:directory-files directory))
        (let ((values (fetch-info-on-file file)))
          (when values (push values values-list))))
      (print values-list)
      (setf values-list (sort values-list 'compare-string-number
                              :key (lambda (x) (cdr (assoc 'count x)))))
      (dolist (values values-list)
        (write-csv-line (mapcar (lambda (x) (cdr (assoc x values))) *labels*) out)))))
                                  
(defun main (&rest args)
  (declare (ignore args))
  (generate-stats))

(main)
