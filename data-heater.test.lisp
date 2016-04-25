#! /data/data12/zhengxin_pro/Heater/BIN/acl10/mlisp -#!
;; it's a script to run a test case against the data-heater module
;; Input: query-file timer-log-file output-file
;; contact: yuqiulin496
;; further information of data-heater module, contact David

(in-package #:cl-user)

(push :test *features*)

(load "load.lisp")

(in-package #:data-heater)

(setf *trace-output* (open (third (sys:command-line-arguments))  :direction :output :if-does-not-exist :create :if-exists :supersede))

(defun main (&rest args)
  (let ((args (cdr (sys:command-line-arguments))))
    (time
     (training (first args) ;input
	       *get-column-line-table-by-line*
	       *info-hash-table*
	       *predicates*
	       :output-exact (third args))) ;output
    (print "weka will complain JDBC .tar not found, but it's ok, time over")))

(main)
