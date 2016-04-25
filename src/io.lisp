;;;; io.lisp
(in-package :cl-user)
(defpackage #:data-heater.io
  (:use #:cl #:excl)
  (:export #:with-csv #:with-multiple-csv)
  (:documentation "This package contains several I/O operations functions or macros;
the reason why it's been separated to be a single package is that
we hope the efficiency of I/O, mainly reading and writing or printing CSV, will be improved in the future."))
(in-package #:data-heater.io)

(defmacro with-csv
    ((var filename &rest args &key (direction nil direction-p) (separator #\Tab) (skip-head nil) &allow-other-keys) &body body)
  (if* (or (not direction-p) (not (find direction '(:input :output))))
       :then (error "Please specify the direction, either :input or :output.")
       :else
       (unless (getf args :separator)
	 (setf (getf args :separator) separator))
       (unless (getf args :skip-head)
	 (setf (getf args :skip-head) skip-head))
       (let* ((separator-position (position :separator args))
	      (specs (append (subseq args 0 separator-position)
			     (subseq args (+ separator-position 2))))
	      (skip-head-position (position :skip-head specs))
	      (specs (append (subseq specs 0 skip-head-position)
			     (subseq specs (+ skip-head-position 2)))))
	 `(alexandria::with-open-file* (,var ,filename ,@specs)
	    (let ((fare-csv:*separator* ,separator))
		,(if skip-head
		     `(progn
			(fare-csv:read-csv-line ,var)
			,@body)
		     `(progn ,@body)))))))

(defmacro with-multiple-csv (bindings &body body)
  (if* (cdr bindings)
       :then
       `(with-csv ,(car bindings)
	  (with-multiple-csv ,(cdr bindings) ,@body))
       :else
       `(with-csv ,(car bindings) ,@body)))
