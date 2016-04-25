;;;; command-line.lisp
(in-package :cl-user)
(defpackage #:data-heater.cmd
  (:use :cl #:data-heater.config)
  (:export #:process-command-line-args)
  (:documentation "This package contains functions that process command line arguments."))
(in-package #:data-heater.cmd)

(let ((zero (char-code #\0))
      (nine (char-code #\9)))
  (defun str-is-number-p (str)
    "Test if every character is a number."
    (declare (type simple-string str))
    (flet ((char-is-number-p (char)
	     (<= zero (char-code char) nine)))
      (every #'char-is-number-p str))))

(declaim (inline split-argument-case-2))
(defun split-argument-case-2 (arg)
  (let* ((index-of-= (position #\= arg :test 'char=))
	 (column (subseq arg 2 index-of-=))
	 (predicate (subseq arg (+ 1 index-of-=))))
    (values column predicate)))

(declaim (inline predicate-not-found-warning column-not-found-warning))
(defun predicate-not-found-warning (predicate predicates)
  (let ((*print-length* 5))
    (warn "~a is not found in the current predicates set: ~A.~%"
	  predicate predicates)))
(defun column-not-found-warning (column columns)
  (let ((*print-length* 5))
    (warn "~a is not found in the current columns set: ~A.~%"
	  column columns)))

(declaim (inline predicate-exists-p column-exissts-p))
(defun predicate-exists-p (predicate predicates)
  (find predicate predicates :test 'string=))
(defun column-exists-p (column columns)
  (find column columns :test 'string=))

(defun verify-perdicate-and-column-existence
    (column columns predicate &optional (predicates *credoo-all-predicates-strings*))
  (progn
    (unless (predicate-exists-p predicate predicates)
      (predicate-not-found-warning predicate predicates))
    (unless (column-exists-p column columns)
      (column-not-found-warning column columns))))

(defun check-rest-args-syntax (args columns &key case (seperator #\Tab))
  (labels ((handle-case-3 (args count)
	     (if (null args)
		 t
		 (cond ((evenp count)
			(when (str-is-number-p (car args))
			  (handle-case-3 (cdr args) (+ count 1))))
		       (t ;; oddp count
			(progn
			  (let ((pred (car args)))
			    (unless (predicate-exists-p pred *credoo-all-predicates-strings*)
			      (predicate-not-found-warning pred *credoo-all-predicates-strings*))
			    (handle-case-3 (cdr args) (+ count 1)))))))))
    (cond ((= case 1)
	   (let ((arg-info (first args))
		 (arg-db (second args))
		 (arg-table (third args)))
	     (when (uiop:file-exists-p arg-info)
	       (let ((fare-csv:*separator* seperator))
		 (alexandria:with-input-from-file (in arg-info)
		   (loop for line = (fare-csv:read-csv-line in) then (fare-csv:read-csv-line in)
		      while line do
			(let ((db (first line))
			      (table (second line)))
			  (when (and (string= db arg-db) (string= table arg-table))
			    (let ((column (third line))
				  (predicate (fourth line)))
			      (verify-perdicate-and-column-existence column columns predicate))))))
		 t))))
	  ((= case 2)
	   (and (every (lambda (x) (alexandria:starts-with-subseq "--" x)) args)
		(every (lambda (x) (find #\= x :test 'char=)) args)
		(progn
		  (map nil (lambda (x)
			     (multiple-value-bind (column predicate) (split-argument-case-2 x)
			       (verify-perdicate-and-column-existence column columns predicate)))
		       args)
		  t)))
	  ((= case 3)
	   (let ((len (length args)))
	     (when (evenp len)
	       (handle-case-3 args 0)))))))

(defun fetch-columns-from-csv (pathname &optional (seperator #\Tab))
  (alexandria:with-input-from-file (in pathname)
    (let* ((fare-csv:*separator* seperator)
	   (line (fare-csv:read-csv-line in)))
      (mapcar (lambda (x)
		(if (find #\. x :test 'char=)
		    (let ((index (position #\. x :from-end t)))
		      (subseq x (+ index 1)))
		    x))
	      line))))

(defun process-command-line-args (args)
  (when args
    (let* ((input-csv (first args))
	   (rest-args (cdr args)) ;; don't count the first arg, which is the input filename
	   (2nd-arg (second args))
	   (columns (fetch-columns-from-csv input-csv))
	   (ht (make-hash-table :test 'string=)))
      (unless (uiop:file-exists-p input-csv)
	(error "Input CSV file: ~a not found.~%" input-csv))
      (cond
	;; case 3, e.g. 'filename 1 idno 2 fullname 3 cellphone'
	((str-is-number-p 2nd-arg)
	 (assert (check-rest-args-syntax rest-args columns :case 3))
	 (let ((indices (map 'list (lambda (x) (1- (parse-integer x)))
			     (remove-if (complement 'str-is-number-p) rest-args)))
	       (predicates (remove-if 'str-is-number-p rest-args))
	       (columns-vector (coerce columns 'vector)))
	   (map 'nil (lambda (index predicate)
		       (let ((column (elt columns-vector index)))
			 (setf (gethash column ht) predicate)))
		indices predicates)))
	;; case 2, e.g. 'filename --idno=idcardno --fullname=cust_name --cellphone=conta_name'
	((and (> (length 2nd-arg) 2) (alexandria:starts-with-subseq "--" 2nd-arg))
	 (assert (check-rest-args-syntax rest-args columns :case 2))
	 (map nil
	      (lambda (x)
		(multiple-value-bind (column predicate) (split-argument-case-2 x)
		  (setf (gethash column ht) predicate)))
	      rest-args))
	;; case 1, e.g. 'filename info db table'
	(t 
	 (assert (check-rest-args-syntax rest-args columns :case 1))
	 (let ((info 2nd-arg)
	       (db-arg (third args))
	       (table-arg (fourth args))
	       (fare-csv:*separator* #\Tab))
	   (alexandria:with-input-from-file (in info)
	     (loop for line = (fare-csv:read-csv-line in) then (fare-csv:read-csv-line in)
		while line do
		  (let ((db (first line))
			(table (second line)))
		    (when (and (string= db db-arg) (string= table table-arg))
		      (let ((column (third line))
			    (predicate (fourth line)))
			(setf (gethash column ht) predicate)))))))))
      (values input-csv columns ht))))
