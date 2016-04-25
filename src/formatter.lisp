;;;; formatter.lisp
(in-package :cl-user)
(defpackage #:data-heater.formatter
  (:use #:cl #:excl)
  (:export #:*formatter*
	   #:set-formatter)
  (:documentation "This package exports a 'static' instance `*formatter*',
and a function `set-formatter'. Those APIs were designed to be as extensible as possible."))
(in-package #:data-heater.formatter)

(defclass data-formatter ()
  ((fn :allocation :class :initarg :formatter-fn :accessor formatter-fn :type function))
  (:documentation "Formatter object, formatting, cleaning and verifying the data.
`@fn' should be a function that takes an input string and returns exactly two values,
the first value means whether it's matched or legal against a certain rule,
while the second value represents a matched, cleaned or fixed(repaired) string.")
  (:metaclass mop:funcallable-standard-class))

(let ((fn-prefix "format-"))
  (defmethod set-formatter ((formatter data-formatter) property)
    (declare (type simple-string property))
    (let ((fn (symbol-function
	       (intern (concatenate 'string fn-prefix property) :data-heater.formatter))))
      (declare (type function fn))
      (setf (formatter-fn formatter) fn)
      (mop:set-funcallable-instance-function formatter fn)
      formatter)))

(defmethod initialize-instance :after ((this data-formatter) &key)
	   (mop:set-funcallable-instance-function this (formatter-fn this)))

(defparameter *formatter*
  (make-instance 'data-formatter
		 :formatter-fn (lambda (x)
				 (declare (ignorable x))
				 (warn "Formatting method has not been properly specified.")))
  "It will be exported and used like:
'(let ((formatter *formatter*))
   (set-formatter formatter \"taxid\")
   (funcall formatter ....))")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; formatter functions definitions ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar +TAXID-MULTIPLIER+
  (list 7 9 10 5 8 4 2 1 6 3 7 9 10 5 8 4 2))
(defvar +TAXID-CHECK-CODE+
  (alexandria:alist-hash-table
   (list (cons 0 "1") (cons 1 "0") (cons 2 "X")
	 (cons 3 "9") (cons 4 "8") (cons 5 "7")
	 (cons 6 "6") (cons 7 "5") (cons 8 "4")
	 (cons 9 "3") (cons 10 "2"))
   :size 11 :test '=))
(defun format-taxid (id)
  (declare (type simple-string id))
  (let ((len (length id))
	(zero (char-code #\0)))
    (flet ((sum-digits (digits)
	     (reduce '+
		   (map 'vector (lambda (x y) (* x (- (char-code y) zero)))
			+TAXID-MULTIPLIER+ digits))))
      (multiple-value-bind (matched-p matched)
	  (regexp:match-re "[1-9][0-9]{16}[0-9xX]|[1-9][0-9]{14}" id)
	(if* matched-p
	     :then
	     (cond ((= len 18)
		    ;; length of id is 18
		    (let ((sum (sum-digits (subseq matched 0 17))))
		      (values (string= (string (char matched 17))
				       (gethash (mod sum 11) +TAXID-CHECK-CODE+))
			      matched)))
		   ((= len 15)
		    ;; length of id is 15
		    (let* ((fixed (concatenate 'string (subseq matched 0 6) "19" (subseq matched 6)))
			   (sum (sum-digits fixed)))
		      (values t
			      (concatenate 'string fixed
					   (gethash (mod sum 11) +TAXID-CHECK-CODE+)))))
		   (t (values nil nil)))
	     :else
	     (values nil nil))))))

(defun format-cellphone (cellphone)
  (declare (type simple-string cellphone))
  (multiple-value-bind (matched-p matched)
      (regexp:match-re "(86)?\\-*(1[34578]\\d)\\-*(\\d{4})\\-*(\\d{4})" cellphone)
    (values matched-p matched)))

#+ignore
(defvar *email-scanner*
  (cl-ppcre:create-scanner "(?P<username>[a-z0-9\._\-]+)@{1,}(?P<domain>[a-z0-9\-\._]+)"))
