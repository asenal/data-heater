;;;; query.lisp
(in-package :cl-user)
(defpackage #:data-heater.query
  (:use #:cl #:excl #:db.agraph #:prolog
	#:data-heater.config)
  (:export #:query-by-predicate-and-object
	   #:get-info)
  (:documentation "This packages contains functions that interface with AllegroGraph."))
(in-package #:data-heater.query)

(defparameter *fuzzy-match-distances-table*
  (alexandria:alist-hash-table
   (list (cons "fullname" 0) ;; will never be used, actually
	 (cons "address" 3)
	 (cons "cellphone" 1) (cons "email" 5)
	 (cons "worksFor" 5))
   :test 'string=)
  "Each key is a freetext index,
and its corresponding value is the distance used for fuzzy matching.")

(defun query-by-predicate-and-object (predicate object mode)
  (declare (type symbol mode)
	   (type simple-string predicate object))
  (cond ((eq mode :exact)
	 (get-triples :p (gethash predicate *credoo-all-predicates-resources-table*)
		      :o (literal object :datatype !xsd:string)))
	((eq mode :fuzzy)
	 (freetext-get-triples `(fuzzy ,object ,(gethash predicate *fuzzy-match-distances-table*))
			       :index predicate))))

#+ignore
(defun get-info (person)
  "Given a person, return a hash table that
contains all information we could query from Credoo."
  (declaim (optimize speed (safety 1) (space 0) (debug 0)))
  (let* ((cursor (get-triples :s person))
	 (ht1 (make-hash-table :test 'string=)))
    ;;(ht2 (make-hash-table)))
    (declare (type db.agraph.http.store::http-triple-cursor cursor)
	     (type hash-table ht1))
    (iterate-cursor (triple cursor)
      (let* ((predicate (part->terse (predicate triple)))
	     (value (part->terse (object triple))))
	;;(graph (graph triple)))
	(declare (type simple-string predicate value))
	(progn
	  (unless (nth-value 1 (gethash predicate ht1))
	    (setf (gethash predicate ht1)
		  (make-array '(1) :adjustable t :fill-pointer 0)))
	  (unless (find value (gethash predicate ht1) :test 'string=)
	    (vector-push-extend value (gethash predicate ht1))))))
    (values ht1)))

(let ((ht (make-hash-table :test 'string=)))
  (defun get-info (person predicates)
    "Given a `@person' and the `@info-hash-table', return a hash table that
contains all information we could query from Credoo."
    (declaim (type person person)
	     (type hash-table info-hash-table)
	     (optimize speed (safety 0) (space 0) (debug 0)))
    (clrhash ht)
    (dolist (pred predicates)
      (declare (type simple-string pred))
      (let ((cursor (get-triples :s person :p (gethash pred *credoo-all-predicates-resources-table*))))
	(iterate-cursor (triple cursor)
	  ;; consider about the graph information ... maybe later ...
	  (let ((value (part->terse (object triple))))
	    (push value (gethash pred ht))))))
    (if (zerop (hash-table-count ht)) nil ht)))
