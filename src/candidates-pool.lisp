;;;; candidates-pool.lisp
(in-package :cl-user)
(defpackage #:data-heater.candidates-pool
  (:use #:cl #:excl #:db.agraph #:prolog
	#:data-heater.config
	#:data-heater.cmd
	#:data-heater.distance
	#:data-heater.formatter
	#:data-heater.query
	#:data-heater.train)
  (:nicknames #:candidates-pool)
  (:export #:candidates-pool #:%candidates-pool% #:make-candiates-pool #:copy-candidates-pool
	   #:candidates #:candidates-list #:candidates-sorted-p #:candidates-score-table
	   #:count-of-candidates #:idno-matched #:idno-matched-is-candidate-p
	   #:candidates-pool-empty-p
	   #:dump-candidates-pool
	   #:candidates-score-equal
	   #:remove-candidate-if
	   #:with-candidates-pool
	   #:refresh-candidates-pool
	   #:parse-csv-line->pool
	   #:pool-suited-for-training
	   #:compute-candidates-score-table
	   #:sort-candidates-by-score
	   #:rewrite-candidates-pred-vals-list-exact-mode
	   #:rewrite-candidates-pred-vals-list-fuzzy-mode
	   #:compute-training-data-on-pool
	   #:candidates-pool-match-status)
  (:documentation "This package contains APIs for handling candidates pool, that is,
when reading a csv file's line which represents a person, we try to collect as much as possible candidates that
match the value of a very property(predicate)."))
(in-package #:data-heater.candidates-pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Global Constants and Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(alexandria:define-constant +empty-string+ ""
  :test 'string=
  :documentation "just empty string \"\"")
(alexandria:define-constant +label-no-match+ "NO-MATCH"
  :test 'string= :documentation "Label NO-MATCH")
(alexandria:define-constant +label-multiple-matches+ "MULTIPLE-MATCHES"
  :test 'string= :documentation "Label MULTIPLE-MATCHES")
(alexandria:define-constant +label-perfect-match+ "PERFECT-MATCH"
  :test 'string= :documentation "Label PERFECT-MATCH")
(alexandria:define-constant +label-match+ "MATCH"
  :test 'string= :documentation "Label MATCH")
(alexandria:define-constant +label-mismatch+ "MISMATCH"
  :test 'string= :documentation "Label MISMATCH")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Definitions and Basic APIs ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass candidates-pool ()
  ((candidates :initarg :candidates :accessor candidates :type hash-table)
   (candidates-score-table :accessor candidates-score-table :type hash-table)
   (candidates-list :initform nil :accessor candidates-list :type (or symbol list))
   (candidates-sorted-p :initform nil :accessor candidates-sorted-p :type symbol)
   (count-of-candidates :initform 0 :accessor count-of-candidates :type fixnum)
   (idno-matched :initform nil :accessor idno-matched :type (or person symbol))
   (idno-matched-is-candidate-p :initform nil :accessor idno-matched-is-candidate-p :type symbol)
   (empty-p :initform t :accessor candidates-pool-empty-p :type symbol))
  (:documentation "Candidates Pool Object;
`@candidates' is a hash-table that the key is a person while the value is a alist,
for which every the CAR is a predicate and the CDR is a list of the corresponding values;
`@candidates-score-table' is the score table of candidates, where each key is a person and the value is its score;
`@candidates-list' is the list of all the keys of `@candidates', and it's been constructed during `add-canddidate' step;
`@count-of-candidates' is the amount of candidates;
`@idno-matched' is the person that has exactly the `idno' predicate and the value is a legal taxid;
`@idno-matched-is-candidate-p' show whether the `@idno-matched' is a candidate;
`@empty-p' tells if it's a empty pool, which means there's no candidates;"))

(defun make-candidates-pool ()
  "Initialize a pool object."
  (make-instance 'candidates-pool
		 :candidates (make-upi-hash-table)))

(defvar %candidates-pool% (make-candidates-pool))

(defmethod print-object ((pool candidates-pool) stream)
  (flet ((helper (symb)
	   (declare (type symbol symb))
	   (the simple-string (if symb "YES" "NO"))))
    (let ((*print-length* 5))
      (with-slots
	    (candidates candidates-list candidates-sorted-p
			count-of-candidates empty-p idno-matched idno-matched-is-candidate-p) pool
	(print-unreadable-object (pool stream :type t :identity t)
	  (if* empty-p
	       :then (format stream "(EMPTY)")
	       :else
	       (format stream
		       "~A CANDIDATES IN THE POOL: ~A WHICH ~A SORTED,~%IDNO-MATCHED EXISTS? : ~A, IDNO-MATCHED IS A CANDIDATE? : ~A"
		       count-of-candidates
		       candidates-list
		       (if candidates-sorted-p "HAS BEEN" "HAS NOT BEEN")
		       (helper idno-matched)
		       (helper idno-matched-is-candidate-p))))))))

(defun refresh-candidates-pool (pool)
  "Clear the candidates pool, make it like it's been initilized again;
basically, resuing a hash-table over and over again can avoid a lot of consing."
  (declare (type candidates-pool pool))
  (with-slots (candidates candidates-score-table candidates-list
			  candidates-sorted-p count-of-candidates
			  idno-matched idno-matched-is-candidate-p empty-p) pool
    (progn
      (clrhash candidates)
      (when (slot-boundp pool 'candidates-score-table)
	(clrhash candidates-score-table))
      (setf idno-matched nil
	    idno-matched-is-candidate-p nil
	    empty-p t
	    count-of-candidates 0
	    candidates-list nil
	    candidates-sorted-p nil))
    t))

(defmethod dump-candidates-pool
    ((pool candidates-pool) (output hash-table)
     &rest args &key (index nil index-p) (copy t) &allow-other-keys)
  "The given hash-table should be integer indexed, which means the key is a index number and the value is the pool."
  (when (not index-p)
    (error "Please specify the index."))
  (setf (gethash index output)
	(if copy (copy-candidates-pool pool) pool))
  pool)

(defun copy-candidates-pool (pool)
  "Deep copy a candidates pool."
  (let ((new (make-candidates-pool)))
    (with-slots (candidates candidates-score-table candidates-list
			    candidates-sorted-p count-of-candidates idno-matched
			    idno-matched-is-candidate-p empty-p) pool
      (progn
	(maphash (lambda (x y)
		   (setf (gethash/upi x (slot-value new 'candidates)) y))
		 candidates)
	(when (slot-boundp pool 'candidates-score-table)
	  (setf (slot-value new 'candidates-score-table)
		(make-upi-hash-table))
	  (maphash (lambda (x y)
		     (setf (gethash/upi x (slot-value new 'candidates-score-table)) y))
		   candidates-score-table))
	(setf (slot-value new 'candidates-list) (copy-list candidates-list)
	      (slot-value new 'candidates-sorted-p) candidates-sorted-p
	      (slot-value new 'count-of-candidates) count-of-candidates
	      (slot-value new 'idno-matched) idno-matched
	      (slot-value new 'idno-matched-is-candidate-p) idno-matched-is-candidate-p
	      (slot-value new 'empty-p) empty-p))
      new)))

(declaim (inline person-exists-in-candidates-p))
(defun person-exists-in-candidates-p (pool person)
  "Check whether the person is a candidate."
  (declare (type candidates-pool pool)
	   (type person person))
  (nth-value 1 (gethash/upi person (slot-value pool 'candidates))))

(declaim (inline candidates-score-equal))
(defun candidates-score-equal (pool c1 c2)
  "Check whether two canddiates score the same."
  (declare (type candidates-pool pool)
	   (type person c1 c2))
  (with-slots (candidates-score-table) pool
    (= (gethash/upi c1 candidates-score-table)
       (gethash/upi c2 candidates-score-table))))

(defun add-candidate (pool person predicate value)
  "Add a candidate into the pool.
If the predicates is `idno', aka. `taxid', then the `@idno-matched' slot will be set to this person,"
  (declare (type candidates-pool pool)
	   (type simple-string predicate value)
	   (type person person))
  (block nil
    (with-slots (candidates candidates-list count-of-candidates idno-matched empty-p) pool
      (progn
	;; set pool not empty
	(when empty-p (setf empty-p nil))
	(if* (person-exists-in-candidates-p pool person)
	     :then ;; person already exists
	     (let* ((pred-vals-list (gethash/upi person candidates)) ; it's actually a proper list
		    (pred-vals (find predicate pred-vals-list :key 'car :test 'string=)))
	       (if* pred-vals
		    :then ;; there already exists `pred-vals'
		    (push value (cdr pred-vals))
		    :else
		    (push (list predicate value)
			  (gethash/upi person candidates))))
	     :else ;; person currently not in the candidates pool
	     (incf count-of-candidates) ;; don't forget to increment the count of candidates
	     (push person candidates-list) ;; add new member to the candidates list
	     (setf (gethash/upi person candidates)
		   (list (list predicate value))))))))

(defun decide-idno-matched (pool)
  "returns 1. whether the slot `@idno-matched' is not nil,
       and 2. whether `@idno-matched' is a candidate."
  (declare (type candidates-pool pool))
  (with-slots (candidates idno-matched idno-matched-is-candidate-p) pool
    (if idno-matched
	(setf idno-matched-is-candidate-p
	      (nth-value 1 (gethash/upi idno-matched candidates)))
	(setf idno-matched-is-candidate-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities on Candidates Pool ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An utility function should take a candidates pool as its first parameter,
;;; acting like it's a method,
;;; and it may modify those slots of that pool.
(defun remove-candidate-if
    (pool predicate &key (key #'identity) (destructive-p nil) keep-idno-matched)
  "Loop through all candidates, may be transformed by `@key',
removed if some certain function `@predicate' which accepts exactly one argument returns t or nil;
If `@destructive-p' is t, then just return the modified pool instance, otherwise return a copied one;
when `@keep-idno-matched' is t, then do not reset the `@idno-matched' slot."
  (declare (type candidates-pool pool))
  (with-slots (candidates candidates-list count-of-candidates candidates-sorted-p
			  candidates-score-table empty-p idno-matched idno-matched-is-candidate-p) pool
    (if* destructive-p
	 :then
	 (dolist (candidate candidates-list)
	   (cond ((and keep-idno-matched idno-matched (part= candidate idno-matched))
		  nil)
		 ((funcall predicate (funcall key candidate))
		  (remhash/upi candidate candidates)
		  (remhash/upi candidate candidates-score-table))))
	 (setf candidates-list (alexandria:hash-table-keys candidates))
	 (setf count-of-candidates (hash-table-count candidates))
	 (setf candidates-sorted-p nil)
	 (when (zerop count-of-candidates) (setf empty-p t))
	 (if* (and idno-matched (nth-value 1 (gethash/upi idno-matched candidates)))
	      :then (setf idno-matched-is-candidate-p t)
	      :else (setf idno-matched-is-candidate-p nil))
	 candidates-list
	 :else (remove-if predicate candidates-list :key key))))

(defun make-sort-candidates-predicate-by-score-table (candidates-score-table &key (order :desc))
  "Given a pool and the candidates-score-table,
this function generate the predicate function for `sort-candidates'."
  (declare (type hash-table candidates-score-table))
  (compile nil `(lambda (c1 c2)
                  (declare (type person c1 c2))
                  (funcall #',(case order
                                    (:desc '>)
                                    (:asce '<))
                           (gethash/upi c1 ,candidates-score-table)
                           (gethash/upi c2 ,candidates-score-table)))))

(defun sort-candidates (pool predicate &key (key #'identity))
  "Sort all candidates by a predicate function, where each candidate may be transformed by `key' funciton.
This fucntion is destructive, it modifies the `@candidates-list' and return the sorted new one."
  (declare (type candidates-pool pool))
  (with-slots (candidates-list candidates-sorted-p) pool
    (setf candidates-sorted-p t
	  candidates-list
	  (sort candidates-list predicate :key key))
    candidates-list))

(defun sort-candidates-by-score (pool)
  "Sort candidates by the score."
  (declare (type candidates-pool pool))
  (sort-candidates
   pool (make-sort-candidates-predicate-by-score-table
         (slot-value pool 'candidates-score-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; High-Level Functions and APIs ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-candidates-pool ((pool) &body body)
  "A utility macro helps reusing one candidates pool."
  `(let ((,pool %candidates-pool%))
     (unwind-protect 
	  (progn ,@body)
       (refresh-candidates-pool ,pool))))

(declaim (ftype (function (candidates-pool hash-table hash-table symbol) candidates-pool)))
(defun parse-csv-line->pool  (pool column-line-table info-hash-table mode)
  "`@pool': a candidates pool waiting to be processed;
`@column-line-table': a hash table maps from columns to fields of the `@line';
`@info-hash-table': a hash table returned by `process-command-line-arguments' from `data-heater.cmd' package,
which contains columns and predicates(properties) mapping relations;
`@mode' sepcifies when querying from creedo, whether we use :exact match or :fuzzy match."
  (declare (optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let ((formatter *formatter*))
    (declare (type candidates-pool pool))
    (with-slots (idno-matched) pool
      (with-hash-table-iterator (next info-hash-table)
        (loop
           (multiple-value-bind (not-done column predicate) (next)
             (declare (type simple-string column predicate))
             (unless not-done (return (decide-idno-matched pool)))
             (when not-done
               (let ((value (gethash column column-line-table)))
                 (declare (type simple-string value))
                 (unless (string= value +empty-string+)
                   ;; exact match
                   (if* (string= predicate "idno")
                        ;; this line contains `idno' field
                        :then
                        (set-formatter formatter "taxid")
                        (multiple-value-bind (matched-p fixed) (funcall formatter value)
                          (when matched-p
                            (setf idno-matched (intern-resource fixed :namespace "credoo"))
                            ))
                        :else
                        (let ((cursor (cond ((string= predicate "fullname")
                                             (query-by-predicate-and-object predicate value :exact))
                                            ((or (eq mode :exact) (eq mode :fuzzy))
                                             (query-by-predicate-and-object predicate value mode))
                                            (t (error "The mode should be either :exact or :fuzzy")))))
                          (iterate-cursor (triple cursor)
                            (add-candidate pool (subject triple) predicate value)))))))))))))

(declaim (inline poolsuited-for-training))
(defun pool-suited-for-training (pool)
  (and (not (candidates-pool-empty-p pool))
       (idno-matched-is-candidate-p pool)))

(defun compute-candidates-score-table (pool score-calculator)
  "Given a weights table, e.g. *default-weight* or *previous-work-weight*, and the mode (:default or :exact or :fuzzy),
compute each candidates' score, finally compute the `@canddidates-score-table' that
the key is the candidate(person), the value is its corresponding weight, and also sort the `@candidates'."
  (declare (type candidates-pool pool)
	   (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0)))
  ;; check whether the slot `@candidates-score-table' is bounded
  (unless (slot-boundp pool 'candidates-score-table)
    (setf (slot-value pool 'candidates-score-table)
	  (make-upi-hash-table)))
  (with-slots (candidates candidates-list candidates-score-table) pool
    (dolist (candidate candidates-list)
      (let* ((pred-vals-list (gethash/upi candidate candidates))
	     (parameters (make-score-calculator-parameters-list pred-vals-list))
	     (score (apply score-calculator parameters)))
	(setf (gethash/upi candidate candidates-score-table) score)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Section 1: Training ;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input: candidates-pool, column-line-table, info-hash-table,
;;; and default parameters: mode(:default),
;;;                         default-score-calculator(*previous-work-score-calculator*),
;;;                         distance-fn(#'levenschtein).
;;; Output: `fields', or say, list(s) for `fare-csv:write-csv-lines' to write to a csv file.
(defun rewrite-candidates-pred-vals-list-exact-mode (pool predicates)
  (declare (type candidates-pool pool)
           (type list predicates)
           (optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (with-slots (candidates candidates-list) pool
    (dolist (candidate candidates-list)
      (let* ((pred-vals-list (gethash/upi candidate candidates))
	     (preds-of-candidate (mapcar 'car pred-vals-list))
	     (new-pred-vals-list nil))
	(dolist (pred predicates)
	  (let* ((pred-in-candidate-p (find pred preds-of-candidate :test 'string=))
		 (value (if pred-in-candidate-p 1.0 0.0)))
	    (push (cons pred value) new-pred-vals-list)))
	(setf (gethash/upi candidate candidates)
	      (nreverse new-pred-vals-list))))
    pool))

(defun find-min-distance-pair (candidate credoo distance-fn)
  "Given two sequences, one represents values for a predicate from 'hive', while another represents 'credoo',
using `@distance-fn' to compute the distance for each pair, return the smallest distance pair."
  (declare (type (array simple-string (*)) candidate credoo)
	   (optimize speed (safety 1) (space 0) (debug 0)))
  (let ((min most-positive-fixnum)
	(result))
    (dolist (x candidate)
      (dolist (y credoo)
	(let ((distance (funcall distance-fn x y)))
	  (when (< distance min)
	    (setf result (list :candidate x :credoo y :distance distance)
		  min distance)))))
    (let* ((x (getf result :candidate)) (x-len (length x))
	   (y (getf result :credoo)) (y-len (length y))
	   (distance (getf result :distance)))
      (setf (getf result :distance) (- 1.0 (/ distance (+ x-len y-len))))
      result)))

(defun rewrite-candidates-pred-vals-list-fuzzy-mode (pool predicates distance-fn)
  (with-slots (candidates candidates-list count-of-candidates) pool
    (dolist (candidate candidates-list)
      (let* ((credoo (get-info candidate predicates))
             (candidate-pred-vals-list (gethash/upi candidate candidates))
             (new-pred-vals-list nil))
        (dolist (pred predicates)
          (let* ((pred-exists-in-candidate-p
                  (find pred candidate-pred-vals-list :test 'string= :key 'car))
                 (candidate-vals
                  (when pred-exists-in-candidate-p
                    (cdr (assoc pred candidate-pred-vals-list :test 'string=)))))
            (multiple-value-bind (credoo-vals credoo-vals-exists-p)
                (gethash pred credoo)
              (cond ((and candidate-vals credoo-vals-exists-p)
                     (let* ((min-distance-pair
                             (find-min-distance-pair candidate-vals credoo-vals distance-fn))
                            (distance (getf min-distance-pair :distance)))
                       (push (cons pred distance) new-pred-vals-list)))
                    (t (push (cons pred 0.0) new-pred-vals-list))))))
        (setf (gethash/upi candidate candidates)
              (nreverse new-pred-vals-list))
        pool))))

(defun compute-training-data-on-pool (pool score-calculator)
  (with-slots (candidates count-of-candidates idno-matched candidates-list candidates-score-table) pool
    ;; compute scores
    (compute-candidates-score-table pool score-calculator)
    ;; sort candidates by score, descend order
    (sort-candidates
     pool (make-sort-candidates-predicate-by-score-table candidates-score-table :order :desc))
    (when (and (cdr candidates-list) (cddr candidates-list))
      (list (cons (cons "Y" "yes") (gethash/upi idno-matched candidates))
	    (cons (cons "Y" "no")
		  (gethash/upi (find-if (complement (lambda (person)
						      (part= person idno-matched)))
					candidates-list)
			       candidates))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Section 2: Validating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun candidates-pool-match-status (pool)
  "Report the match status for a candidates pool, which should be non-empty, already rewritten and sorted."
  (with-slots (candidates-list count-of-candidates idno-matched idno-matched-is-candidate-p candidates-score-table) pool
    (cond ((and idno-matched-is-candidate-p (= count-of-candidates 1))
	   (values :single-match (first candidates-list) (gethash/upi (first candidates-list) candidates-score-table)))
	  ((and (null idno-matched-is-candidate-p) (= count-of-candidates 1)) (values :no-match nil nil))
	  (t (let ((1st (first candidates-list))
		   (2nd (second candidates-list)))
	       (cond ((candidates-score-equal pool 1st 2nd) (values :multiple-match 1st (gethash/upi 1st candidates-score-table)))
		     ((part= 1st idno-matched) (values :perfect-match 1st (gethash/upi 1st candidates-score-table)))
		     (t (values :mismatched 1st (gethash/upi 1st candidates-score-table)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Section 3: Reporting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+test
(progn
  (defparameter *chezhu.data* "/data/data12/zhengxin_pro/Heater/DATA/chezhu_info.data")
  (defparameter *yezhu.data* "/data/data12/zhengxin_pro/Heater/DATA/yezhu_info.data")
  (defparameter *output.tmp* "/data/data12/zhengxin_pro/gutianyu860/data_heater/local-projects/data-heater/data/output.tmp")
  (defparameter *cmd-args* (cons *chezhu.data* '("--fullname=fullname" "--cellphone=cellphone" "--idcard=idno" "--address=address")))
  (defparameter *info-hash-table* (nth-value 2 (process-command-line-args *cmd-args*)))
  (defparameter *columns* (nth-value 1 (process-command-line-args *cmd-args*)))
  (defun select-data-line (data index)
    (let ((fare-csv:*separator* #\Tab))
      (alexandria:with-input-from-file (in data)
	(let (result)
	  (dotimes (i (+ 1 index))
	    (setf result (fare-csv:read-csv-line in)))
	  result))))
  (defparameter *line* (select-data-line *chezhu.data* 299))  
  (defun count-csv-lines (file)
    (let ((fare-csv:*separator* #\Tab))
      (alexandria:with-input-from-file (in file)
	(loop with lino = 0 
	   for line = (read-line in nil nil) then (read-line in nil nil)
	   while line do (incf lino)
	   finally (return lino)))))
  (defun make-column-line-table-by-line (columns)
  (let ((ht (make-hash-table :test 'string=)))
    (lambda (line)
      (map nil (lambda (column value)
		 (setf (gethash column ht) value))
	   columns line)
      ht)))
  (defparameter *column-line-table* (funcall (make-column-line-table-by-line *columns*) *line*))
  (defparameter *predicates* (remove "idno"
				     (remove-duplicates (alexandria:hash-table-values *info-hash-table*) :test 'string=)
				     :test 'string=))
  )
