;;;; data-heater.lisp
(in-package :cl-user)
(defpackage #:data-heater
  (:use #:cl #:excl #:db.agraph
	#:data-heater.config
	#:data-heater.cmd
	#:data-heater.distance
	#:data-heater.formatter
	#:data-heater.train
	#:data-heater.io
	#:data-heater.candidates-pool)
  (:export #:main))
(in-package #:data-heater)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Toplevel Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (vom:config t :info))

(defparameter *backup-candidates*
  (make-upi-hash-table) "A backup candidates hash table.")

(defun backup-candidates (candidates)
  "Given a candidates hash table, copy key-value pairs to `*backup-candidates*'."
  (with-hash-table-iterator (next-candidate candidates)
    (loop
       (multiple-value-bind (not-done k v) (next-candidate)
	 (if not-done
	     (setf (gethash k *backup-candidates*) v)
	     (return nil))))))

(defparameter *lines-with-idno-indices* nil
  "A list of indices that records which line has a non-empty `idno' property.")

(defvar *exact-candidates-pools-with-idno* (make-hash-table :test '=)
  "A hash table that each key is the index of a line,
and the value is the corresponding canddiates pool which is geenrated in :exact mode.")

(defvar *fuzzy-candidates-pools-with-idno* (make-hash-table :test '=)
    "A hash table that each key is the index of a line,
and the value is the corresponding canddiates pool which is geenrated in :fuzzy mode.")

(declaim (inline reset-global-definitions))
(defun reset-global-definitions ()
  "Make sure those global variables are cleaned."
  (setf *lines-with-idno-indices* nil)
  (clrhash *exact-candidates-pools-with-idno*)
  (clrhash *fuzzy-candidates-pools-with-idno*))

(defun main (args)
  (multiple-value-bind (input-data columns info-hash-table)
      (process-command-line-args args)
    (let* ((get-column-line-table-by-line (%make-column-line-table-by-line% columns))
	   (predicates
	    (delete-duplicates (alexandria:hash-table-values info-hash-table) :test 'string=))
	   (*print-length*  (length predicates)))
      (alexandria:with-output-to-file (log (make-output-filename input-data :type :log)
					   :if-does-not-exist :create :if-exists :supersede
					   :external-format :utf-8)
	(let ((vom:*log-stream* log))
	  (if* (find "idno" predicates :test 'string=)
	       :then
	       (multiple-value-bind (exact-score-calculator fuzzy-score-calculator)
		   (training input-data get-column-line-table-by-line info-hash-table predicates)
		 (validating input-data predicates exact-score-calculator fuzzy-score-calculator)
		 (matching input-data get-column-line-table-by-line info-hash-table predicates exact-score-calculator fuzzy-score-calculator))
	       :else nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((ht (make-hash-table :test 'string=)))
  (defun %make-column-line-table-by-line% (columns)
    "Given a list of columns names, return a function accepts a list of values from a line,
reuse(for the concern of efficiency) a hash table to map the columns names to the corresponding values."
    (lambda (line)
      (mapc (lambda (column value)
	      (setf (gethash column ht) value))
	    columns line)
      ht)))

(defun line-has-valid-idno-p (column-line-table info-hash-table)
  "Check whther a line has a non-empty AND legal `idno' property."
  (with-hash-table-iterator (next-info info-hash-table)
    (loop
       (multiple-value-bind (not-done col pred) (next-info)
	 (unless not-done
	   (return-from line-has-valid-idno-p nil))
	 (when (string= pred "idno")
	   (return-from line-has-valid-idno-p
	     (let ((*formatter* *formatter*)
		   (value (gethash col column-line-table)))
	       (set-formatter *formatter* "taxid")
	       (and (string/= value "")
		    (funcall *formatter* value)))))))))

(defun read-csv-line (stream)
  (ignore-errors (fare-csv:read-csv-line stream)))

(declaim (inline write-csv-head))
(defun write-csv-head-line (stream head-line)
  "Write the information line to a output stream."
  (declare (type list head-line))
  (fare-csv:write-csv-line head-line stream))

(defun find-column-by-predicate (predicate info-hash-table)
  "Given an info-hash-table and a predicate, find the key, that is, a column name of it."
  (declare (type simple-string predicate)
	   (type hash-table info-hash-table))
  (with-hash-table-iterator (next-info info-hash-table)
    (loop
       (multiple-value-bind (not-done col pred) (next-info)
	 (declare (ignore not-done))
	 (when (string= pred predicate)
	   (return col))))))

(declaim (inline make-output-filename))
(defun make-output-filename (input-data &key type)
  "During the 3 sections, training, validating and matching,
this function decides the output filename for each section."
  (let* ((name (pathname-name input-data))
         (surffix-p (pathname-type input-data))
         (surffix (if surffix-p surffix-p "csv")))
    (merge-pathnames (util.string:string+
			"data/" name "." (symbol-name type) "." surffix)
                     *root-path*)))

(declaim (inline data-heater.log))
(defun data-heater.log (message &optional (stream *standard-output*))
  (write-sequence (util.string:string+ #\[ (excl.osi:ctime) "]: " message #\Newline) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Features (3 Steps) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun training
    (input-data get-column-line-table-by-line info-hash-table predicates
     &key (output-exact (make-output-filename input-data :type :exact-training))
       (output-fuzzy (make-output-filename input-data :type :fuzzy-training))
       (default-score-calculator *previous-work-score-calculator*) (distance-fn #'levenschtein) (limit most-positive-fixnum))
  (reset-global-definitions) ;; make sure these global variables are cleaned.
  (let (;; Y-and-predicates represents the meaning of each value,
	;; e.g. ("Y" "fullname" "cellphone" "address")
	(Y-and-predicates (cons "Y" predicates)))
    (with-multiple-csv (;; the input data
			(in input-data
			    :direction :input :separator #\Tab :skip-head t
			    :external-format :utf-8)
			;; the output data which stores training data in exact mode
			(out-exact output-exact
				   :direction :output :separator #\Tab :external-format :utf-8
				   :if-does-not-exist :create :if-exists :supersede)
			;; the output data which stores training data in fuzzy mode
			#|(out-fuzzy output-fuzzy
                        :direction :output :separator #\Tab :external-format :utf-8
                        :if-does-not-exist :create :if-exists :supersede)|#)
      ;; don't forget to write the head line
      (write-csv-head-line out-exact Y-and-predicates)
      ;;(write-csv-head-line out-fuzzy Y-and-predicates)
      ;; start iterating over the lines of input data
      (loop
         for index = 1 then (+ 1 index)
	 for line = (handler-case (fare-csv:read-csv-line in)
                      (error (c) 'bad-format-csv-line))
	 while (and (<= index limit) line) do
           (unless (eq line 'bad-format-csv-line)
             (let ((column-line-table (funcall get-column-line-table-by-line line)))
               ;; we then start querying from credoo database.
               (when (line-has-valid-idno-p column-line-table info-hash-table)
                 ;; record index information, this is for Section II(validating).
                 ;; here, *lines-with-idno-indices* is a list that every element is an index where
                 ;; a line with that index has a valid `idno' property.
                 (push index *lines-with-idno-indices*)
                 (with-candidates-pool (pool)
                   ;;(tagbody
                   ;;exact ;; first, do exact match and write the generated training data.
                   (parse-csv-line->pool pool column-line-table info-hash-table :exact)
                   ;;(backup-candidates (slot-value pool 'candidates))
                   #+ignore (dump-candidates-pool pool *exact-candidates-pools-with-idno* :copy t :index index)
                   (when (pool-suited-for-training pool)
                     (rewrite-candidates-pred-vals-list-exact-mode pool predicates)
                     #+ignore (dump-candidates-pool pool *exact-candidates-pools-with-idno* :copy t :index index)
                     (let ((pred-val-alists
                            (compute-training-data-on-pool pool default-score-calculator)))
                       (when pred-val-alists
                         (let ((lines
                                (mapcar (lambda (pred-val-alist)
                                          (mapcar 'cdr pred-val-alist))
                                        pred-val-alists)))
                           (fare-csv:write-csv-lines lines out-exact))))))))))))
  ;;(refresh-candidates-pool pool)
  #|(parse-csv-line->pool pool column-line-table info-hash-table :fuzzy)
  (go fuzzy)
  fuzzy
  (dump-candidates-pool pool *fuzzy-candidates-pools-with-idno* :copy t :index index)
  (when (pool-suited-for-training pool)
  (rewrite-candidates-pred-vals-list-fuzzy-mode pool predicates distance-fn)
  #+ignore (dump-candidates-pool pool *fuzzy-candidates-pools-with-idno* :copy t :index index)
  (let ((pred-val-alists
  (compute-training-data-on-pool pool default-score-calculator)))
  (when pred-val-alists
  (let ((lines
  (mapcar (lambda (pred-val-alist)
  (mapcar 'cdr pred-val-alist))
  pred-val-alists)))
  (fare-csv:write-csv-lines lines out-fuzzy)))))
  (go end)
  try-to-reduce-pool-size
  (with-slots (candidates count-of-candidates candidates-list candidates-score-table) pool
  ;; in this section, this pool is detected as `pool-suited-for-training-p'
  (let ((highest-score
  (gethash/upi (first candidates-list) candidates-score-table)))
  (remove-candidate-if pool
  (lambda (p)
  (/= highest-score (gethash/upi p candidates-score-table)))
  :destructive-p t
  :keep-idno-matched t) ;; remove but keep the idno-matched candidate
  (when (= count-of-candidates (hash-table-count *backup-candidates*))
  (refresh-candidates-pool pool)
  (parse-csv-line->pool pool column-line-table info-hash-table :fuzzy)
  (go fuzzy))
  (dolist (candidate candidates-list)
  (setf (gethash/upi candidate candidates)
  (gethash/upi candidate *backup-candidates*)))))
  (go fuzzy)
  end (clrhash *backup-candidates*))|#
  (progn
    (let ((score-calculator-exact (train-from-csv (namestring output-exact) predicates 0 :logistic))
          #+ignore (score-calculator-fuzzy (train-from-csv (namestring output-fuzzy) predicates 0 :logistic)))
      (setf *lines-with-idno-indices* (nreverse *lines-with-idno-indices*))
      (values score-calculator-exact #+ignore score-calculator-fuzzy #+test limit))))

(defun validating
    (input-data predicates exact-score-calculator fuzzy-score-calculator
     &key (threshold-output (make-output-filename input-data :type :threshold-report))
       (distance-fn #'levenschtein)
       (limit most-positive-fixnum))
  (declare (ignorable input-data))
  (with-multiple-csv ((in input-data :direction :input :skip-head t
			  :external-format :utf-8 :separator #\Tab)
		      (out threshold-output :direction :output :separator #\Tab
			   :if-does-not-exist :create :if-exists :supersede
			   :external-format :utf-8))
    (dolist (index *lines-with-idno-indices*)
      (when (<= index limit)
	(let ((pool (gethash index *exact-candidates-pools-with-idno*)))
	  (with-slots (idno-matched candidates-score-table) pool
	    (tagbody
	       (when (candidates-pool-empty-p pool)
		 (remhash index *exact-candidates-pools-with-idno*)
		 (go fuzzy))
	       (unless (idno-matched-is-candidate-p pool)
		 (rewrite-candidates-pred-vals-list-exact-mode pool predicates))
	       (compute-candidates-score-table pool exact-score-calculator)
	       (sort-candidates-by-score pool)
	       (multiple-value-bind (status candidate score)
		   (candidates-pool-match-status pool)
		 (vom:alert "NO.~A line's matching status: ~A" index status)
		 (case status
		   ((:no-match :mismatch :mutiple-match)
		    (remhash index *exact-candidates-pools-with-idno*)
		    (go fuzzy))
		   (:perfect-match
		    (fare-csv:write-csv-line
		     (list index "exact_match" (part->terse candidate) score
			   (part->terse idno-matched) (gethash/upi idno-matched candidates-score-table))
		     out))
		   (:single-match
		    (fare-csv:write-csv-line
		     (list index "exact_match" (part->terse candidate) score
			   (part->terse idno-matched) (gethash/upi idno-matched candidates-score-table))
		     out))))
	       (go end)
	     fuzzy
	       (setf pool (gethash index *fuzzy-candidates-pools-with-idno*))
	       (when (candidates-pool-empty-p pool)
		 (fare-csv:write-csv-line
		  (list index "no_match" "" "" "" "") out))
	       (unless (idno-matched-is-candidate-p pool)
		 (rewrite-candidates-pred-vals-list-fuzzy-mode pool predicates distance-fn))
	       (compute-candidates-score-table pool fuzzy-score-calculator)
	       (sort-candidates-by-score pool)
	       (multiple-value-bind (status candidate score)
		   (candidates-pool-match-status pool)
		 (case status
		   (:no-match
		    (fare-csv:write-csv-line (list index "no_match" "" "" "" "") out))
		   (:mismatch
		    (fare-csv:write-csv-line
		     (list index "mismatch" (part->terse candidate) score
			   (part->terse idno-matched) (gethash idno-matched candidates-score-table ""))
		     out))
		   (:multiple-match
		    (fare-csv:write-csv-line
		     (list index "multiple_match" (part->terse candidate) score
			   (part->terse idno-matched) (gethash idno-matched candidates-score-table ""))
		     out))
		   (:single-match 
		    (fare-csv:write-csv-line
		     (list index "fuzzy_match" (part->terse candidate) score
			   (part->terse idno-matched) (gethash idno-matched candidates-score-table))
		     out))
		   (:perfect-match
		    (fare-csv:write-csv-line
		     (list index "fuzzy_match" (part->terse candidate) score
			   (part->terse idno-matched) (gethash idno-matched candidates-score-table))
		     out))))
	       (go end)
	     end (values)))))))
  #-test(clrhash *exact-candidates-pools-with-idno*)
  #-test(clrhash *fuzzy-candidates-pools-with-idno*))

(defun matching
    (input-data get-column-line-table-by-line info-hash-table predicates exact-score-calculator fuzzy-score-calculator
     &key (final-output (make-output-filename input-data :type :final-report))
       (distance-fn #'levenschtein)
       (limit most-positive-fixnum))
  (with-multiple-csv ((in input-data :direction :input :skip-head t
			  :external-format :utf-8 :separator #\Tab)
		      (out final-output :direction :output :separator #\Tab
			   :if-does-not-exist :create :if-exists :supersede
			   :external-format :utf-8))
    (loop with lst = *lines-with-idno-indices* and result = nil
       for index = 1 then (1+ index)
       for line = (fare-csv:read-csv-line in) then (fare-csv:read-csv-line in)
       while (and (<= index limit) line) do
	 (let ((idno-index (car lst)))
	   (cond ((= index idno-index)
		  (setf lst (cdr lst)))
		 (t ;; this line does not contains `idno'
		  (let ((column-line-table (funcall get-column-line-table-by-line line)))
		    (with-candidates-pool (pool)
		      (with-slots (candidates-list candidates-score-table) pool
			(tagbody
			 ;; try-exact-match
			   (parse-csv-line->pool pool column-line-table info-hash-table :exact)
			   (rewrite-candidates-pred-vals-list-exact-mode pool predicates)
			   (compute-candidates-score-table pool exact-score-calculator)
			   (sort-candidates-by-score pool)
			   (when (candidates-pool-empty-p pool)
			     (refresh-candidates-pool pool)
			     (go try-fuzzy-match))
			   (let* ((1st-candidate (first candidates-list))
				  (2nd-candidate-p (cdr candidates-list))
				  (2nd-candidate (if 2nd-candidate-p (second candidates-list) nil)))
			     (cond ((and 2nd-candidate-p (candidates-score-equal pool 1st-candidate 2nd-candidate))
				    (refresh-candidates-pool pool)
				    (go try-fuzzy-match))
				   ((or (null 2nd-candidate-p)
					(and 2nd-candidate-p (not (candidates-score-equal pool 1st-candidate 2nd-candidate))))
				    (setf result (list "EXACT_MATCH" (part->terse 1st-candidate)
						       (gethash 1st-candidate candidates-score-table)))
				    (go report))))
			 try-fuzzy-match
			   (parse-csv-line->pool pool column-line-table info-hash-table :fuzzy)
			   ;; reduce pool size
			   (rewrite-candidates-pred-vals-list-fuzzy-mode pool predicates distance-fn)
			   (compute-candidates-score-table pool fuzzy-score-calculator)
			   (sort-candidates-by-score pool)
			   (when (candidates-pool-empty-p pool)
			     (setf result (list "NO_MATCH" "" ""))
			     (go report))
			   (let* ((1st-candidate (first candidates-list))
				  (2nd-candidate-p (cdr candidates-list))
				  (2nd-candidate (if 2nd-candidate-p (second candidates-list) nil)))
			     (cond ((and 2nd-candidate-p (candidates-score-equal pool 1st-candidate 2nd-candidate))
				    (setf result (list "MULTIPLE_MATCH" "" ""))
				    (go report))
				   ((or (null 2nd-candidate-p)
					(and 2nd-candidate-p (not (candidates-score-equal pool 1st-candidate 2nd-candidate))))
				    (setf result (list "FUZZY_MATCH" (part->terse 1st-candidate)
						       (gethash 1st-candidate candidates-score-table)))
				    (go report))))
			 report
			   (fare-csv:write-csv-line result out)))))))))))

#+test
(progn
  (defparameter *chezhu.data* "/data/data12/zhengxin_pro/Heater/DATA/chezhu_info.data")
  (defparameter *yezhu.data* "/data/data12/zhengxin_pro/Heater/DATA/yezhu_info.data")
  (defparameter *output.tmp* "/data/data12/zhengxin_pro/gutianyu860/data_heater/local-projects/data-heater/data/output.tmp")
  (defparameter *cmd-args* (cons *chezhu.data* '("--fullname=fullname" "--cellphone=cellphone" "--idcard=idno" "--address=address")))
  (defparameter *info-hash-table* (nth-value 2 (process-command-line-args *cmd-args*)))
  (defparameter *columns* (nth-value 1 (process-command-line-args *cmd-args*)))
  (defparameter *get-column-line-table-by-line* (%make-column-line-table-by-line% *columns*))
  (defparameter *predicates* (list "fullname" "cellphone" "address"))
  (defun select-data-line (data index)
    (let ((fare-csv:*separator* #\Tab))
      (alexandria:with-input-from-file (in data)
	(let (result)
	  (dotimes (i (+ 1 index))
	    (setf result (fare-csv:read-csv-line in)))
	  result))))
  ;;(defparameter *line* (select-data-line *chezhu.data* 100))  
  (defun count-csv-lines (file)
    (let ((fare-csv:*separator* #\Tab))
      (alexandria:with-input-from-file (in file)
	(loop with lino = 0 
	   for line = (read-line in nil nil) then (read-line in nil nil)
	   while line do (incf lino)
	   finally (return lino)))))
  (defvar *exact-score-calculator* nil)
  (defvar *fuzzy-score-calculator* nil)
  )
