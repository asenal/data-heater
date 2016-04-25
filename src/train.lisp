;;;; train.lisp
(in-package :cl-user)
(defpackage #:data-heater.train
  (:use #:cl #:excl #:net.jlinker)
  (:export #:*default-score-calculator*
	   #:*previous-work-score-calculator*
	   #:score-calculator-content
	   #:make-score-calculator-parameters-list
	   #:train-from-csv)
  (:documentation "This package contains the training section's functionality."))
(in-package #:data-heater.train)

(defclass score-calculator ()
  ((type :initarg :type :type symbol)
   (content :initarg :content :accessor score-calculator-content :type (or hash-table function)))
  (:metaclass mop:funcallable-standard-class))

(defun make-score-calculator (content)
  (declare (type (or hash-table function) content))
  (make-instance 'score-calculator
		 :content content :type (cond ((typep content 'hash-table) 'hash-table)
					      ((typep content 'function) 'function))))

(defun make-score-calculator-fn (content type)
  (case type
    (function content)
    (hash-table
     (let ((predicates (mapcar 'intern (alexandria:hash-table-keys content))))
       (compile nil `(lambda (&key ,@(mapcar
				      (lambda (p) `(,p 0))
				      predicates))
		       (+ ,@(mapcar (lambda (p)
				      `(* ,p ,(gethash p content)))
				    predicates))))))))
  
(defmethod initialize-instance
    :after ((this score-calculator) &key)
  (with-slots (content type) this
    (mop:set-funcallable-instance-function
     this (make-score-calculator-fn content type))))

(defmethod (setf score-calculator-content)
    :after ((new-content hash-table) (this score-calculator))
  (progn
    (setf (slot-value this 'type) 'hash-table)
    (mop:set-funcallable-instance-function
     this (make-score-calculator-fn new-content 'hash-table))))

(defmethod (setf score-calculator-content)
    :after ((new-content function) (this score-calculator))
  (progn
    (setf (slot-value this 'type) 'function)
    (mop:set-funcallable-instance-function
     this (make-score-calculator-fn new-content 'function))))

(defun make-score-calculator-parameters-list (pred-val-alist)
  (declare (type list pred-val-alist))
  (let (parameters)
    (dolist (pred-val pred-val-alist)
      (let* ((pred (car pred-val))
	     (val (cdr pred-val))
	     (pred* (intern pred :keyword)))
	(setf (getf parameters pred*) val)))
    parameters))

(defvar *default-score-calculator*
  (make-score-calculator
   (alexandria:alist-hash-table
    (list (cons "fullname" 1.0d0) (cons "cellphone" 1.0d0)
	  (cons "address" 1d0) (cons "email" 1.0d0)
	  (cons "worksFor" 1d0))
    :test 'string= :size 5)))				   

(defvar *previous-work-score-calculator*
  (make-score-calculator
   (alexandria:alist-hash-table
    (list (cons "fullname" 0.35d0) (cons "cellphone" 0.51d0)
	  (cons "email" 1.67d0) (cons "address" 0.3d0)
	  (cons "worksFor" 0.25d0))
    :test 'string= :size 5)))

(defun set-class-index (weka-instances index)
  (jcall (jmethod 'weka.core.Instances 'setClassIndex 'int)
	 weka-instances index))

(defun new-logistic-model ()
  (jnew (jconstructor 'weka.classifiers.functions.Logistic)))

(defun get-classifiers-options (classifier)
  (jcall (jmethod 'weka.classifiers.Classifier 'getOptions)
	 classifier))

(defun set-classifiers-options (classifier options)
  (jcall (jmethod 'weka.classifiers.functions.Logistic 'setOptions
		  'Ljava.lang.String\;)
	 classifier
	 options))

(let ((csv-loader (jnew (jconstructor 'weka.core.converters.CSVLoader)))
      (%csv-loader-set-source%
       (jmethod 'weka.core.converters.CSVLoader 'setSource 'java.io.File))
      (%java-new-file% (jconstructor 'java.io.File 'java.lang.String)))
  (defun load-csv (csv-pathname)
    (let ((file (jnew %java-new-file% csv-pathname)))
      (jcall %csv-loader-set-source% csv-loader file)
      csv-loader)))

(defun get-dataset-from-csv (csv-pathname class-index)
  (let* ((csv-loader (load-csv csv-pathname))
	 (dataset (jcall (jmethod 'weka.core.converters.CSVLoader 'getDataSet)
			 csv-loader)))
    (set-class-index dataset class-index)
    dataset))

(defun train-from-csv (training-data predicates &optional (class-index 0) (classifier-mode :logistic))
  (vom:info "Start Training weights using weka.jar.
TThe training data is: ~A; the classifier model is set to ~A." training-data classifier-mode)
  (let ((classifier (jnew (jconstructor (case classifier-mode
					  (:logistic 'weka.classifiers.functions.Logistic)
					  (:libsvm 'weka.classifiers.functions.LibSVM)))))
	(dataset (get-dataset-from-csv training-data class-index)))
    ;; buildClassifier
    (jcall (jmethod 'weka.classifiers.Classifier 'buildClassifier
		    'weka.core.Instances)
	   classifier dataset)
    (let* ((len (length predicates))
	   (coeffs-jarray (jcall (jmethod 'weka.classifiers.functions.Logistic 'coefficients) classifier))
	   (coeffs (loop for i from 0 to len collect (jarray-ref (jarray-ref coeffs-jarray i) 0)))
	   (intercept (car coeffs))
	   (coeffs-alist (mapcar (lambda (name value) (cons (intern name) value)) predicates (cdr coeffs))))
      (vom:info "Training for the file: ~A successfully finished, the coefficients are: ~A"
		training-data coeffs-alist)
      (make-score-calculator
       (compile nil
		`(lambda (&key ,@(mapcar (lambda (c) `(,(car c) 0)) coeffs-alist))
		   (+ ,intercept
		      ,@(mapcar (lambda (item)
				  (let ((predicate (car item))
					(value (cdr item)))
				    `(* ,predicate ,value)))
				coeffs-alist))))))))
