;;;; config.lisp
(in-package :cl-user)
(defpackage #:data-heater.config
  (:use #:cl #:excl #:db.agraph #:net.jlinker)
  (:export #:*root-path*
           #:.composed-time.
	   #:*credoo-db*
           #:person
	   #:disconnet-credoo
	   #:*all-freetext-indices*
	   #:*credoo-all-predicates-strings*
	   #:*credoo-all-predicates-resources-table*)
  (:documentation "This package contains some initilization and cinfiguration work"))
(in-package #:data-heater.config)

(defvar *root-path*
  (uiop:pathname-parent-directory-pathname (uiop:current-lisp-file-pathname)))

(define-symbol-macro .composed-time. (excl:universal-time-to-string (get-universal-time)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-!-reader)
  (enable-print-decoded t))

(register-namespace "credoo" "http://credoo.com/")

(deftype person () '(simple-array (unsigned-byte 8) (12)))

(defvar *credoo-db*
  (open-triple-store "credoo")
  "The db object.")

(defun disconnect-credoo
    (&key (if-closed :ignore) (commit t) ensure-not-lingering)
  (close-triple-store :db *credoo-db*
		      :if-closed if-closed
		      :commit commit
		      :ensure-not-lingering ensure-not-lingering))

(defvar *all-freetext-indices* (list-freetext-indices *credoo-db*))

(defparameter *credoo-all-predicates-strings*
  (list "idno" ;; this is special, because we need this to be the subject, usually
	"cellphone" "fullname"
	"householdRegisterationAddress"
	"alumniOf" "jobTitle" "email"
	"worksFor" "deathDate" "monthlyIncome"
	"creditLimit" "isPingAnVIP"
	"telephone" "address" "qq"
	"driverLicenseIssueDate" "driverLicense"
	"homeAddress" "username")
  "All predicates (STRINGS) have been added (yet).")

(defparameter *credoo-all-predicates-resources-table*
  (let ((ht (make-hash-table :test 'string=)))
    (map nil
	 (lambda (pred)
	   (setf (gethash pred ht) (intern-resource pred :namespace "credoo")))
	 *credoo-all-predicates-strings*)
    ht))
