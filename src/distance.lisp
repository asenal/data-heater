;;;; distance.lisp
;;;; This is file was copied from @yuqiulin496's previous work,
;;;; and may be modified a lot in the future,
;;;; depends on how many different distance measuring method we decide to deploy or test.
(in-package :cl-user)
(defpackage #:data-heater.distance
  (:use #:cl)
  (:export #:levenschtein)
  (:documentation "This package contains functions that measure the distances between strings."))
(in-package #:data-heater.distance)
;;;---1. basic operator defination: get-in, clojure-assoc, assoc-in, based on dictionary
(defun get-in (trie ks)
  "return children share a ancestor chain ks; return nil if ancestor chain not exist; return trie if ancestor chain is null"
  (reduce (lambda (node k)
	    (declare (type hash-table node))
	    (gethash k node (make-hash-table :test 'equal)))
	  ks :initial-value trie))

(defun clojure-assoc (trie k v)
  "to monkey clojure's assoc, return a updated hash-table, k is atomic."
  (progn
    (setf (gethash k trie (make-hash-table :test 'equal)) v)
    trie))

(defun assoc-in (trie ks v)
  "input a key-chain (a list),"
  (if (= 1 (length ks))
      (clojure-assoc trie (car ks) v)
    (clojure-assoc trie (car ks) (assoc-in (gethash (car ks) trie (make-hash-table :test 'equal)) (cdr ks) v))))

;;;---2. trie-insert, trie-select defination. A trie is a nested hash-table (list also OK), with a special {:leaf value (default to T)}
(defun trie-update (trie ks v)
  ;;  `(assoc-in ,trie (list ,@ks :leaf) ,v))  ; dosn't work, why??? '(,@ks :leaf) not working, either.
  (assoc-in trie (append ks '(:leaf)) v))

(defun trie-select (trie ks)
  (get-in trie (append ks '(:leaf))))

(defun words2trie (words trie)
  "input a list of string, convert each string into character key-chain,insert to a trie, side effect = yes"
  (loop for word in words do (trie-update trie (coerce word 'list) word))
  trie)

;;--- test for get-in,assoc-in,
#+test
(progn
  (setf *dic-trie* (make-hash-table :test 'equal))
  (trie-update *dic-trie* '(:a :b) 1)
  (trie-update *dic-trie* '(:a :c) 2)
  (trie-update *dic-trie* '(:a) 3)
  (trie-update *dic-trie* '(:x) 4))

;;--3. levenschtein distance
; --3.1 a helper function, update the cost column given previous cost column and current input char.
(defun update-col-by-extended-char (prev-col word c)
  (let* ((l (length word))
         (col (make-array (1+ l) :element-type 'integer)))
    (setf (svref col 0) (1+ (svref prev-col 0)))
    (dotimes (i l)
      (setf (svref col (1+ i))
            (min (1+ (svref col i))
                 (1+ (svref prev-col (1+ i)))
                 (+ (svref prev-col  i)
                    (if (char-equal (char word i) c) 0 1)))))
    col))

; --3.2 helper function
(defun array-upto (l)
  (let ((x (make-array l :element-type 'integer)))
    (dotimes (i l) (setf (svref x i) i))
    x))

; --3.3 levenschtein distance defination
(defun levenschtein (s1 s2)
  (let ((l (length s1)))
    (svref ; return the last element of cost column, after running out char from s2
     (reduce #'(lambda (x y) ; iterate char over s2 and update cost column against s1
                 (update-col-by-extended-char x s1 y))
             (coerce s2 'list)
             :initial-value (array-upto (1+ l)))
     l)))

;;; --3.4 test code
#+test
(defun test-leven ()
  (and
   (= (levenschtein "hello" "hell0") 1)
   (= (levenschtein "hello" "") 5)
   ))

;;--4. running levenschein match against a trie
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))


(defun leven-trie (trie word threshhold)
  (defun tt (ks trie threshhold cost-column)
    (mapcan #'(lambda (k)
                (if  (equal :leaf k)
                      (and   (>= threshhold (svref cost-column (1- (length cost-column))))
                             (list ks))
                  (let ((minimal-cost (apply #'min (coerce cost-column 'list)))
                      (next-node (gethash k trie))
                      (next-ks (concatenate 'list ks (list k)))
                      (next-col (update-col-by-extended-char cost-column word (coerce k 'character))))
                    (and (<= minimal-cost threshhold)
;                       (if (not (hash-table-p next-node))  (list ks)
                         (tt next-ks next-node threshhold next-col)))))
            (hash-keys trie)))
  (mapcar #'(lambda (x) (coerce x 'string))
          (tt '() trie threshhold (array-upto (1+ (length word))))))


;;-- test code
#+test
(progn
  (setf words '("深圳市 罗湖区平安大厦" "深圳市 罗湖平安大厦" "深圳市平安大厦" "广东深圳罗湖区平安大厦" "深圳市罗湖区平安银行"))
  (defvar *trie* (make-hash-table :test 'equal))
  (words2trie words *trie*)
  (leven-trie *trie* "深圳罗湖平安大厦" 3))

;(require :osi)
;(use-package :excl.osi)
;(defparameter *more-words*  (remove-duplicates (sort (mapcan #'(lambda (x) (split-re " " x)) (command-output "man curl")) #'string-lessp)))
;(words2trie *more-words* *trie*)
