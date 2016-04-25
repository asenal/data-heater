(in-package #:user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (unless (find-package '#:asdf)
    (error "ASDF could not be required"))
  (require :regexp2)
  (require :util-string))

;;; set the :print and :stats switches to be true
(setf (sys:gsgc-switch :print) t)
(setf (sys:gsgc-switch :stats) t)

#-allegrograph
(load "/data/data12/zhengxin_pro/Heater/BIN/agraph-6.0.1/lib/agraph.fasl")

(defparameter *root-path*
  (uiop:pathname-directory-pathname (uiop:current-lisp-file-pathname)))

;; initilize jlinker and then include weka.jar
(defparameter *weka.jar*
  (merge-pathnames "software/weka.jar" *root-path*))
(uiop:with-current-directory (*root-path*)
  (load "jl-config"))
(unless net.jlinker:*jlinker-connection*
  (net.jlinker:jlinker-init :native :classpath *weka.jar*))
(format t "JLinker and weka.jar has been initialized successfully.~%")

(defparameter *softwares*
  `((alexandria . ,(merge-pathnames "software/alexandria-20150505-git/" *root-path*))
    (fare-csv . ,(merge-pathnames "software/fare-csv-20151218-git/" *root-path*))
    (vom . ,(merge-pathnames "software/vom/" *root-path*))
    (data-heater . ,(merge-pathnames "src/" *root-path*))))

(defparameter *files*
  '((alexandria .
     ("package" "strings" "symbols" "macros" "lists" "definitions" "functions"
      "types" "sequences" "control-flow" "binding" "conditions" "hash-tables" "io"
      "arrays" "numbers" "features"))
    (fare-csv . ("package" "csv"))
    (vom . ("vom"))
    (data-heater . ("config" "command-line" "formatter" "io" "distance" "query" "train"
                    "candidates-pool" "data-heater"))))

(defun load-software (name)
  (let ((directory (cdr (assoc name *softwares*)))
        (files (cdr (assoc name *files*))))
    (uiop:with-current-directory (directory)
      (dolist (f files)
        (format t "Compiling: ~A ... ~A loaded.~%"
                name (compile-file f :load-after-compile t))))))

(dolist (software *softwares*)
  (handler-bind ((warning #'muffle-warning))
    (load-software (car software))))
