;; This work in the Public Domain, thereby relinquishing all
;; copyrights. Everyone is free to use, modify, republish, sell or give
;; away this work without prior consent from anybody.
;;
;; This code is provided on an "as is" basis, without warranty of any
;; kind. Use at your own risk! Under no circumstances shall the author(s)
;; or contributor(s) be liable for damages resulting directly or indirectly
;; from the use or non-use of this program.

(in-package :user)

;;;
;;; SAMPLE FILE with DUMMY values to be customized for each site
;;;
;;; On Microsoft Windows, this file is normally not needed because
;;;  the required information can be found in the Windows Registry.
;;;
;;; An application using jlinker may need to provide settings for the 
;;; variables mentioned in this file in the following cases:
;;;
;;;   - Java is started from Lisp with a call to (jlinker-init [:start] ... )
;;;   - Java is linked in native mode with (jlinker-init :native ... )
;;; 
;;; An application may load a customized copy of this file, 
;;; or relevant portions of this file can be included in the
;;; application source.
 

;;;??? marks places that MUST be customized


;;;??? - Comment out the following statement in the customized
;;;       copy of this file.
;;(error "This file must be renamed and customized before it can be used")

(eval-when (compile load eval) (require :jlinker))

;; The variable net.jlinker:*jlinker-java-home* must be set to 
;; the directory (folder) where Java is installed.
;;
;; Expected value of this symbol:
;; 
;; string -> Path namestring of Java home directory
;;           jLinker will construct a suitable value for PATH
;;           and for CLASSPATH
;;
;; nil    -> jLinker makes no modifications to PATH or CLASSPATH
;;           User must make sure that the file jlinker.jar is 
;;           visible to Java.

;;;??? - Modify the following expression to set an appropriate value.
;;       Use one (and only one) of case A or case B.

;; Case A: You are the only user of jLinker and jLinker will be only
;;         be used on one machine.
;;      Step A.1: Modify the string "???" to the path of the Java
;;                installation in your machine.
;;      Step A.2: Uncomment the next line.
(setf net.jlinker:*jlinker-java-home* "/usr/lib/jdk")
(setf net.jlinker:*jni-library* "/usr/lib/jdk/jre/lib/amd64/server/libjvm.so")

(setf *site-keyword*
	  ;; normalize the machine name to a keyword 
	  ;; in the current readtable case
	  (read-from-string
	     (concatenate 'string ":" (string-downcase (short-site-name))))
	  )

;; Case B: If jLinker must run on several different machines with different 
;;         Java configurations, the following expression allows one 
;;         jl-config.cl file to be used on all the machines.
;; If using Case B, comment out the following (#+...) line
#+ignore
(setf net.jlinker:*jlinker-java-home*
      (case *site-keyword*

;;;???(Case B only)  Insert your own machine names here.

	(:frigate   "c:\\Franz\\Java\\jdk150")

	;; The value "" implies that java is already in the path
	;;  the current directory will be added to CLASSPATH.
	;; The value NIL implies that both path and CLASSPATH
	;;  are preset correctly outside this script.
	;; The value :FIND asks jlinker to look in the Windows
	;;  Registry for the location of Java components.
	(otherwise   #+windows :find #+unix "/usr/java/default" #-unix "")
	)
      )
#+ignore
(setf net.jlinker:*jni-library* 
      (case *site-keyword*
	
;;;???(Case B only)  Insert your own machine names here.

	(:frigate  "jre/bin/client/jvm.dll")
	(:teller   "jre/lib/i386/client/libjvm.so")
	(:blade    "jre/lib/sparc/client/libjvm.so")
	))

;;; Some Unix systems need this to be t
;;; When t, LD_LIBRARY_PATH must contain directory with libjvm.so
(setf net.jlinker:*jni-ld-path-p* 
      (case *site-keyword*
	(:gazelle  t)
	))

;; jLinker default settings
;;
;; The following variables have default values that may be changed
;; to suit special situations (such as heavy debugging).  The symbols
;; are described in more detail in the reference manual.
;; 
;;(setf net.jlinker:*jlinker-error-p* t)            ;; default is nil
;; 
(setf net.jlinker:*jlinker-verbose* t)            ;; default is nil
;;(setf net.jlinker:*jlinker-debug* t)              ;; default is nil
;;(setf net.jlinker:*jlinker-retry-number* 3)       ;; default is 3
;;(setf net.jlinker:*jlinker-retry-delay*  5)       ;; default is 5


;; The following variable is used only in Unix implementations.
;; When t, Java is started directly, without invoking a shell (and
;; shell profile).  In general, this is a good idea because it avoids user
;; shell profiles when we call Java (after setting up path and CLASSPATH).
(setf net.jlinker:*jlinker-unix-vector-p* t)        ;; default is t


;; *jlinker-run-java*
;;
;; The variable net.jlinker:*jlinker-run-java* must be set to the
;; name of the function that will be used to start Java.  The default
;; setting shown below should be adequate for most installations.
(setf net.jlinker:*jlinker-run-java* 'net.jlinker::run-java)
