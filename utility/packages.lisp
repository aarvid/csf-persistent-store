;;;; package.lisp for utility of persistent-store
;;;; this file was not originally part of the ChangeSafe code.


(defpackage #:csf-config
  (:documentation "Configuration variables.")
  (:use #:common-lisp)
  (:export
   ;; declarations.lisp
   standard-optimizations
   performance-optimizations
   *disable-debug-messages*
   *debug-noise-level*
   *debug-noise-print-level*
   *debug-noise-print-length*
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; remove installation of series package
  (let ((spkg (find-package :series))
        (pkg (find-package :csf-utility)))
    (when (and spkg pkg (member spkg (package-use-list pkg)))
      (series::install :pkg pkg :remove t)))
  
  (defpackage #:csf-utility
    (:nicknames :putility :putils)
    ;; do not :use series, install series after definition of package
    (:use #:cl #:csf-config)  
    #-(or allegro clisp)
    (:import-from #+lispworks #:mp
                  #+(or openmcl digitool)       #:ccl
                  #+cmu       #:system
                  #+sbcl      #:sb-sys

                  #:without-interrupts)
    (:import-from #:alexandria define-constant)
    (:export
     ;; types.lisp
     optional
     array-index
     restrict-type
     non-negative-integer
     non-negative-fixnum
     simple-vector-8b
     simple-vector-32b
     ;; length.lisp
     string-length
     vector-length
     simple-bit-vector-length
     simple-vector-length
     ;; vector.lisp
     simple-vector-32b-allocate
     simple-vector-32b-ref
     simple-string-length
     %simple-substring-move-right
     ;; utility-macros.lisp
     defvar-unbound
     defsubst
     tcond 
     ;; pathname-utilities.lisp
     absolute-pathname
     absolute-file-pathname
     pathname-syntactic-parent
     ;; utils.lisp
     align-stream
     fixnum-base10-digit-count
     read-string-data
     write-string-data
     write-simple-bit-vector
     read-simple-bit-vector
     %mirror-fixnum
     %write-fixnum-to-string
     %write-fixnum
     %write-unsigned32
     write-fixnum
     write-unsigned16
     find-keyword
     read-fixnum
     read-unsigned32
     objects-equalp
     pushlast
     byte-counting-stream
     ;; replacement-macros.lisp
     debug-message
     when-debugging
     ;; ascii.lisp
     +ascii-linefeed+
     ;; osapi.lisp
     scan-directory
     ;; oset.lisp
     make-ordered-integer-set
     ordered-set/adjoin!
     ordered-integer-set/adjoin!
     ordered-integer-set/empty?
     ordered-integer-set/union!
     ordered-integer-set/union?
     ;; guid.lisp
     guid
     write-guid
     read-guid
     generate-guid
     ;; media-type.lisp
     media-type
     media-type/primary-type
     media-type/subtype
     find-media-type
     )
    )

  ;; install series package after definition of package.
  (let ((spkg (find-package :series))
        (pkg (find-package :csf-utility)))
    (when (and spkg pkg (not (member spkg (package-use-list pkg))))
      (series::install :pkg pkg :macro t :remove nil))))

(defpackage #:fixnum-math
  (:nicknames :fix)
  (:use #:cl #:csf-config)
  (:shadow
   = /= < > <= >=
   max min
   minusp plusp
   floor ceiling
   * + - / 1+ 1-
   abs evenp oddp
   gcd incf decf
   lcm ash
   logand logandc1 logandc2
   logeqv logior lognand
   lognor lognot logorc1 logorc2
   logxor logbitp logcount logtest
   deposit-field dpb ldb ldb-test
   mask-field)
  (:export
   ;; fixnum.lisp
   = /= < > <= >=
   max min
   minusp plusp
   floor ceiling
   * + - / 1+ 1-
   abs evenp oddp
   gcd incf decf
   lcm ash
   logand logandc1 logandc2
   logeqv logior lognand
   lognor lognot logorc1 logorc2
   logxor logbitp logcount logtest
   deposit-field dpb ldb ldb-test
   mask-field
   )
  )



#|
;; original defpackage in lwrebuild.lsp 
 (defpackage "CSF/CONFIG"
  (:documentation "Configuration variables.")
  (:use "COMMON-LISP")
  (:export "*MAJOR-SOFTWARE-VERSION*"
           "*MINOR-SOFTWARE-VERSION*"))
 (defpackage "CSF/UTILITY"
  (:nicknames "UTILITY" "UTILS")
  (:documentation "The basic utility package.")
  (:use "COMMON-LISP" "CSF/CONFIG"))

|#
