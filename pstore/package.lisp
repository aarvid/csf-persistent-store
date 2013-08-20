
;;;; package.lisp for persistent-store
;;;; this file was not originally part of the ChangeSafe code.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; remove installation of series package before redefinition of package
  (let ((spkg (find-package :series))
        (pkg (find-package :csf-persistent-store)))
    (when (and spkg pkg (member spkg (package-use-list pkg)))
      (series::install :pkg pkg :remove t)))

  (defpackage #:csf-persistent-store
    (:nicknames :pstore)
    ;; do not :use series, install series after definition of package
    (:use #:closer-common-lisp #:csf-config #:csf-utility) 
    #-(or allegro clisp)
    (:import-from #+lispworks #:mp
                  #+(or openmcl digitool) #:ccl
                  #+cmu       #:system
                  #+sbcl      #:sb-sys

                  #:without-interrupts)
    (:import-from #:alexandria define-constant)
    (:export
     ;; did.lisp
     did->list
     did->string
     did/class
     did/class
     did/domain
     did/numeric-id
     did/repository
     distributed-identifier
     distributed-identifier?
     list->did
     make-distributed-identifier
     parse-did   
     ;; pclass.lisp
     persistent-store
     persistent-store/probe
     persistent-store/open
     persistent-store/open?
     persistent-store/open-mode
     persistent-store/close
     +object-id-of-root+
     )
    )
  
  ;; install series package after definition of package.
  (let ((spkg (find-package :series))
        (pkg (find-package :csf-persistent-store)))
    (when (and spkg pkg (not (member spkg (package-use-list pkg))))
      (series::install :pkg pkg :macro t :remove nil))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (series::install :pkg (find-package :csf-persistent-store)
                   :macro t))
