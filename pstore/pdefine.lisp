;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;;  this file did not exists in the original code
;;;;  here it exists to avoid style warnings on compile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Persistent classes for the log-based persistent store.
(in-package #:csf-persistent-store)


;;; $$$ was defined in ptxn.lisp, first used in pclass.lisp
(defvar-unbound *default-persistent-store*
  "Persistent store used for allocation if no other is specified.")

(defun call-with-default-persistent-store (persistent-store thunk)
  (multiple-value-prog1
      (let ((*default-persistent-store* persistent-store))
        (debug-message 4 "Default persistent store is now ~s" *default-persistent-store*)
        (funcall thunk))
    (if (boundp '*default-persistent-store*)
        (debug-message 4 "Default persistent store is now ~s" *default-persistent-store*)
        (debug-message 4 "There is no default persistent store."))))
