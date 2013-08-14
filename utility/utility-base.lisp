;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: New BSD
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file has utility functions that other utility functions use.
;;; As such, it *must* be written using *only* the built-in CommonLisp
;;; features (and native extensions).

(in-package #:csf-utility)

(proclaim (standard-optimizations))



(defvar *debug-noise-print-level* 5)
(defvar *debug-noise-print-length* 10)

(defun format-debug-message (noise format-string arglist)
  ;; Do not `optimize' this.  We want to construct the format-string
  ;; and *then* print it.
  (let ((message (let ((*print-length* *debug-noise-print-length*)
                       (*print-level*  *debug-noise-print-level*))
                   ;; Magic format string:  print DEBUG level
                   ;; and indent that many spaces.
                   (format nil "DEBUG ~d>~v@T ~?~&"
                           noise noise format-string arglist))))
    (fresh-line *debug-io*)
    (write-string message *debug-io*)
    #+lispworks
    (unless system::*in-no-interrupts*
      ;; Send the output, but don't wait.
      (force-output *debug-io*)
      ;; Let other processes (like the GUI that will display this output) run.
      (mp:process-allow-scheduling 0))
    #-lispworks
    (force-output *debug-io*)
    ))

