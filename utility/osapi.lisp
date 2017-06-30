;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; API to the Operating system

(in-package #:csf-utility)

(proclaim (standard-optimizations))




;;; !!! is this correct?
;;; changed to use cl-fad, code was win32 only.
(defun scan-directory (pathspec)
  ;; can't use native scanner.
  (declare (optimizable-series-function))
  (scan 'list
        (cl-fad:list-directory pathspec)))

