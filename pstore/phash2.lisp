;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;;  Note: phash was divided into two parts
;;;;        this was because of a strange situation where compile/loading
;;;;        the whole file as one file would cause unreachable code warnings
;;;;        that made no sense at the point the files were divided.
;;;;        Though individual compilation and breaking into two files does
;;;;        not cause these warnings.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Jrm's log-based persistent store
(in-package #:csf-persistent-store)

(proclaim (standard-optimizations))

;;; !!!
;;; if placed in phash1.lisp this function would
;;; give several notes about unreachable code which makes no sense.
;;; here in a separate file this note does not occur.
;;; !!!
(defun persistent-hash-table/puthash (phashtb key value)
  (let* ((buckets (persistent-hash-table/bucket-vector phashtb))
         (length  (persistent-vector-length buckets))
         (hkey    (mod (sxhash key) length))
         (entry   (persistent-hash-table-bucket-entry/find
                   key
                   (persistent-vector-ref buckets hkey)
                   (persistent-hash-table/test-function phashtb))))
    (if entry
        (remake-instance entry
                         :key key
                         :value value
                         :next-entry
                         (persistent-hash-table-bucket-entry/next-entry entry))
        (setf (persistent-vector-ref buckets hkey)
              (make-instance 'persistent-hash-table-bucket-entry
                             :key key
                             :value value
                             :next-entry (persistent-vector-ref buckets hkey))))))

(defun (setf persistent-hash-table/gethash) (new-value phashtb key)
  (persistent-hash-table/puthash phashtb key new-value))

;;; Cheesy, but effective.  A pstore-list is a 2-element pstore vector.
(defclass pstore-list ()
  ((cell-node-id :initarg :cell-node
                 :initform (error "Required initarg :cell-node omitted")
                 :reader pstore-list/cell-node-id))
  (:metaclass persistent-standard-class)
  (:schema-version 0))

(defmethod print-object ((object pstore-list) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defun %pstore-car (pstore-list)
  (persistent-object/find
   (persistent-standard-object/pstore pstore-list)
   (persistent-object/vector-ref
    (persistent-standard-object/pstore pstore-list)
    (pstore-list/cell-node-id pstore-list)
    1)))

(defun %pstore-cdr (pstore-list)
  (persistent-object/find
   (persistent-standard-object/pstore pstore-list)
   (persistent-object/vector-ref
    (persistent-standard-object/pstore pstore-list)
    (pstore-list/cell-node-id pstore-list)
    2)))

(defun (setf %pstore-car) (new-value pstore-list)
  (persistent-object/vector-set
   (persistent-standard-object/pstore pstore-list)
   (pstore-list/cell-node-id pstore-list)
   1
   new-value))

(defun (setf %pstore-cdr) (new-value pstore-list)
  (persistent-object/vector-set
   (persistent-standard-object/pstore pstore-list)
   (pstore-list/cell-node-id pstore-list)
   2
   new-value))

(defmethod make-instance ((class (eql (find-class 'pstore-list))) &rest initargs
                          &key (persistent-store *default-persistent-store*)
                          pcar-node pcdr-node)
  (let ((cell-node (persistent-object/allocate-vector persistent-store)))
    (persistent-object/vector-set
     persistent-store cell-node 1 pcar-node)
    (persistent-object/vector-set
     persistent-store cell-node 2 pcdr-node)
    (apply #'call-next-method class
           :cell-node cell-node
           initargs)))

(defun pstore-list-cons (pcar pcdr)
  (let ((car-store (and (typep pcar 'persistent-standard-object)
                        (persistent-standard-object/pstore pcar)))

        (cdr-store (and (typep pcar 'persistent-standard-object)
                        (persistent-standard-object/pstore pcar))))

    (cond ((and car-store cdr-store)
           (unless (eq car-store cdr-store)
             (error 'changesafe-database-error
                    :format-control "Attempt to create a cross-repository pointer."
                    :format-arguments (list pcar pcdr)))
           (make-instance 'pstore-list
                          :persistent-store car-store
                          :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar car-store))
                          :pcdr-node (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr car-store))))
          (car-store
           (make-instance 'pstore-list
                          :persistent-store car-store
                          :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar car-store))
                          :pcdr-node (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr car-store))))
          (cdr-store
           (make-instance 'pstore-list
                          :persistent-store cdr-store
                          :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar cdr-store))
                          :pcdr-node (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr cdr-store))))
          (t (make-instance 'pstore-list
                            :persistent-store *default-persistent-store*
                            :pcar-node (if (typep pcar 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcar)
                                         (persistent-object/save pcar *default-persistent-store*))
                            :pcdr-node  (if (typep pcdr 'persistent-standard-object)
                                         (persistent-standard-object/node-id pcdr)
                                         (persistent-object/save pcdr *default-persistent-store*)))))))

(defun pstore-list/car (pstore-list)
  (%pstore-car pstore-list))

(defun pstore-list/cdr (pstore-list)
  (%pstore-cdr pstore-list))

(defun scan-pstore-list-cdrs (pstore-list)
  (declare (optimizable-series-function))
  (scan-fn '(optional pstore-list)
           (lambda () pstore-list)
           (lambda (previous-list) (pstore-list/cdr previous-list))
           (lambda (previous-list) (null previous-list))))



(defun scan-pstore-list (pstore-list)
  (declare (optimizable-series-function 1))
  (#M pstore-list/car (scan-pstore-list-cdrs pstore-list)))

(defun (setf pstore-list/car) (new-value pstore-list)
  (ensure-same-repository pstore-list new-value)
  (setf (%pstore-car pstore-list)
        (if (typep new-value 'persistent-standard-object)
            (persistent-standard-object/node-id new-value)
            (persistent-object/save new-value (persistent-standard-object/pstore pstore-list))))
  new-value)

(defun (setf pstore-list/cdr) (new-value pstore-list)
  (ensure-same-repository pstore-list new-value)
  (setf (%pstore-cdr pstore-list)
        (if (typep new-value 'persistent-standard-object)
            (persistent-standard-object/node-id new-value)
            (persistent-object/save new-value (persistent-standard-object/pstore pstore-list))))
  new-value)

(defun pstore-list-reclaim (pstore-list)
  ;; We need a GC
  (setf (%pstore-car pstore-list) nil)
  (setf (%pstore-cdr pstore-list) nil))

(defun lisp-list->pstore-list (lisp-list)
  (if (null lisp-list)
      nil
      (pstore-list-cons (car lisp-list)
                        (lisp-list->pstore-list (cdr lisp-list)))))

(defun persistent-list (&rest args)
  (lisp-list->pstore-list args))

(defun pstore-list->lisp-list (pstore-list)
  (collect 'list (scan-pstore-list pstore-list)))

(defmacro pstore-list-push (object pstore-list)
  `(SETF ,pstore-list (PSTORE-LIST-CONS ,object ,pstore-list)))


