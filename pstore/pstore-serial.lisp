;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;; $$$ new file pstore-serial, stuff in pstore that depends on serial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Jrm's log-based persistent store


(in-package #:csf-persistent-store)

(proclaim (standard-optimizations))


(defun persistent-store/reopen (pstore-pathname mode auxiliary-info probe)
  "reopen an already open store"
  (if (or (eq (persistent-store/open-mode probe) mode)
          (eq mode :read-only))
      (progn 
        (incf (persistent-store/open-count probe))
        probe)
      (let ((id         (persistent-store/guid probe))
            (open-count (persistent-store/open-count probe)))
        (debug-message 5 "Closing and re-opening store ~s to upgrade mode." id)
        (unregister-open-persistent-store id)
        (let ((reopened (persistent-store/open pstore-pathname
                                               :mode mode
                                               :auxiliary-info auxiliary-info)))
          (incf (persistent-store/open-count reopened) open-count)
          reopened))))

(defun persistent-store/open-new (pstore-pathname auxiliary-info index-stream)
  "create a totally new store"
  ;; Brand new store.  Need to get an ID and create a symbol table.
  (let ((store-id (generate-guid)))
    (debug-message 2 "Creating new database ~s." store-id)
    (write-sequence (babel:string-to-octets
                     (format nil "~s~%~%"
                             `((,+persistent-store-cookie+
                                ,+persistent-store-format+)
                               (:guid ,store-id)
                               ,@auxiliary-info)))
                    index-stream)
    (finish-output index-stream)
    (let* ((symbol-table (make-symbol-table))
           (store (%make-persistent-store pstore-pathname
                                          symbol-table
                                          index-stream
                                          store-id))
           (zero-loc  (persistent-store/next-location store)))
      (debug-message 5 "new register")
      (register-open-persistent-store store-id store)
      (debug-message 5 "new serialize")
      (serialize 0 index-stream symbol-table)
      (debug-message 5 "new object-map")
      (setf (persistent-store/object-map store)
            ;; install 0
            (object-map/add index-stream
                            ;; install NIL
                            (object-map/add index-stream
                                            nil
                                            +object-id-of-nil+
                                            0
                                            nil)
                            +object-id-of-zero+
                            zero-loc
                            0))
      (debug-message 5 "new commit")
      (persistent-store/commit store (format nil "Create database ~s" store-id))
      (values store store-id t))))

(defun persistent-store/open-existing (pstore-pathname index-stream)
  "opening an existing store"
  ;; Previously existing store.
  ;; need to find the last commit record.
  (with-open-file (scan pstore-pathname
                        :direction :input
                        :element-type 'unsigned-byte
                        :if-does-not-exist :error) ;; shouldn't happen!
    (let* ((store-id (recover-store-header scan))
           (commit-record (recover-commit-record pstore-pathname scan))
           (symbol-table (recover-symbol-table scan))
           (store (%make-persistent-store pstore-pathname
                                          symbol-table
                                          index-stream
                                          store-id)))
      (register-open-persistent-store store-id store)
      (setf (persistent-store/most-recent-commit-record store)
            (%make-commit-record (cadr (assoc :transaction-master commit-record))
                                 (cadr (assoc :node-id commit-record))
                                 (cadr (assoc :previous-record-offset commit-record))
                                 (cadr (assoc :location commit-record))
                                 (cadr (assoc :reason commit-record)))
            (persistent-store/guid store) store-id
            (persistent-store/symbol-table store) symbol-table

            ;; Fetching the object map should restore the entire
            ;; contents of the store to memory.
            (persistent-store/object-map store)
            (fetch-object-map store (persistent-store/symbol-table store) scan
                              (cadr (assoc :object-map-offset (cdr commit-record))))
            (persistent-store/object-count store)
            (cadr (assoc :object-count (cdr commit-record))))
      (values store store-id nil))))


(defun persistent-store/open (pstore-pathname &key (mode :read-write)
                                                   (auxiliary-info nil))
  ;; Auxiliary info is simply printed into the header.
  "Create a new persistent store, or open an existing one."
  (check-type pstore-pathname absolute-file-pathname)
  (let ((probe (find-open-persistent-store-by-pathname pstore-pathname)))
    (if probe
        (persistent-store/reopen pstore-pathname mode auxiliary-info probe)
       (let ((index-stream nil)
              (close-it? t))
          (unless (eq mode :read-only)
            (ensure-directories-exist
             (pathname-syntactic-parent pstore-pathname)))
          (unwind-protect
              (progn
                (when (eq mode :read-write)
                  (setf index-stream (open pstore-pathname
                                           ;; !!! :buffered t
                                           :direction :output
                                           :element-type 'unsigned-byte
                                           :if-exists :append
                                           :if-does-not-exist :create)))
                (if (and (eq mode :read-write)
                         (zerop (file-position index-stream)))
                    (persistent-store/open-new pstore-pathname
                                               auxiliary-info
                                               index-stream)
                    (persistent-store/open-existing pstore-pathname index-stream))
                (setq close-it? nil))`

            "cleanup after opening"
            (when close-it?
              (when index-stream (close index-stream))))))))




(defun find-open-persistent-store (pathname store-id)
  (let ((probe1 (cdr (assoc store-id *open-persistent-stores*))))
    (if probe1
        ;; Found one already open.  Since `recover-commit-record' will
        ;; close this again, bump the open-count.
        (progn
          (incf (persistent-store/open-count probe1))
          probe1)
        ;; Look in the directory.  Just open it read only while we
        ;; check the cell.  `recover-commit-record' will close it
        ;; again.
        (let ((probe2 (persistent-store/find-file pathname store-id)))
          (persistent-store/open probe2 :mode :read-only)
          (cdr (assoc store-id *open-persistent-stores*))))))



;;; !!! more series warnings.
(defun recover-commit-record (pathname stream)
  (debug-message 5 "Recovering commit record.")
  (collect-first
   (choose-if (lambda (commit-record)
               (debug-message 5 "Checking commit record ~{~s~}" commit-record)
               (let ((transaction-master (assoc :transaction-master commit-record)))
                 (debug-message 5 "Transaction master is ~s" transaction-master)
                 (and transaction-master
                      (or (null (cdr transaction-master))
                          (let* ((master-guid    (cadr transaction-master))
                                 (master-cell-id (cddr transaction-master))
                                 (master-pstore  (find-open-persistent-store pathname master-guid)))
                            (cond ((null master-pstore)
                                   (error 'changesafe-database-recovery-error
                                          :format-control "Two phase commit record database is missing."
                                          :format-arguments (list master-pstore)))
                                  ((not (persistent-store/open? master-pstore))
                                   (error 'changesafe-database-recovery-error
                                          :format-control "Two phase commit record is in unopen store ~s"
                                          :format-arguments (list master-pstore)))
                                  (t (prog1 (car (object-map-info/value
                                                  (object-map/find (persistent-store/object-map master-pstore)
                                                                   master-cell-id)))
                                       (persistent-store/close master-pstore)))))))))
             (scan-commit-records stream))))
