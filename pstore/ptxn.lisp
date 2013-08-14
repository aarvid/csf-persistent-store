;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:csf-persistent-store)

(proclaim (standard-optimizations))

#|(eval-when (:compile-toplevel :load-toplevel)
  (export '(
            *default-persistent-store*
            call-with-default-persistent-store
            call-with-transaction
            persistent-object/save
            persistent-object/find
            transaction-type
            transaction/disposition
            transaction/find-root
            transaction/mode
            transaction/reason
            )))|#

;;; $$$ Changed earmuffs from * to +, use alexandria:define-constant
(define-constant +transaction-types+ '(:read-only :read-write :read-cons)
  :test #'equalp)
(define-constant +transaction-dispositions+ '(:running :committing :aborting
                                              :committed :aborted)
  :test #'equalp)
;; running = transaction should commit upon return,
;;           but abort if throw
;;
;; aborting = transaction is still running, but it must abort upon exit
;; committing = transaction is still running, but it must commit
;;              upon exit, even if throwing
;;
;; aborted = transaction is over, it aborted
;; committed = transaction is over, it committed

(deftype transaction-type () `(member ,@+transaction-types+))
(deftype transaction-disposition () `(member ,@+transaction-dispositions+))

#|
;;; moved to pdefine.lisp
\(defvar-unbound *default-persistent-store*
  "Persistent store used for allocation if no other is specified.")

\(defun call-with-default-persistent-store (persistent-store thunk)
  (multiple-value-prog1
      (let ((*default-persistent-store* persistent-store))
        (debug-message 4 "Default persistent store is now ~s" *default-persistent-store*)
        (funcall thunk))
    (if (boundp '*default-persistent-store*)
        (debug-message 4 "Default persistent store is now ~s" *default-persistent-store*)
        (debug-message 4 "There is no default persistent store."))))
|#

;;; *current-transaction*

(defvar-unbound *current-transaction*
  "The current transaction.  Not bound if we are not in a transaction.")

;;; Transactions
(defclass transaction ()
  (
   ;; When LOCK is T, disallow modifications to this transaction.
   ;; SET when nesting transactions.
   (lock       :initform nil
               :reader transaction/locked?
               :accessor transaction/%lock)

   (disposition :accessor transaction/disposition
                :initform :running
                :type transaction-disposition)
   (reason      :accessor transaction/reason
                :initarg :reason
                :initform "Unknown"
                :type string)

   ;; The transaction in which we are nested (if we are nested).
   (parent      :accessor transaction/parent
                :initform (and (boundp '*current-transaction*)
                               *current-transaction*)
                :type (or null transaction))

   ;; Transactions dependent upon us if we have nested.
   (children    :accessor transaction/children
                :initform nil
                :type (or null transaction))

   ;; The object table contains entries for all the stores
   ;; participating in this transaction.
   (object-table :accessor transaction/object-table
                 :initarg  :object-table
                 :type     list)))

(defun transaction/lock (transaction)
  (debug-message 6 "Locking transaction ~s" transaction)
  (setf (transaction/%lock transaction) t))

(defun transaction/unlock (transaction)
  (debug-message 6 "Unlocking transaction ~s" transaction)
  (setf (transaction/%lock transaction) nil))

(defmethod initialize-instance :after ((instance transaction) &rest initargs)
  (declare (ignore initargs))
  ;; When creating a new transaction, lock the current one.
  (when (boundp '*current-transaction*)
    (assert (not (transaction/locked? *current-transaction*)))
    (transaction/lock *current-transaction*)
    (push instance (transaction/children *current-transaction*))))

;;; In each transaction there is an object table that holds
;;; the view of the persistent store that the transaction
;;; can see.  If the transaction aborts, it just discards its
;;; object table.  There needs to be an entry for each database
;;; participating in the transaction.
;;;
;;; These are copied to nested transactions, and if the nested
;;; transaction commits, copied back.

(defstruct (object-table-entry
            (:type list)
            (:copier nil)
            (:conc-name object-table-entry/)
            (:constructor %make-object-table-entry
                          (persistent-store object-map
                                            &aux
                                            (nodes-read      (make-ordered-integer-set))
                                            (nodes-written   (make-ordered-integer-set))
                                            (nodes-allocated (make-ordered-integer-set)))))
  ;; pstore *must* be first.
  (persistent-store)
  (object-map nil)
  (nodes-read)
  (nodes-written)
  (nodes-allocated))

(declaim (ftype (function (transaction persistent-store) (or null cons))
                transaction/find-object-table-entry)
         (inline transaction/find-object-table-entry))

(defun transaction/find-object-table-entry (transaction persistent-store)
  "Returns the object-table-entry associated with PERSISTENT-STORE."
  ;; Use the fact that the table is also an alist.
  ;; This has to be fast.
  (declare #.(performance-optimizations))
  (assoc persistent-store (transaction/object-table transaction) :test #'eq))

;;; These need to record what nodes are examined, written, and read
;;; in order to serialize multiple transactions.  But since they occur
;;; on *every* read, write, or cons, they have to be FAST.

(declaim (ftype (function (list persistent-object-id) t)
                object-table-entry/note-read
                object-table-entry/note-write
                object-table-entry/note-allocation)
         (inline
           object-table-entry/note-read
           object-table-entry/note-write
           object-table-entry/note-allocation))

(defun object-table-entry/note-read (object-table-entry node-id)
  "Record that node-id was referred to."
  ;; (debug-message 6 "Noting read of node ~d from ~s" node-id (object-table-entry/persistent-store object-table-entry))
  (declare #.(performance-optimizations))
  (ordered-integer-set/adjoin! (object-table-entry/nodes-read object-table-entry) node-id))

(defun object-table-entry/note-write (object-table-entry node-id)
  "Record that node-id was written to."
  ;; (debug-message 6 "Noting write of node ~d from ~s" node-id (object-table-entry/persistent-store object-table-entry))
  (declare #.(performance-optimizations))
  (ordered-integer-set/adjoin! (object-table-entry/nodes-written object-table-entry) node-id))

(defun object-table-entry/note-allocation (object-table-entry node-id)
  "Record that node-id was allocated."
  ;; (debug-message 6 "Noting allocation of node ~d from ~s" node-id (object-table-entry/persistent-store object-table-entry))
  (declare #.(performance-optimizations))
  (ordered-integer-set/adjoin! (object-table-entry/nodes-allocated object-table-entry) node-id))

(defun copy-object-table-entry (entry)
  (%make-object-table-entry
   (object-table-entry/persistent-store entry)
   (object-table-entry/object-map entry)))

(defun transaction/copy-object-table (transaction)
  (map 'list #'copy-object-table-entry (transaction/object-table transaction)))



;;; Marks transaction as aborting.
;;; Does NOT cause a transfer of control.
(defgeneric transaction/abort (transaction reason)
  (:documentation "Abort the transaction and discard the results.")
  ;; This method just marks the transaction.
  (:method ((transaction transaction) reason)
    (ecase (transaction/disposition transaction)
      (:aborting (warn  "Double aborting of transaction ~s" transaction)
                 (setf (transaction/reason transaction) reason))

      (:committing (warn "Aborting committing transaction ~s" transaction)
                   (setf (transaction/disposition transaction) :aborting
                         (transaction/reason transaction)      reason))

      (:running (setf (transaction/disposition transaction) :aborting
                      (transaction/reason transaction)      reason)))))

(defgeneric transaction/clear (transaction)
  (:documentation "Zero/null out the storage used by a transaction.")
  (:method ((transaction transaction))
    (setf (transaction/object-table transaction) nil)))

;;; Marks transaction as committed and makes it durable.
;;; Does NOT cause a transfer of control.
(defgeneric transaction/commit (transaction &optional reason)
  (:documentation "Commit the transaction and make it durable.")
  (:method ((transaction transaction) &optional reason)
    (ecase (transaction/disposition transaction)
      (:aborting (error 'changesafe-database-error
                        :format-control "Attempt to commit aborting transaction ~s"
                        :format-arguments (list transaction)))

      (:committing (warn "Double committing transaction ~s" transaction)
                   (when reason
                     (setf (transaction/reason transaction) reason)))

      (:running (setf (transaction/disposition transaction) :committing)
                (when reason
                  (setf (transaction/reason transaction) reason))))))

;;; These generic functions are called by the transaction
;;; machinery when exiting a transaction.  Use
;;; transaction/abort or transaction/commit above if you
;;; want to abort transactions from elsewhere.
(defgeneric transaction/do-abort (transaction)
  (:documentation "Perform the necessary work to abort the transaction.")
  (:method :before (transaction)
    (assert (or (eq (transaction/disposition transaction) :aborting)
                (eq (transaction/disposition transaction) :running)))

    (when (transaction/parent transaction)
      (assert (transaction/locked? (transaction/parent transaction)))))

  (:method :after (transaction)
    (transaction/clear transaction)
    (when (transaction/parent transaction)
      (transaction/unlock (transaction/parent transaction)))
    (setf (transaction/disposition transaction) :aborted)))

(defgeneric transaction/do-commit (transaction)
  (:documentation "Perform the necessary work to abort the transaction.")
  (:method :before (transaction)
    (assert (or (eq (transaction/disposition transaction) :committing)
                (eq (transaction/disposition transaction) :running)))

    (when (transaction/parent transaction)
      (assert (transaction/locked? (transaction/parent transaction)))))

  (:method :after (transaction)
    (transaction/clear transaction)
    (when (transaction/parent transaction)
      (transaction/unlock (transaction/parent transaction)))
    (setf (transaction/disposition transaction) :committed)))


(defun transaction/find-object-map-info (transaction persistent-store node-id &optional node-index)
  (declare (type transaction transaction)
           (type persistent-store persistent-store)
           (type persistent-object-id node-id)
           #.(performance-optimizations))
  (let ((object-table-entry (or (transaction/find-object-table-entry transaction persistent-store)
                                (error 'changesafe-database-error
                                       :format-control "Persistent store ~s not participating in transaction ~s"
                                       :format-arguments (list persistent-store transaction)))))
    (locally
     (declare (type list object-table-entry))
     (object-table-entry/note-read object-table-entry node-id)
     (if node-index
         (object-map/find2 (object-table-entry/object-map object-table-entry) node-id node-index)
         (object-map/find (object-table-entry/object-map object-table-entry) node-id)))))


(defun transaction/find (transaction persistent-store node-id)
  "Returns the object assocated with NODE-ID as currently visible from this transaction."
  (when (transaction/locked? transaction)
    (error 'changesafe-database-error :format-control "Attempt to read from a shadowed transaction."))

  (object-map-info/value
   (the object-map-info
     (transaction/find-object-map-info transaction persistent-store node-id))))

(defun transaction/find-root (transaction persistent-store)
  "Returns the root object of the PERSISTENT-STORE as visible from this transaction."
  (transaction/find transaction persistent-store +object-id-of-root+))

(defun transaction/allocate-vector (transaction persistent-store)
  "Return a node ID that can be used as the basis for a persistent vector."
  (when (transaction/locked? transaction)
    (error 'changesafe-database-error
           :format-control "Transaction/allocate-array on locked transaction ~s"
           :format-arguments (list transaction)))

  (let* ((object-table-entry (transaction/find-object-table-entry transaction persistent-store))
         (id (persistent-store/allocate-object-id persistent-store)))
    (object-table-entry/note-allocation object-table-entry id)
    id))

(defun transaction/vector-ref (transaction persistent-store node-id node-index)
  "Read vector element."
  (when (transaction/locked? transaction)
    (error 'changesafe-database-error
           :format-control "Transaction/vector-ref on locked transaction ~s"
           :format-arguments (list transaction)))
  (object-map-info/value
   (the object-map-info
     (transaction/find-object-map-info transaction persistent-store node-id node-index))))







(defclass read-only-transaction  (transaction)
  ()
  (:documentation "A transaction that only reads data.  Does not lock the database, but can read stale data."))

;;; READ-ONLY transaction

(defmethod transaction/do-commit ((transaction read-only-transaction))
  (let ((parent (transaction/parent transaction)))
    (dolist (entry (transaction/object-table transaction))
      (assert (ordered-integer-set/empty? (object-table-entry/nodes-written entry)))
      (assert (ordered-integer-set/empty? (object-table-entry/nodes-allocated entry)))
      (tcond ((null parent) (persistent-store/close (object-table-entry/persistent-store entry)))
            ((transaction/find-object-table-entry
              parent
              (object-table-entry/persistent-store entry))
             => (lambda (parent-entry)
                  (ordered-integer-set/union! (object-table-entry/nodes-read parent-entry)
                                              (object-table-entry/nodes-read entry))))
            (t
             (push entry (cdr (transaction/object-table parent))))))))

;; Question:  should we allow `writes' but just abort the transaction upon
;; exit?  Maybe....

(defmethod transaction/do-abort ((transaction read-only-transaction))
  (debug-message 3 "Aborting read-only transaction ~s" transaction)
  (let ((parent (transaction/parent transaction)))
    (dolist (entry (transaction/object-table transaction))
      (assert (ordered-integer-set/empty? (object-table-entry/nodes-written entry)))
      (assert (ordered-integer-set/empty? (object-table-entry/nodes-allocated entry)))
      (cond ((null parent) (persistent-store/close (object-table-entry/persistent-store entry)))
            ((transaction/find-object-table-entry
                               parent
                               (object-table-entry/persistent-store entry))
             nil)
            (t
             (push entry (cdr (transaction/object-table parent))))))))

(defclass read-write-transaction (transaction)
  ((master-cell :accessor transaction/master-cell
                :initform nil))
  (:documentation "A transaction that reads and writes data."))



(defmethod transaction/do-abort ((transaction read-write-transaction))
  (debug-message 3 "Aborting read-write transaction ~s" transaction)
  (unless (transaction/parent transaction)
    (dolist (entry (transaction/object-table transaction))
      ;; These repositories are awaiting the two phase commit.
      (persistent-store/close (object-table-entry/persistent-store entry))))
  (transaction/clear transaction))



(defclass read-cons-transaction  (transaction)
  ((master-cell :accessor transaction/master-cell))
  (:documentation "A transaction that reads data, and writes new data, but does not `modify' old data."))




(defmethod transaction/do-abort ((transaction read-cons-transaction))
  (debug-message 3 "Aborting read-cons transaction ~s" transaction)
  (let ((parent (transaction/parent transaction)))
    (when (null parent)
      (dolist (entry (transaction/object-table transaction))
        ;; These repositories are awaiting the two phase commit.
        (persistent-store/close (object-table-entry/persistent-store entry)))))
  (transaction/clear transaction))



(declaim (ftype (function (transaction) transaction-type) transaction/mode))

(defun transaction/mode (transaction)
  "Return a transaction-type describing the mode of the transaction."
  (etypecase transaction
    (read-cons-transaction :read-cons)
    (read-only-transaction :read-only)
    (read-write-transaction :read-write)))

(defun abort-current-transaction (reason)
  (transaction/abort *current-transaction* reason))

(defun transaction/find-object-map (transaction pstore)
  ;; This is called when a nested transaction is performed.
  ;; If a parent transaction has an object-map entry for the pstore,
  ;; we need to get it so that we see the modified view.

  (tcond ((null transaction)
         ;; no parent transaction had an object map entry, so
         ;; we need to use the one from the pstore
         (debug-message 5 "Using object-map entry from ~s" pstore)
         ;; bump the open count so it doesn't get closed before commit.
         (debug-message 5 "Incrementing open count on ~s, was ~d" pstore (persistent-store/open-count pstore))
         (incf (persistent-store/open-count pstore))
         (persistent-store/object-map pstore))

        ((transaction/find-object-table-entry transaction pstore)
         => (lambda (object-table-entry)
              (debug-message 5 "Using object-map entry from ~s" transaction)
              (object-table-entry/object-map object-table-entry)))

        (t
         (debug-message 5 "Looking deeper into the transaction tree.")
         (let ((inner (transaction/find-object-map (transaction/parent transaction) pstore)))
           ;; But if we find it, we have to propagate it back up the
           ;; transaction tree so all the intermediate transactions see it.
           (debug-message 5 "Installing object table entry for pstore ~s in transaction ~s" pstore transaction)
           (push (%make-object-table-entry pstore inner)
                 (cdr (transaction/object-table transaction)))
           inner))))

(defun call-with-transaction (pstore transaction-type reason receiver)
  (check-type pstore persistent-store)
  (check-type transaction-type transaction-type)
  (check-type reason string)
  ;; Ensure proper nesting.
  (when (boundp '*current-transaction*)
    (ecase (transaction/mode *current-transaction*)
      (:read-cons  (assert (or (eq transaction-type :read-cons)
                               (eq transaction-type :read-only))))
      (:read-only  (assert (eq transaction-type :read-only)))
      (:read-write t)))
  (debug-message 3 "Beginning ~:[outermost~;nested~] ~s pstore transaction on ~s."
                 (boundp '*current-transaction*)
                 transaction-type pstore)
  (let ((transaction   nil)
        (normal-return nil)
        (abort-reason  "Non-local exit from call-with-transaction."))
      (unwind-protect
          (progn
            ;; This is the only place that transactions are created.
            (setq transaction
                  (make-instance
                   (ecase transaction-type
                     (:read-only  'read-only-transaction)
                     (:read-write 'read-write-transaction)
                     (:read-cons  'read-cons-transaction))
                   :object-table  (let ((copies (when (boundp '*current-transaction*)
                                                 (transaction/copy-object-table *current-transaction*))))
                                   (if (assoc pstore copies)
                                       copies
                                         (cons (%make-object-table-entry pstore
                                                                         (transaction/find-object-map
                                                                          (if (boundp '*current-transaction*)
                                                                              *current-transaction*
                                                                              nil)
                                                                          pstore))
                                               copies)))
                   :reason reason))
            (multiple-value-prog1
             (let ((*current-transaction* transaction))
               (call-with-default-persistent-store
                pstore
                (lambda ()
                  (funcall receiver transaction))))
             (setq normal-return t)))
        "finalizing transaction"
        ;; If the transaction is null, we never even got started.
        (unless (null transaction)
          (debug-message 5 "Finishing ~s, disposition is ~s"
                         transaction
                         (transaction/disposition transaction))
          (ecase (transaction/disposition transaction)
             ;; If we return normally and no one decided to commit
             ;; or abort before now, we commit.
            (:running (if normal-return
                          (transaction/do-commit transaction)
                          (progn
                            (setf (transaction/reason transaction) abort-reason)
                            (transaction/do-abort  transaction))))
            ;; Rare case, non-local exit, but committing
            (:committing (transaction/do-commit transaction))
            (:aborting   (transaction/do-abort  transaction)))
          (debug-message 4 "returning from call-with-transaction ~s, disposition is ~s"
                         transaction
                         (transaction/disposition transaction))))))


(defun persistent-object/find (persistent-store node-id)
  (cond ((= node-id +object-id-of-nil+) nil)
        ((= node-id +object-id-of-zero+) 0)
        (t (transaction/find *current-transaction* persistent-store node-id))))

(defsubst persistent-object/allocate-vector (persistent-store)
  (transaction/allocate-vector *current-transaction* persistent-store))

(defsubst persistent-object/vector-ref (persistent-store node-id node-index)
  (transaction/vector-ref *current-transaction* persistent-store node-id (+ node-index 2)))

(defsubst persistent-object/find-object-map-info (persistent-store node-id)
  (transaction/find-object-map-info *current-transaction* persistent-store node-id))
