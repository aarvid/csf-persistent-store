;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;; $$$ new file ptxn-serial, stuff in ptxn that depends on serial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Jrm's log-based persistent store


(in-package #:csf-persistent-store)

(proclaim (standard-optimizations))


;;; !!! depnds on serial.lisp

(defun transaction/vector-set (transaction persistent-store node-id node-index object)
  "Assign vector element."
  (when (transaction/locked? transaction)
    (error 'changesafe-database-error
           :format-control "Transaction/vector-set on locked transaction ~s"
           :format-arguments (list transaction)))
  (let* ((object-table-entry (transaction/find-object-table-entry transaction persistent-store))
         (stream (persistent-store/log-stream persistent-store))
         (symtab (persistent-store/symbol-table persistent-store))
         (location (persistent-store/next-location persistent-store)))
    (serialize object stream symtab)
    (object-table-entry/note-write object-table-entry node-id)
    (setf (object-table-entry/object-map object-table-entry)
          (object-map/add2 (persistent-store/log-stream persistent-store)
                           (object-table-entry/object-map object-table-entry)
                           node-id
                           node-index
                           location
                           object))
    object))


(defun transaction/save (transaction object persistent-store node-id)
  (when (transaction/locked? transaction)
    ;; Hey, leave this alone, bub!
    (error 'changesafe-database-error
           :format-control "Transaction/save called on locked transaction ~s"
           :format-arguments (list transaction)))

  (let* ((object-table-entry (transaction/find-object-table-entry transaction persistent-store))
         (stream (persistent-store/log-stream persistent-store))
         (symtab (persistent-store/symbol-table persistent-store))
         (id (or node-id
                 (persistent-store/allocate-object-id persistent-store)))
         (location (persistent-store/next-location persistent-store)))
    (debug-message 6 "Writing object ~s to ~s, node ~s" object persistent-store id)
    (serialize object stream symtab)

    (if node-id
        (object-table-entry/note-write object-table-entry node-id)
        (object-table-entry/note-allocation object-table-entry id))

    (setf (object-table-entry/object-map object-table-entry)
          (object-map/add (persistent-store/log-stream persistent-store)
                          (object-table-entry/object-map object-table-entry)
                          id
                          location
                          object))
    id))

(defun persistent-object/save (object persistent-store &optional node-id)
  (cond ((and (null node-id) (null object)) +object-id-of-nil+)
        ((and (null node-id) (eql object 0)) +object-id-of-zero+)
        (t (transaction/save *current-transaction* object persistent-store node-id))))


(defun transaction/initialize-master-entry (master-entry)
  ;; Set up the transaction for a two-phase commit.
  ;; This is done at allocation time unless it can be
  ;; proven that no other persistent stores will be involved.
  (let* ((persistent-store (object-table-entry/persistent-store master-entry))
         (symtab           (persistent-store/symbol-table persistent-store))
         (stream           (persistent-store/log-stream persistent-store))
         (node-id          (persistent-store/allocate-object-id persistent-store))
         (location         (persistent-store/next-location persistent-store))
         (cell             (cons nil nil)))
    (debug-message 5 "Initializing synchronization cell.")
    (serialize cell stream symtab)
    (object-table-entry/note-allocation master-entry node-id)
    (setf (object-table-entry/object-map master-entry)
          (object-map/add (persistent-store/log-stream persistent-store)
                          (object-table-entry/object-map master-entry)
                          node-id
                          location
                          cell))
    ;; We must immediately commit this cell so it can be used
    ;; if the transaction fails later during the two-phase commit.
    (persistent-store/commit persistent-store "Allocate record for two-phase-commit.")

    node-id))


;;; READ-WRITE transaction
(defmethod initialize-instance :after ((transaction read-write-transaction) &rest ignore)
  (declare (ignore ignore)) ;;  $$$
  (tcond ((transaction/parent transaction)
         => (lambda (parent)
              (setf (transaction/master-cell transaction) (transaction/master-cell parent))))
        (t (let* ((master-entry (car (transaction/object-table transaction)))
                  (guid (persistent-store/guid (object-table-entry/persistent-store master-entry)))
                  (master-node-id (transaction/initialize-master-entry master-entry)))
             (setf (transaction/master-cell transaction)
                   (cons guid master-node-id))
             (debug-message 5 "Master cell for transaction ~s is ~s"
                            transaction (transaction/master-cell transaction))))))

;;; READ-CONS transaction
(defmethod initialize-instance :after ((transaction read-cons-transaction) &rest ignore)
  (declare (ignore ignore)) ;;  $$$
  (tcond ((transaction/parent transaction)
         => (lambda (parent)
              (setf (transaction/master-cell transaction) (transaction/master-cell parent))))
        (t (let* ((master-entry (car (transaction/object-table transaction)))
                  (guid (persistent-store/guid (object-table-entry/persistent-store master-entry)))
                  (master-node-id (transaction/initialize-master-entry master-entry)))
             (setf (transaction/master-cell transaction)
                   (cons guid master-node-id))
             (debug-message 5 "Master cell for transaction ~s is ~s"
                            transaction (transaction/master-cell transaction))))))



(defmethod transaction/do-commit ((transaction read-write-transaction))
  (let ((parent (transaction/parent transaction)))
    (if (null parent)
        (let* ((master-cell       (transaction/master-cell transaction))
               (master-guid       (and master-cell (car master-cell)))
               (master-node-id    (and master-cell (cdr master-cell)))
               (object-table      (transaction/object-table transaction))
               (master-entry      (and master-cell (car object-table)))
               (master-store      (and master-cell (object-table-entry/persistent-store master-entry)))
               (other-store-entries (if master-cell
                                        (cdr object-table)
                                        object-table)))
          (debug-message 5 "Transaction/commit of ~s" transaction)
          (debug-message 5 "Parent is NIL, master-cell is ~s" master-cell)
          (when master-cell
            (assert (eq (persistent-store/guid master-store) master-guid)));; sanity check
          (dolist (entry other-store-entries)
            (unless (eq (persistent-store/open-mode (object-table-entry/persistent-store entry)) :read-only)
              ;; Write the updated object map to the pstore.
              (setf (persistent-store/object-map (object-table-entry/persistent-store entry))
                    (object-table-entry/object-map entry))
              (persistent-store/commit (object-table-entry/persistent-store entry)
                                       (transaction/reason transaction)
                                       :transaction-master master-cell))
            (persistent-store/close (object-table-entry/persistent-store entry)))

          (when master-cell
            ;; Now toggle the master cell and commit it.
            (let ((stream    (persistent-store/log-stream master-store))
                  (symtab    (persistent-store/symbol-table master-store))
                  (location  (persistent-store/next-location master-store))
                  (cell      (cons t nil)))
              (serialize cell stream symtab)
              (setf (persistent-store/object-map master-store)
                    (object-map/add (persistent-store/log-stream master-store)
                                    (object-table-entry/object-map master-entry)
                                    master-node-id
                                    location
                                    cell))
              (persistent-store/commit master-store (transaction/reason transaction))
              (persistent-store/close master-store))))

        (dolist (entry (transaction/object-table transaction))
          ;; Propagate the new state up to the parent.
          (let ((parent-entry (transaction/find-object-table-entry
                               parent
                               (object-table-entry/persistent-store entry))))
            (if (null parent-entry)
                ;; easy case, no parent entry.  Just re-use this one.
                (push entry (cdr (transaction/object-table (transaction/parent transaction))))
                ;; Hard case, need to merge the entries.
                (progn
                  (debug-message 5 "Propagating nested transaction info for ~s from ~s to parent ~s"
                                 (object-table-entry/persistent-store entry)
                                 transaction parent)
                  (setf (object-table-entry/object-map parent-entry) (object-table-entry/object-map entry))
                  (ordered-integer-set/union! (object-table-entry/nodes-read parent-entry)
                                              (object-table-entry/nodes-read entry))
                  (ordered-integer-set/union! (object-table-entry/nodes-written parent-entry)
                                              (object-table-entry/nodes-written entry))
                  (ordered-integer-set/union! (object-table-entry/nodes-allocated parent-entry)
                                              (object-table-entry/nodes-allocated entry)))))))))



(defmethod transaction/do-commit ((transaction read-cons-transaction))
  (let ((parent (transaction/parent transaction)))
    (if (null parent)
        (let* ((master-cell       (transaction/master-cell transaction))
               (master-guid       (and master-cell (car master-cell)))
               (master-node-id    (and master-cell (cdr master-cell)))
               (object-table      (transaction/object-table transaction))
               (master-entry      (and master-cell (car object-table)))
               (master-store      (and master-cell (object-table-entry/persistent-store master-entry)))
               (other-store-entries (if master-cell
                                        (cdr object-table)
                                        object-table)))
          (debug-message 5 "Transaction/commit of ~s" transaction)
          (debug-message 5 "Parent is NIL, master-cell is ~s" master-cell)
          (when master-cell
            (assert (eq (persistent-store/guid master-store) master-guid)));; sanity check
          (dolist (entry other-store-entries)
            (unless (eq (persistent-store/open-mode (object-table-entry/persistent-store entry)) :read-only)
              ;; Write the updated object map to the pstore.
              (setf (persistent-store/object-map (object-table-entry/persistent-store entry))
                    (object-table-entry/object-map entry))
              (persistent-store/commit (object-table-entry/persistent-store entry)
                                       (transaction/reason transaction)
                                       :transaction-master master-cell))
            (persistent-store/close (object-table-entry/persistent-store entry)))

          (when master-cell
            ;; Now toggle the master cell and commit it.
            (let ((stream    (persistent-store/log-stream master-store))
                  (symtab    (persistent-store/symbol-table master-store))
                  (location  (persistent-store/next-location master-store))
                  (cell      (cons t nil)))
              (serialize cell stream symtab)
              (setf (persistent-store/object-map master-store)
                    (object-map/add (persistent-store/log-stream master-store)
                                    (object-table-entry/object-map master-entry)
                                    master-node-id
                                    location
                                    cell))
              (persistent-store/commit master-store (transaction/reason transaction))
              (persistent-store/close master-store))))

        (dolist (entry (transaction/object-table transaction))
          (assert (ordered-integer-set/empty? (object-table-entry/nodes-written entry)))
          ;; Propagate the new state up to the parent.
          (let ((parent-entry (transaction/find-object-table-entry
                               parent
                               (object-table-entry/persistent-store entry))))
            (if (null parent-entry)
                ;; easy case, no parent entry.  Just re-use this one.
                (push entry (cdr (transaction/object-table (transaction/parent transaction))))
                ;; Hard case, need to merge the entries.
                (progn
                  (debug-message 5 "Propagating nested transaction info for ~s from ~s to parent ~s"
                                 (object-table-entry/persistent-store entry)
                                 transaction parent)
                  (setf (object-table-entry/object-map parent-entry) (object-table-entry/object-map entry))
                  (ordered-integer-set/union! (object-table-entry/nodes-read parent-entry)
                                      (object-table-entry/nodes-read entry))
                  (ordered-integer-set/union! (object-table-entry/nodes-allocated parent-entry)
                                      (object-table-entry/nodes-allocated entry)))))))))


(defsubst persistent-object/vector-set (persistent-store node-id node-index value)
  (transaction/vector-set *current-transaction* persistent-store node-id (+ node-index 2) value))

