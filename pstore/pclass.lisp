;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Persistent classes for the log-based persistent store.
(in-package #:csf-persistent-store)

;; (proclaim (standard-optimizations))

#|(eval-when (:compile-toplevel :load-toplevel)
  (export '(
            persistent-node-id
            persistent-standard-class
            persistent-standard-object
            persistent-slot-definition
            remake-instance
            slot-initial-value
            slot-definition-standard-initargs)))|#

(defclass persistent-standard-class (standard-class)
  ((schema-version :initarg :schema-version
                   :initform (error "Class initialization :schema-version omitted.")
                   :reader class-schema-version
                   :type (integer 0 *)))
  (:documentation "The metaclass for persistent objects."))

;; Oh well.
;; Have to suppress optimization of slot-value and (setf slot-value)
;; because we have modified those methods.
#+lispworks
(defmethod clos::can-optimize-access-using-class ((class persistent-standard-class))
  nil)

(defun slot-name->persistent-initarg (slot-name)
  (intern (concatenate 'string
                       "PERSISTENT-INITARG-FOR-"
                       (symbol-name slot-name))
          (find-package :csf-utility)))

;;; Some slots are persistent and some are transient only.  The
;;; persistent slots will inherit from this class of slot definition
;;; so we can tell them apart.
(defclass persistent-slot-definition (c2mop:standard-slot-definition)
  ((persistent-initarg :initarg :persistent-initarg
                       :reader persistent-slot-definition/persistent-initarg)))

(defmethod shared-initialize :around ((instance persistent-slot-definition) slot-names
                                           &rest initargs &key name &allow-other-keys)
  (apply #'call-next-method instance slot-names
         :persistent-initarg (slot-name->persistent-initarg name)
         initargs))

(defmethod c2mop:slot-definition-initargs ((slot-definition persistent-slot-definition))
  (cons (persistent-slot-definition/persistent-initarg slot-definition)
        (call-next-method)))

;;; !!! sbcl thinks this is new
;;;     not sure what this does.
;;;     added defgeneric to avoid warning.
(defgeneric slot-definition-standard-initargs (slot-definition-standard-initargs))
(defmethod slot-definition-standard-initargs ((slot-definition persistent-slot-definition))
  (cdr (c2mop:slot-definition-initargs slot-definition)))

;;; In theory, the inheritance list ought to be
;;; (persistent-slot-definition c2mop:direct-slot-definition) but there
;;; is apparently an initialization on
;;; c2mop:standard-direct-slot-definition that we need.
(defclass persistent-direct-slot-definition (persistent-slot-definition
                                             c2mop:standard-direct-slot-definition)
  ())

;;; $$$ see closer-mop to fix arguments
(defmethod c2mop:direct-slot-definition-class
    ((class persistent-standard-class) &rest initargs)
  (destructuring-bind
      (&key transient-only &allow-other-keys)
      (c2mop:fix-slot-initargs initargs)
    (if transient-only
        (find-class 'c2mop:standard-direct-slot-definition)
        (find-class 'persistent-direct-slot-definition))))

(defclass persistent-effective-slot-definition (persistent-slot-definition
                                                c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:effective-slot-definition-class
    ((class persistent-standard-class) &rest initargs)
  (if (typep (car initargs) 'persistent-slot-definition)
      (find-class 'persistent-effective-slot-definition)
      (find-class 'c2mop:standard-effective-slot-definition)))

;;; $$$ class-effective-slots -> compute-slots
(defun scan-class-persistent-effective-slots (class)
  (declare (optimizable-series-function))
  (choose-if (lambda (slot)
               (typep slot 'persistent-slot-definition))
             (scan 'list (c2mop:compute-slots class))))

;;; The :transient-only option is rejected during class slot
;;; canonicalization.  This function fixes that by stripping off the
;;; :transient-only option before canonicalization and replacing it
;;; after.
#+lispworks
(defmethod c2mop::canonicalize-defclass-slot ((prototype persistent-standard-class) slot)
  ;; make sure the slot is a list.
  (unless (consp slot)
    (setq slot (list slot)))
  (let ((transient-only (or (getf (cdr slot) :transient-only)
                            nil)))
    (remf (cdr slot) :transient-only)
  `(,@(call-next-method prototype slot)
      :transient-only ,transient-only)))

;;; The :schema-version initializer for persistent metaclasses is
;;; rejected during class canonicalization in lispworks.  This
;;; function fixes that by stripping off the :schema-version option
;;; before canonicalizing the other options, then replacing it.
#+lispworks
(defmethod c2mop::canonicalize-class-options ((prototype persistent-standard-class) options)
  (let ((schema-version-option (find :schema-version options :key #'car)))
    `(,@(call-next-method prototype (delete schema-version-option options))
        ,@(when schema-version-option
            schema-version-option))))

;;; All persistent objects inherit from persistent-standard-object.
(defclass persistent-standard-object (standard-object)
  ((pstore :accessor persistent-standard-object/pstore
           :initform *default-persistent-store*)
   (node-id :accessor persistent-standard-object/node-id)
   (node-index :accessor persistent-standard-object/node-index)))

(defmethod objects-equalp ((left persistent-standard-object) right) nil)
(defmethod objects-equalp (left (right persistent-standard-object)) nil)

(defmethod objects-equalp ((left persistent-standard-object) (right persistent-standard-object))
  (or (eq left right)
      (and (eq (persistent-store/guid (persistent-standard-object/pstore left))
               (persistent-store/guid (persistent-standard-object/pstore right)))
           (= (persistent-standard-object/node-id left)
              (persistent-standard-object/node-id right))
           (= (persistent-standard-object/node-index left)
              (persistent-standard-object/node-index right)))))

;;; Only persistent-standard-object and its subclasses are valid
;;; superclasses for persistent-objects
(defmethod c2mop:validate-superclass ((class persistent-standard-class)
                                      superclass)
  (subtypep superclass 'persistent-standard-object))

;;; !!! needed this for persistent-vector, is this correct?
(defmethod closer-mop:validate-superclass ((sub persistent-standard-class)
                                           (super closer-mop:standard-class))
  t)

;;; If you leave the direct superclass list blank, it will
;;; be defaulted to persistent-standard-object.
(defmethod shared-initialize ((class persistent-standard-class) slot-names &rest initargs)
  "When initializing a persistent-standard-class, ensure that
   persistent-standard-object appears in the direct-superclasses."
  (let ((direct-superclasses (getf initargs :direct-superclasses)))
    (when (or (null direct-superclasses)
              (and (null (cdr direct-superclasses))
                   (eq (car direct-superclasses) (find-class 'standard-object))))
      (setf (getf initargs :direct-superclasses) (list (find-class 'persistent-standard-object))))
    (call-next-method)))




(defvar *dereference-persistent-slots* T
  "When T, persistent slot refs return the object rather than the OID.")

(defclass persistent-node-id ()
  ((node-id :initarg :node-id :reader persistent-node-id/number)
   (index   :initarg :node-index :reader persistent-node-id/index))
  (:documentation "What gets returned from a slot if there isn't a transaction in progress."))

(defmethod print-object ((object persistent-node-id) stream)
  (print-unreadable-object (object stream :type t)
    (princ (persistent-node-id/number object) stream)
    (unless (= (persistent-node-id/index object) +node-index-scalar+)
      (write-char #\[ stream)
      (princ (persistent-node-id/index object) stream)
      (write-char #\] stream))))



(defmethod (setf c2mop:slot-value-using-class) (new-value (class persistent-standard-class)
                                                         (object persistent-standard-object)
                                                         slot)
  (declare #.(performance-optimizations))
  (let ((slot-descriptor (find slot (c2mop:compute-slots class) :key #'c2mop:slot-definition-name)))
    (if (typep slot-descriptor 'persistent-slot-definition)
        (error "Persistent slot ~s in ~s is immutable." slot-descriptor object)
        (call-next-method))))

(defvar *restoring-instance* nil
  "Bound to an instance being restored.")

(declaim (ftype (function (persistent-slot-definition)
                          (values list t symbol)) effective-slot-initialization-info)
         (inline effective-slot-initialization-info))

(defun effective-slot-initialization-info (slot)
  "Return 3 values of interest in initializing this slot:

   First, the user-supplied initargs for this slot.  These will
   be matched against the initargs supplied to shared-initialize.

   Second, the initfunction, which will be called if none of the
   initargs match.

   Third, the persistent-initarg which will be passed to the next
   layer down to stuff in a value."
  (declare #.(performance-optimizations))
  (let ((initargs (c2mop:slot-definition-initargs slot)))
    (values (cdr initargs)
            (c2mop:slot-definition-initfunction slot)
            (car initargs))))

(declaim (ftype (function (list list t t) t) slot-initial-value)
         (inline slot-initial-value))

(defun slot-initial-value (initargs slot-initargs slot-initfunction default)
  "Finds the leftmost initarg that matches one of SLOT-INITARGS,
   or invokes SLOT-INITFUNCTION to determine what the initial value
   of a slot should be.  IF there is no SLOT-INITFUNCTION, default is
   returned.

   Performance critical!"
  (declare #.(performance-optimizations))
  (do* ((plist initargs (cddr plist))
        (key (car plist) (car plist))
        (value (cadr plist) (cadr plist)))
      ((null plist) (if slot-initfunction
                        (funcall slot-initfunction)
                        default))
    (when (member key slot-initargs :test #'eq)
      (return-from slot-initial-value value))))








(defmethod restore-instance ((class persistent-standard-class) schema persistent-store node-id node-index init-plist)
  "The standard restore method for a persistent object."
  (cond ((< (class-schema-version class) schema)
         (error 'changesafe-schema-mismatch
                :format-control "Schema of object in database (~d) appears newer than that in memory (~d)."
                :format-arguments (list schema (class-schema-version class))))
        ;; This should not happen because when we upgrade the schema we're supposed
        ;; to include schema migration code.  But if we didn't we end up here.
        ((> (class-schema-version class) schema)
         (warn 'changesafe-schema-upgrade
               :format-control "Schema of object in database (~d) is older than that in memory (~d)."
               :format-arguments (list schema (class-schema-version class))))
        ;; If it is the same, we're ok.
        (t nil))

  (let ((*restoring-instance* (allocate-instance class nil)))
    (setf (persistent-standard-object/pstore *restoring-instance*) persistent-store)
    (setf (persistent-standard-object/node-id *restoring-instance*) node-id)
    (setf (persistent-standard-object/node-index *restoring-instance*) node-index)
    (apply #'c2mop::shared-initialize *restoring-instance* t init-plist)
    *restoring-instance*))



(defgeneric remake-instance (original-instance &rest initargs)
  (:documentation "Recreates original-instance with new initargs
                   installing the new instance in the object map at the old location.")
  (:method ((original-instance persistent-standard-object) &rest initargs)
    (debug-message 5 "Remaking ~s with ~s" original-instance initargs)
    (let ((class (class-of original-instance)))
      (apply #'c2mop::shared-initialize (allocate-instance class) t
             :persistent-store (persistent-standard-object/pstore original-instance)
             :node-id (persistent-standard-object/node-id original-instance)
             (append initargs
                     (multiple-value-bind (slot-persistent-initargs values)
                         (map-fn '(values symbol integer)
                                 (lambda (persistent-slot)
                                   (values (car (c2mop:slot-definition-initargs persistent-slot))
                                           (slot-value original-instance
                                                       (c2mop:slot-definition-name persistent-slot))))
                                 (choose-if (lambda (persistent-slot)
                                              (c2mop:slot-boundp-using-class class original-instance
                                                                             (c2mop:slot-definition-name persistent-slot)))
                                            (scan-class-persistent-effective-slots class)))
                       (collect-plist
                        slot-persistent-initargs
                        values)))))))
