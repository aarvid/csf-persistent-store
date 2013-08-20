;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;; $$$ new file pclass-ptxn, stuff in  pclass that depends on ptxn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Jrm's log-based persistent store


(in-package #:csf-persistent-store)

(proclaim (standard-optimizations))




;;; The persistent slots in a persistent object actually contain the
;;; node-id of the value that is supposed to be there.  This is for
;;; two reasons:  to break circularity and to allow atomic rollback
;;; via the object map.
(defmethod slot-value-using-class ((class persistent-standard-class)
                                   (object persistent-standard-object)
                                   slot)
  (declare #.(performance-optimizations))
  (let ((slot-descriptor (find slot (compute-slots class)
                               :key #'slot-definition-name)))
    (if (and (typep slot-descriptor 'persistent-slot-definition)
             *dereference-persistent-slots*
             (boundp '*current-transaction*))
        (persistent-object/find (persistent-standard-object/pstore object)
                                (call-next-method))
        (make-instance 'persistent-node-id
                       :node-id (call-next-method)
                       :node-index +node-index-scalar+))))

(defmethod (setf slot-value-using-class) (new-value (class persistent-standard-class)
                                                         (object persistent-standard-object)
                                                         slot)
  (declare #.(performance-optimizations))
  (let ((slot-descriptor (find slot (compute-slots class) :key #'slot-definition-name)))
    (if (typep slot-descriptor 'persistent-slot-definition)
        (error "Persistent slot ~s in ~s is immutable." slot-descriptor object)
        (call-next-method))))

(declaim (ftype (function (persistent-store t) non-negative-integer) slot-value->persistent-node)
         (inline slot-value->persistent-node))

;;; depends on serial also.
(defun slot-value->persistent-node (persistent-store value)
  "Given the value to place in a persistent node, return
   the node-id for the persistent version of the value.

   If value is a persistent-instance, just get the node-id,
   but if value is a lisp object, persist it."
  (declare #.(performance-optimizations))
  (if (typep value 'persistent-standard-object)
      (progn
        (assert (eq (persistent-standard-object/pstore value) persistent-store))
        (persistent-standard-object/node-id value))
      (persistent-object/save value persistent-store)))




;;; !!! clos problem. need to substitute (slot-unbound-value)
(defun compute-persistent-slot-initargs (class persistent-store initargs)
  "Scan over the persistent effective slots in CLASS,
   determine the value to be assigned to each slot, either
   from the initargs or from the initfunction, then
   using the persistent-initarg as a key, construct a
   plist for use in the persistent initializer and in
   the inner call to shared-initialize."
  (declare #.(performance-optimizations))
  (let ((result nil))
    (symbol-macrolet ((!slot-unbound! #+sbcl sb-pcl:+slot-unbound+
                                      #+lispworks (clos::slot-unbound-value)
                                      #-(or sbcl lispworks)
                                      '..slot-unbound..)) ;sbcl uses this.
     (iterate (((slot-initargs slot-initfunction slot-persistent-initarg)
                (map-fn '(values t t symbol)
                        #'effective-slot-initialization-info
                        (scan-class-persistent-effective-slots class))))
              (let ((initial-value (slot-initial-value initargs slot-initargs
                                                       slot-initfunction
                                                       !slot-unbound!)))
                (unless (eq initial-value !slot-unbound!)
                  (push slot-persistent-initarg result)
                  (push (slot-value->persistent-node persistent-store initial-value)
                        result)))))
    (nreverse result)))


;; Override the primary shared-instance method
;; in order to deal with persistent slots.
;; This actually has to perform because instances are
;; immutable:  we will be consing more of them.
(defmethod shared-initialize ((instance persistent-standard-object) slot-names
                                   &rest initargs
                                   &key persistent-store node-id node-index
                                   &allow-other-keys)

  (if (eq instance *restoring-instance*)
      (call-next-method)
      ;; If we are being called from elsewhere,
      ;; we have to wrap the initargs and initforms
      ;; in persistent-objects and create an initializer
      ;; for this object.
      (let* ((class (class-of instance))

             (init-plist (compute-persistent-slot-initargs class
                                                           (or persistent-store  *default-persistent-store*)
                                                           initargs))

             (node-id (persistent-object/save
                       (make-initializer class
                                         (class-schema-version class)
                                         init-plist)
                       (or persistent-store  *default-persistent-store*)
                       node-id)))

        (apply #'call-next-method instance slot-names (nconc init-plist initargs))

        (setf (persistent-standard-object/node-id instance) node-id)
        (setf (persistent-standard-object/node-index instance) node-index)
        (setf (object-map-info/%cached-value
               (persistent-object/find-object-map-info
                (or persistent-store  *default-persistent-store*)  node-id))
              instance)

        instance)))

