;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;; $$$ new file objmap-serial, stuff in objmap that depends on serial
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Jrm's log-based persistent store


(in-package #:csf-persistent-store)

(proclaim (standard-optimizations))


(defun object-map-info/read (symbol-table stream)
  (let* ((encoded-node-id  (read-fixnum stream))
         (node-index       (read-fixnum stream))
         (location         (read-unsigned32 stream))
         (value            (unless (zerop location)
                             (deserialize stream symbol-table location))))
    (%make-object-map-info encoded-node-id node-index location value)))

(defun restore-from-initializer (persistent-store node-id node-index initializer)
  "Invoked to re-create a persistent object from its initial form."
  (restore-instance
   (initializer/class initializer)
   (initializer/schema-version initializer)
   persistent-store
   node-id
   node-index
   (initializer/init-plist initializer)))


(defun fetch-object-map (persistent-store symbol-table stream location)
  "Given a location within an input-stream, return the object map node at
   that location.  This is used to fetch the root node of the
   object map."
  (debug-message 5 "Reloading object map.")
  ;; Fetching the root node will suck in the rest of the nodes.
  (let ((object-map
           (fetch-persistent-node symbol-table stream location #'object-map-info/read)))
    ;; Once the object map is loaded,
    ;; make a second pass to restore persistent objects from their
    ;; initializers.
    (debug-message 5 "Reconstructing persistent objects.")
    (let ((count 0))
      (object-map/for-each
       (lambda (object-map-info)
         (when (initializer? (object-map-info/value object-map-info))
           (setf (object-map-info/%cached-value object-map-info)
                 (restore-from-initializer
                  persistent-store
                  (object-map-info/node-id object-map-info)
                  (object-map-info/node-index object-map-info)
                  (object-map-info/value object-map-info)))
           (incf count)))
       object-map)
      (debug-message 4 "Reconstructed ~d persistent objects." count))
    object-map))
