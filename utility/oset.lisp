;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002, 2003 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 "New BSD"
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:
;;;;
;;;; Ordered set abstraction
;;;;
;;;; Many set operations can be done more efficiently if there is an
;;;; ordering to the set elements.  OSETS are implemented using Red-Black
;;;; trees, so testing for membership in a set is O(log n).
;;;;
;;;; The equal? and the less? operators are predicates on the members of
;;;; the ordered set.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:csf-utility)

(proclaim (standard-optimizations))



(defsubst make-ordered-set (equal? less?)
  "Make a new, empty ordered set with equality predicate EQUAL? and
   ordering predicate LESS?"
  (make-tree equal? less?))

(defsubst make-ordered-integer-set ()
  "Make a new, empty ordered set with equality predicate = and <"
  (make-integer-tree))

(defsubst ordered-set/empty? (oset)
  "Return T iff there are no elements in OSET."
  (rb-tree/empty? oset))

(defsubst ordered-integer-set/empty? (oset)
  "Return T iff there are no elements in OSET."
  (integer-rb-tree/empty? oset))

(defsubst ordered-set/equal? (oset)
  "Return the equality predicate of OSET."
  (rbtree-key=? oset))

(defsubst ordered-set/less? (oset)
  "Return the ordering predicate of OSET."
  (rbtree-key<? oset))

(defsubst ordered-set/adjoin! (oset element)
  "Modify OSET to add ELEMENT."
  (rb-tree/insert! oset element t))

(defsubst ordered-integer-set/adjoin! (oset element)
  "Modify OSET to add ELEMENT."
  (integer-rb-tree/insert! oset element t))

(defun ordered-set/adjoin-list! (oset elements)
  "Modify OSET to contain every element in the list ELEMENTS."
  (mapc (lambda (element)
            (ordered-set/adjoin! oset element))
        elements))


(defun ordered-set/union! (target-oset source-oset)
  "Modify TARGET-OSET to be the union of TARGET-OSET and SOURCE-OSET."
  (iterate ((key (rb-tree/scan-keys source-oset)))
    (rb-tree/insert! target-oset key t)))


(defun ordered-integer-set/union! (target-oset source-oset)
  "Modify TARGET-OSET to be the union of TARGET-OSET and SOURCE-OSET."
  (iterate ((key (integer-rb-tree/scan-keys source-oset)))
    (integer-rb-tree/insert! target-oset key t)))

(defsubst ordered-set/member? (oset element)
  "Return T iff ELEMENT is a member of OSET."
  (rb-tree/lookup oset element nil))

(defsubst ordered-integer-set/member? (oset element)
  "Return T iff ELEMENT is a member of OSET."
  (integer-rb-tree/lookup oset element nil))

(defun ordered-set/scan (oset)
  (declare (optimizable-series-function))
  (rb-tree/scan-keys oset))


(defsubst ordered-set->list (oset)
  "Return a LIST representing the elements in OSET."
  (collect 'list (ordered-set/scan oset)))

(defsubst ordered-set/pick! (oset)
  "Return an element from OSET and modify OSET to no longer
   contain that element."
  (rb-tree/pick! oset))



(defun ordered-set/map (func oset)
  "Collect in a list the result applying func to each member of OSET."
  (collect 'list (map-fn 't func (rb-tree/scan-keys oset))))

(defsubst ordered-set/foreach (oset function)
  "Apply FUNCTION to each element in OSET."
  (rb-tree/foreach-key oset function))


(defun ordered-set/intersection (left right)
  "Return a new OSET that contains only those elements in both LEFT and RIGHT."
  (let ((result (make-ordered-set (ordered-set/equal? left) (ordered-set/less? left))))
    (iterate ((element (choose-if (lambda (element)
                                    (ordered-set/member? right element))
                                  (ordered-set/scan left))))
      (ordered-set/adjoin! result element))
    result))


(defun ordered-set/difference (left right)
  "Return a new OSET that contains only those elements in LEFT that are not in RIGHT."
  (let ((result (make-ordered-set (ordered-set/equal? left) (ordered-set/less? left))))
    (iterate ((element (choose-if (lambda (element)
                                     (not (ordered-set/member? right element)))
                                   (ordered-set/scan left))))
      (ordered-set/adjoin! result element))
    result))
