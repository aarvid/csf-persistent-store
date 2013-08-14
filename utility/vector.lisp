;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 (New BSD)
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:csf-utility)


(proclaim (performance-optimizations))

(declaim
 (ftype (function (array-index) simple-vector-1b)  simple-vector-1b-allocate)
 (ftype (function (array-index) simple-vector-8b)  simple-vector-8b-allocate)
 (ftype (function (array-index) simple-vector-32b) simple-vector-32b-allocate)
 (ftype (function (array-index) simple-string-8b)  simple-string-8b-allocate)
 (ftype (function (simple-vector-8b  array-index) (unsigned-byte  8)) simple-vector-8b-ref)
 (ftype (function (simple-vector-32b array-index) (unsigned-byte 32)) simple-vector-32b-ref)
 (inline simple-vector-8b-ref
         simple-vector-32b-ref
         (setf simple-vector-8b-ref)
         (setf simple-vector-32b-ref)))

(defun simple-vector-1b-allocate (size)
  (make-array size :element-type '(unsigned-byte 1)))

(defun simple-vector-8b-allocate (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defun simple-vector-32b-allocate (size)
  (make-array size :element-type '(unsigned-byte 32)))

(defun simple-string-8b-allocate (size)
  (make-array size :element-type 'char-8b))


;; $$ this had a bug in declare type
(defmacro simple-vector-8b-length (simple-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
     (LET ((VECTOR ,simple-vector))
       (DECLARE (TYPE SIMPLE-VECTOR-8B VECTOR)
                ,(performance-optimizations))
       (LENGTH VECTOR))))


;; l229
(defmacro simple-string-length (simple-string)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((SIMPLE-STRING ,simple-string))
        (DECLARE (TYPE SIMPLE-STRING SIMPLE-STRING)
                ,(performance-optimizations))
        (LENGTH SIMPLE-STRING))))

(defun simple-vector-8b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-vector-8b simple-vector)
           (type array-index offset)
           )
  (the (unsigned-byte 8) (aref simple-vector offset)))

(defun simple-vector-32b-ref (simple-vector offset)
  (declare #.(performance-optimizations)
           (type simple-vector-32b simple-vector)
           (type array-index offset)
           )
  (the (unsigned-byte 32) (aref simple-vector offset)))

(defsetf simple-vector-8b-ref (simple-vector offset) (new-value)
  `(LET ((SIMPLE-VECTOR ,simple-vector)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-VECTOR-8B SIMPLE-VECTOR)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE (UNSIGNED-BYTE 8) NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-VECTOR OFFSET) NEW-VALUE)))

(defsetf simple-vector-32b-ref (simple-vector offset) (new-value)
  `(LET ((SIMPLE-VECTOR ,simple-vector)
         (OFFSET ,offset)
         (NEW-VALUE ,new-value))
        (DECLARE ,(performance-optimizations)
                 (TYPE SIMPLE-VECTOR-32B SIMPLE-VECTOR)
                 (TYPE ARRAY-INDEX OFFSET)
                 (TYPE (UNSIGNED-BYTE 32) NEW-VALUE)
                 )
        (SETF (AREF SIMPLE-VECTOR OFFSET) NEW-VALUE)))


;;; l362
;;; Vector moving is critical.  This function, DEFINE-SUBVECTOR-MOVER, defines functions that
;;; can move simple vector elements as quickly as possible.
;;; As an example,
;;; (define-subvector-mover %subvector-move t)
;;; generates 4 functions.  In all these functions, src-start and dest-start
;;; are positive and inclusive, src-limit is positive and exclusive.  The destination
;;; vector must have a size >= (+ (- src-limit src-start) dest-start) or you will run
;;; off the end.
;;;
;;; %subvector-move-right src src-start src-limit dest dest-start
;;;   Copies elements from src to dest.  The order of access is from highest
;;;   to lowest, so this is used for moving vector elements to the RIGHT.
;;;
;;; %subvector-move-left src src-start src-limit dest dest-start
;;;   Copies elements from src to dest.  The order of access is from lowest
;;;   to highest, so this is used for moving vector elements to the LEFT.
;;;   Moving left is slightly faster than moving right.
;;;
;;;  If src and dest are the same vector, and you choose the wrong direction
;;;  to move, then these will not work correctly.

(defun %simple-substring-move-right (source src-start src-limit dest dest-start)
  #+lispworks
  (cond ((lw:simple-text-string-p dest)
         (cond ((lw:simple-text-string-p source)
                (%simple-substring-16b-move-right
                 source src-start src-limit dest dest-start))
               ((lw:simple-base-string-p source)
                (%simple-substring-8b->16b-move-right
                 source src-start src-limit dest dest-start))
               (t (error "Cannot move characters from ~s to ~s" source dest))))
        ((lw:simple-base-string-p dest)
         (cond ((lw:simple-base-string-p source)
                (%simple-substring-8b-move-right
                 source src-start src-limit dest dest-start))
               (t (error "Cannot move characters from ~s to ~s" source dest))))
        (t (error "Cannot move characters from ~s to ~s" source dest)))
  #-lispworks
  (progn
    ;; !! note this is not optimized, but should be compatible.
    (incf dest-start (- src-limit src-start))
    (loop
      (when (= src-limit src-start)
        (return-from nil nil))
      (decf src-limit)
      (decf dest-start)
      (setf (aref dest dest-start)
            (aref source src-limit)))))


;;; The FILL routines return the object filled for convenience in writing
;;; code like this:
;;; (simple-subvector-8b-fill (simple-subvector-8b-allocate 20) 0)
(defmacro define-fast-subvector-filler (function-name array-type vector-element-type &optional (fill-value-type vector-element-type))
  (labels ((symbol-append (&rest syms)
                          (intern (format nil "~a"
                                          (apply #'concatenate 'string
                                                 (map 'list #'symbol-name syms)))
                                  (symbol-package function-name)))

           (vector-type ()
                        `(,array-type ,vector-element-type (*)))

           (decls ()
                  `(DECLARE (TYPE ,fill-value-type FILL-VALUE)
                            (TYPE ,(vector-type) VECTOR)
                            (TYPE ARRAY-INDEX INDEX LIMIT)
                            ,(performance-optimizations)
                            #+allegro (:EXPLAIN :CALLS)))

           (inner-loop ()
                       `(LOOP
                         (PROGN
                          (WHEN (= INDEX LIMIT)
                                (RETURN-FROM NIL VECTOR))
                          (SETF (AREF VECTOR INDEX) (THE ,fill-value-type FILL-VALUE))
                          (INCF INDEX)))))
    `(PROGN
      (DECLAIM (FTYPE (FUNCTION (,(vector-type) ,fill-value-type ARRAY-INDEX ARRAY-INDEX) ,(vector-type))
                      ,function-name)
               (INLINE ,function-name)
               )
      (DEFUN ,function-name (VECTOR FILL-VALUE INDEX LIMIT)
        ,(decls)
        ,(inner-loop)))))

(define-fast-subvector-filler %simple-subvector-8b-fill  simple-array (unsigned-byte 8))

(proclaim (standard-optimizations))

(defmacro define-vector-filler (name vector-type element-type
                                subvector-filler vector-length)
  `(DEFUN ,name (VECTOR FILL-VALUE &key (START 0) END)
     (CHECK-TYPE VECTOR ,vector-type)
     (CHECK-TYPE FILL-VALUE ,element-type)
     (CHECK-TYPE START NON-NEGATIVE-INTEGER)
     (CHECK-TYPE END (OPTIONAL NON-NEGATIVE-INTEGER))
     (IF (ZEROP (,vector-length VECTOR))
         ;; Degenerate case of filling an empty vector does nothing.
         (PROGN
           (CHECK-TYPE START (INTEGER 0 (1)))
           (WHEN END (CHECK-TYPE END (INTEGER 0 (1))))
           VECTOR)
         (PROGN
           (CHECK-RANGE START 0 (,vector-length VECTOR))
           (WHEN END (CHECK-RANGE END START (,vector-length VECTOR)))
           (,subvector-filler VECTOR FILL-VALUE START (OR END (,vector-length VECTOR)))))))

(define-vector-filler simple-vector-8b-fill simple-vector-8b (unsigned-byte 8)
  %simple-subvector-8b-fill
  simple-vector-8b-length)
