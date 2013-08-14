;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 (New BSD)
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:csf-utility)

(proclaim (standard-optimizations))

;;; Length is called far too often to not optimize it!

(defmacro bit-vector-length (bit-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((BIT-VECTOR ,bit-vector))
        (DECLARE (TYPE BIT-VECTOR BIT-VECTOR)
                ,(performance-optimizations))
        (LENGTH BIT-VECTOR))))

(defmacro simple-bit-vector-length (simple-bit-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((SIMPLE-BIT-VECTOR ,simple-bit-vector))
        (DECLARE (TYPE SIMPLE-BIT-VECTOR SIMPLE-BIT-VECTOR)
                ,(performance-optimizations))
        (LENGTH SIMPLE-BIT-VECTOR))))

(defmacro simple-vector-length (simple-vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((SIMPLE-VECTOR ,simple-vector))
        (DECLARE (TYPE SIMPLE-VECTOR SIMPLE-VECTOR)
                ,(performance-optimizations))
        (LENGTH SIMPLE-VECTOR))))

(defmacro string-8b-length (string-8b)
  `(THE (INTEGER 0 ,array-dimension-limit)
     (LET ((STRING-8B ,string-8b))
       (DECLARE (TYPE STRING-8B STRING-8B)
                ,(performance-optimizations))
       (LENGTH STRING-8B))))

(defmacro string-16b-length (string-16b)
  `(THE (INTEGER 0 ,array-dimension-limit)
     (LET ((STRING-16B ,string-16b))
       (DECLARE (TYPE STRING-16B STRING-16B)
                ,(performance-optimizations))
       (LENGTH STRING-16B))))

(defmacro string-length (string)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((STRING ,string))
        (DECLARE (TYPE STRING STRING)
                ,(performance-optimizations))
        (LENGTH STRING))))

(defmacro vector-length (vector)
  `(THE (INTEGER 0 ,array-dimension-limit)
      (LET ((VECTOR ,vector))
        (DECLARE (TYPE VECTOR VECTOR)
                ,(performance-optimizations))
        (LENGTH VECTOR))))

;;; Length is one of those functions that is called way
;;; too often.  A common pattern is something like this:
;;; (= (length x) 2)
;;;
;;; If x is a list, this traverses the entire list.
;;;
;;; These functions/macros compute lengths much more
;;; efficiently.

(defun %list-length< (list amount)
  (declare (type array-index amount)
           #.(performance-optimizations))
  (cond ((consp list) (and (plusp amount)
                           (%list-length< (cdr list) (1- amount))))
        ((null list) (plusp amount))
        (t (error 'changesafe-type-error
                  :datum list
                  :expected-type 'list))))

(defun %list-length<= (list amount)
  (declare (type array-index amount)
           #.(performance-optimizations))
  (cond ((consp list) (and (plusp amount)
                           (%list-length<= (cdr list) (1- amount))))
        ((null list) (not (minusp amount)))
        (t (error 'changesafe-type-error
                  :datum list
                  :expected-type 'list))))

(defun %list-length= (list amount)
  (declare (type array-index amount)
           #.(performance-optimizations))
  (cond ((consp list) (and (plusp amount)
                           (%list-length= (cdr list) (1- amount))))
        ((null list) (zerop amount))
        (t (error 'changesafe-type-error
                  :datum list
                  :expected-type 'list))))

(defun %list-length>= (list amount)
  (declare (type array-index amount)
           #.(performance-optimizations))
  (cond ((consp list) (or (not (plusp amount))
                          (%list-length>= (cdr list) (1- amount))))
        ((null list) (zerop amount))
        (t (error 'changesafe-type-error
                  :datum list
                  :expected-type 'list))))

(defun %list-length> (list amount)
  (declare (type array-index amount)
           #.(performance-optimizations))
  (cond ((consp list) (or (not (plusp amount))
                          (%list-length> (cdr list) (1- amount))))
        ((null list) nil)
        (t (error 'changesafe-type-error
                  :datum list
                  :expected-type 'list))))

(defun length< (sequence amount)
  (check-type amount array-index)
  (etypecase sequence
    (null (plusp amount))
    (cons (and (plusp amount)
               (%list-length< (cdr sequence) (1- amount))))
    (vector (< (vector-length sequence) amount))))

(defun length<= (sequence amount)
  (check-type amount array-index)
  (etypecase sequence
    (null (not (minusp amount)))
    (cons (and (plusp amount)
               (%list-length<= (cdr sequence) (1- amount))))
    (vector (<= (vector-length sequence) amount))))

(defun length= (sequence amount)
  (check-type amount array-index)
  (etypecase sequence
    (null (zerop amount))
    (cons (and (plusp amount)
               (%list-length= (cdr sequence) (1- amount))))
    (vector (= (vector-length sequence) amount))))

(defun length>= (sequence amount)
  (check-type amount array-index)
  (etypecase sequence
    (null (zerop amount))
    (cons (or (not (plusp amount))
              (%list-length>= (cdr sequence) (1- amount))))
    (vector (>= (vector-length sequence) amount))))

(defun length> (sequence amount)
  (check-type amount array-index)
  (etypecase sequence
    (null nil)
    (cons (or (not (plusp amount))
              (%list-length> (cdr sequence) (1- amount))))
    (vector (>= (vector-length sequence) amount))))

(defun %list-shorter? (left right)
  (declare #.(performance-optimizations))
  (cond ((consp right) (cond ((consp left) (%list-shorter? (cdr left) (cdr right)))
                             ((null left) t)
                             (t (error 'changesafe-type-error
                                       :datum left
                                       :expected-type 'list))))
        ((null right) nil)
        (t (error 'changesafe-type-error
                  :datum right
                  :expected-type 'list))))

(defun %list-same-length? (left right)
  (declare #.(performance-optimizations))
  (cond ((consp left) (cond ((consp right) (%list-same-length? (cdr left) (cdr right)))
                            ((null right) nil)
                            (t (error 'changesafe-type-error
                                      :datum right
                                      :expected-type 'list))))
        ((null left) (null right))
        (t (error 'changesafe-type-error
                  :datum left
                  :expected-type 'list))))

(defun %list-longer? (left right)
  (declare #.(performance-optimizations))
  (cond ((consp left) (cond ((consp right) (%list-longer? (cdr left) (cdr right)))
                            ((null right) t)
                            (t (error 'changesafe-type-error
                                      :datum right
                                      :expected-type 'list))))
        ((null left) nil)
        (t (error 'changesafe-type-error
                  :datum left
                  :expected-type 'list))))

(defun longer? (first second &rest others)
  "True if the length of sequences is in descending order."
  (and (etypecase first
         (null nil)
         (cons (etypecase second
                 (null t)
                 (cons (%list-longer? (cdr first) (cdr second)))
                 (vector (%list-length> first (vector-length second)))))
         (vector (etypecase second
                   (null (not (zerop (vector-length first))))
                   (cons (%list-length< second (vector-length first)))
                   (vector (> (vector-length first) (vector-length second))))))
       (or (null others)
           (apply #'longer? second others))))

(defun same-length? (first second &rest others)
  "True if all sequences have the same length"
  (and (etypecase first
         (null (etypecase second
                 (null t)
                 (cons nil)
                 (vector (zerop (vector-length second)))))
         (cons (etypecase second
                 (null nil)
                 (cons (%list-same-length? (cdr first) (cdr second)))
                 (vector (%list-length= first (vector-length second)))))
         (vector (etypecase second
                   (null (zerop (vector-length first)))
                   (cons (%list-length= second (vector-length first)))
                   (vector (= (vector-length first) (vector-length second))))))
       (or (null others)
           (apply #'same-length? second others))))

(defun shorter? (first second &rest others)
  "True if the length of sequences is in ascending order."
  (and (etypecase first
         (null (not (null second)))
         (cons (etypecase second
                 (null nil)
                 (cons (%list-shorter? (cdr first) (cdr second)))
                 (vector (%list-length< first (vector-length second)))))
         (vector (etypecase second
                   (null t)
                   (cons (%list-length> second (vector-length first)))
                   (vector (< (vector-length first) (vector-length second))))))
       (or (null others)
           (apply #'shorter? second others))))
