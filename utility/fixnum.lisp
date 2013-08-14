;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 - 2005 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: New BSD
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:fixnum-math)

#|(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(= /= < > <= >=
            MAX MIN
            MINUSP PLUSP
            FLOOR CEILING
            * + - / 1+ 1-
            ABS EVENP ODDP
            GCD INCF DECF
            LCM ASH
            LOGAND LOGANDC1 LOGANDC2
            LOGEQV LOGIOR LOGNAND
            LOGNOR LOGNOT LOGORC1 LOGORC2
            LOGXOR LOGBITP LOGCOUNT LOGTEST
            DEPOSIT-FIELD DPB LDB LDB-TEST
            MASK-FIELD)))|#

(proclaim (standard-optimizations))

;;; The FIX* provide for optimized fixnum manipulation,
;;; where the inputs AND the outputs are expected to be fixnums.
;;; They should normally be used in code compiled for performance.

(defun the-fixnum (thing env)
  (csf-utility:restrict-type thing 'fixnum env))

(defun expand-fixnum-form (env operator operand1 operand2 operand-list)
  "Perform an n-ary expansion of a binary fixnum operator.
   (fix:+ a b c) =>
       (the fixnum
         (+ (the fixnum
              (+ (the fixnum a)
                 (the fixnum b)))
            (the fixnum c)))"
  (do ((forms operand-list (cdr forms))
       (result (the-fixnum
                `(,operator ,(the-fixnum operand1 env)
                            ,(the-fixnum operand2 env))
                env)
               (the-fixnum
                `(,operator ,result
                            ,(the-fixnum (car forms) env))
                env)))
      ((null forms) result)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package :common-lisp))

(defmacro fix::+ (&environment env i1 i2 &rest fixnum-forms)
  "Add two fixnums that are expected to return a fixnum result.
   The goal is to have the compiler efficiently inline this expression when suitable
   optimization settings are in effect."
  (expand-fixnum-form env 'cl:+ i1 i2 fixnum-forms))

(defmacro fix::* (&environment env i1 i2 &rest fixnum-forms)
  "Multiply two fixnums that are expected to return a fixnum result.
   The goal is to have the compiler efficiently inline this expression when suitable
   optimization settings are in effect."
  (expand-fixnum-form env 'cl:* i1 i2 fixnum-forms))

(defmacro fix::- (&environment env i1 i2 &rest fixnum-forms)
  "Subtract two fixnums that are expected to return a fixnum result.
   The goal is to have the compiler efficiently inline this expression when suitable
   optimization settings are in effect."
  (expand-fixnum-form env 'cl:- i1 i2 fixnum-forms))

(defmacro fix::< (&environment env l r)
  `(CL:< ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro fix::> (&environment env l r)
  `(CL:> ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro fix::<= (&environment env l r)
  `(CL:<= ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro fix::>= (&environment env l r)
  `(CL:>= ,(the-fixnum l env) ,(the-fixnum r env)))

(defmacro fix::= (l r)
  `(CL:EQ ,l ,r))

(defmacro fix::1+ (&environment env arg)
  `(CL:1+ ,(the-fixnum arg env)))

(defmacro fix::1- (&environment env arg)
  `(CL:1- ,(the-fixnum arg env)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :common-lisp))
