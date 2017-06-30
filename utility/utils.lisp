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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +alignment-vector+
    (if (boundp '+alignment-vector+)
        (symbol-value '+alignment-vector+)
        (let ((alignment-vector (simple-vector-8b-allocate 512)))
          (simple-vector-8b-fill alignment-vector (char-code #\space))
          (setf (simple-vector-8b-ref alignment-vector 511)
                (char-code #\newline))
          alignment-vector))))

(defun align-stream (stream alignment)
  "Write padding bytes to STREAM until (mod (file-position stream) alignment)
   is zero."
  (let ((current-offset (file-position stream)))
    (cond ((null current-offset) (error "Could not determine offset in file."))
          ((= alignment 1) current-offset)
          (t (let ((remainder (mod current-offset alignment)))
               (cond ((zerop remainder) current-offset)
                     ;; A hack.  When padding to align the data
                     ;; structures, terminate the padding with a
                     ;; newline.  Makes it easier to read database in
                     ;; to a text editor!
                     ((< (- alignment remainder) 512)
                      (write-sequence +alignment-vector+
                                      stream
                                      :start (- 512 (- alignment remainder))
                                      :end 512)
                      (+ current-offset (- alignment remainder)))
                     (t
                      (dotimes (i (- alignment remainder 1)
                                  (progn (write-byte (char-code #\newline)
                                                     stream)
                                         (+ current-offset
                                            (- alignment remainder))))
                        (write-byte 32 stream)))))))))
(defclass byte-counting-stream
    (trivial-gray-streams:fundamental-binary-output-stream)
  ((position :initarg :initial-position
             :initform 0
             :accessor trivial-gray-streams:stream-file-position)))

(defmethod trivial-gray-streams:stream-write-byte ((s byte-counting-stream) byte)
  (incf (trivial-gray-streams:stream-file-position s))
  byte)

(defmethod trivial-gray-streams:stream-write-sequence
    ((s byte-counting-stream) sequence start end &key)
  (incf (trivial-gray-streams:stream-file-position s) (- end start))
  sequence)



;;
;; Fast number<->string
;;

;; creates a function for each base that counts the number of digits 
;; this generates fast conditional branching code
(defmacro define-fast-digit-counter (function-name base) 
  (let ((max-max (length (write-to-string most-positive-fixnum :base base))))
    (labels ((if-brancher (min max)
               (let* ((diff (- max min))
                      (mid (+ min (ceiling diff 2 ))))
                 (if (< diff 2)
                     max
                     `(if (< fixnum ,(expt base mid))
                          ,(if-brancher min mid )
                          ,(if-brancher mid max))))))
      `(defun ,function-name (fixnum)
         (declare #.(performance-optimizations)
           (type fixnum fixnum))
         ,(if-brancher 0 max-max)))))

;; $$$ this code was re-written because the original
;;     assumed that most-positive-fixnum is <=  8388607
;;     and SBCL the value is 4611686018427387903  19 digits
(define-fast-digit-counter fixnum-base10-digit-count 10)


;;; ??? floor gives a optimization fail in SBCL though foo below does not ???
(defun %write-fixnum-to-string (number string position)
  "Write the digits of FIXNUM to STRING in base 10, most significant digit
   at POSITION."
  (declare #.(performance-optimizations)
           (type fixnum number)
           (type array-index position)
           (type #+lispworks lw:simple-text-string
                 #-lispworks simple-string
                 string)
           )

  (do ((pos (1- (+ position (the (integer 0 #.(ceiling (log most-positive-fixnum 10))) 
                                 (fixnum-base10-digit-count number))))
            (1- pos)))
      ((< pos position))
    (declare (type fixnum pos))
    (multiple-value-bind (quotient remainder)
        (floor number 10)
      (declare (type fixnum quotient)
               (type (integer 0 (10)) remainder))
      (setq number quotient)
      (setf (char string pos)
            (char "0123456789" remainder)))))

#|(defun write-fixnum-to-string-test (number string position)
  "Write the digits of FIXNUM to STRING in base 10, most significant digit
   at POSITION."
  (declare (optimize
            (compilation-speed 0)
            (debug             0)
            (safety            0)
            (space             0)
            (speed             2))
           (type fixnum number)
           (type array-index position)
           (type simple-string string))

  (do ((pos (1- (+ position (ceiling (log number 10))))
            (1- pos)))
      ((< pos position))
    (declare (type fixnum pos))
    (multiple-value-bind (quotient remainder)
        (floor number 10)
      (declare (type fixnum quotient)
               (type (integer 0 (10)) remainder))
      (setq number quotient)
      (setf (char string pos)
            (char "0123456789" remainder)))))|#

#|(defun foo (n) 
	  (declare #.(performance-optimizations)
		   (type fixnum n))
	  (multiple-value-bind (q r) 
	      (floor n 10)
	    (declare (type fixnum q)
               (type (integer 0 (10)) r))
	    (print q)
	    (print r)
	    (values q r)))|#

(defun read-string-data (length stream)
  (declare (type array-index length)
           #.(performance-optimizations))
  (let ((temp (simple-vector-8b-allocate length))
        (result (simple-string-8b-allocate length)))
    (declare (type simple-vector-8b temp)
             (type simple-string-8b result))
    (read-sequence temp stream)
    (dotimes (i length result)
      (setf (schar result i) (code-char (aref temp i))))))

(defun write-string-data (string stream)
  (write-sequence (map 'simple-vector-8b #'char-code string) stream))


;;; Make these perform better.
(defun write-simple-bit-vector (vector stream)
  (let ((bytes (ceiling (simple-bit-vector-length vector) 8)))
    (do ((byte-count 0 (1+ byte-count))
         (index 0 (+ index 8)))
        ((>= byte-count bytes))
      (do ((byte 0 (+ (* byte 2)
                      (if (>= idx (simple-bit-vector-length vector))
                          0
                          (sbit vector idx))))
           (count 0 (+ count 1))
           (idx (+ index 7) (- idx 1)))
          ((>= count 8) (write-byte byte stream))))))

(defun read-simple-bit-vector (length stream)
  (let* ((result (simple-vector-1b-allocate length))
         (bytes (ceiling length 8)))
    (do ((byte-count 0 (1+ byte-count))
         (index 0 (+ index 8)))
        ((>= byte-count bytes) result)
      (do ((byte (read-byte stream) (floor byte 2))
           (count 0 (+ count 1))
           (idx  index (+ idx 1)))
          ((>= count 8))
        (when (< idx length)
          (setf (sbit result idx) (if (evenp byte) 0 1)))))))

(declaim (ftype (function (fixnum #+lispworks output-stream
                                  #-lispworks stream)
                          (values))
                %write-fixnum)
         (ftype (function ((unsigned-byte 16) #+lispworks output-stream
                                              #-lispworks stream)
                          (values))
                %write-unsigned16)
         (ftype (function ((unsigned-byte 32) #+lispworks output-stream
                                              #-lispworks stream)
                          (values))
                %write-unsigned32)
         (inline %write-fixnum
                 %write-unsigned16
                 %write-unsigned32))


;;;   $$$ originally written for 24 bit fixnums of lispworks
(defun %write-fixnum (value stream)
  (declare #.(performance-optimizations)
           (type fixnum value))
  (macrolet ((write-bytes ()
               (let ((bytes (ceiling (integer-length most-negative-fixnum)
                                     8)))
                 (cons 'progn
                       (loop for i upto (1- bytes) collect
                             `(write-byte (ldb (byte 8 ,(* 8 i)) value)
                                          stream))))))
    (write-bytes)))

(defun %write-unsigned16 (value stream)
  (declare #.(performance-optimizations)
           (type (unsigned-byte 16) value)
           )
  (write-byte (ldb (byte 8 0)  value) stream)
  (write-byte (ldb (byte 8 8)  value) stream))

(defun %write-unsigned32 (value stream)
  (declare #.(performance-optimizations)
           (type (unsigned-byte 32) value)
           )
  (%write-unsigned16 (the (unsigned-byte 16) (logand value #xFFFF)) stream)
  (%write-unsigned16 (the (unsigned-byte 16) (ash value -16)) stream))

(defun write-fixnum (value stream)
  (check-type value fixnum)
  (%write-fixnum value stream))

(defun write-unsigned16 (value stream)
  (check-type value (unsigned-byte 16))
  (%write-unsigned16 value stream))

(defun write-unsigned32 (value stream)
  (check-type value (unsigned-byte 32))
  (%write-unsigned32 value stream))

(defun %expmod (base exponent mod)
  (declare (type (integer 0 *) exponent)
           (type (integer 1 *) base mod))
  (cond ((zerop exponent) 1)
        ((evenp exponent) (mod (let ((x (%expmod base (/ exponent 2) mod)))
                                 (* x x))
                               mod))
        (t (mod (* base (%expmod base (1- exponent) mod))
                mod))))

(defun expmod (base exponent mod)
  "Raise BASE to EXPONENT power modulo MOD."
  (check-type base (integer 1 *))
  (check-type mod (integer 1 *))
  (check-type exponent (integer 0 *))
  (%expmod base exponent mod))

(defun fermat-test (n)
  "Use Fermat's little theorem to test N for primality.
   Return value of NIL means N is not prime.
   Return value of T means N is likely to be prime with 50% confidence."
  (let ((trial-number (1+ (random (1- n)))))
    (= (expmod trial-number n n) trial-number)))

(defun prime? (n)
  (dotimes (i 128 t)
    (unless (fermat-test n)
      (return nil))))

(defun n-digit-prime (n)
  (let ((trial (random (1- (expt 2 n)))))
    (if (prime? trial)
        trial
        (n-digit-prime n))))


(defun keyword-package ()
  #+lispworks (sys:keyword-package)
  #-lispworks (find-package "KEYWORD"))

(defun find-keyword (string)
  (find-symbol (string string) (keyword-package)))


(defun %mirror-fixnum (fixnum)
  "Return a fixnum whose bit pattern is the mirror image of the input."
  (declare (type fixnum fixnum)
           #.(performance-optimizations))
  (do ((input  fixnum (floor input 2))
       (output      0 (if (evenp input)
                          (fix:* output 2)
                          (fix:1+ (fix:* output 2))))
       (count       0 (fix:1+ count)))
      ((= count #.(integer-length most-positive-fixnum)) output)
    (declare (type fixnum input output)
             (type (integer 0 #.(integer-length most-positive-fixnum)) count))
    ))

(defun read-fixnum (stream)
  (declare #.(performance-optimizations))
  (let* ((b0 (read-byte stream))
         (b1 (read-byte stream))
         (b2 (read-byte stream)))
    ;(declare (type (unsigned-byte 8) b0 b1 b2))
    ;; by checking for the top byte being zero,
    ;; we can avoid a trip through the bignum
    ;; routines.
    (if (zerop b2)
        (the (unsigned-byte 16)
          (+ (the (unsigned-byte 16) (ash b1 8))
             b0))
        (+ (the (unsigned-byte 24)
             (ash b2 16))
           (the (unsigned-byte 16)
             (+ (the (unsigned-byte 16) (ash b1 8))
                b0))))))

;;; !!! changed input-stream to stream
(declaim (ftype (function (stream) (unsigned-byte 16)) read-unsigned16)
         (inline read-unsigned16))

(defun read-unsigned16 (stream)
  (declare #.(performance-optimizations))
  (let* ((b0 (read-byte stream))
         (b1 (read-byte stream)))
    (declare (type (unsigned-byte 8) b0 b1))
    (the (unsigned-byte 16)
      (+ (the (unsigned-byte 16) (ash b1 8))
         b0))))

(defun read-unsigned32 (stream)
  (declare #.(performance-optimizations))
  (let* ((word0 (read-unsigned16 stream))
         (word1 (read-unsigned16 stream)))
    (declare (type (unsigned-byte 16) word0 word1))
    (if (< word1 #.(ash most-positive-fixnum -16))
        (the fixnum
          (+ (the fixnum (ash word1 16))
             word0))
        (the (unsigned-byte 32)
          (+ (the (unsigned-byte 32) (ash word1 16))
             word0)))))


;;; Logical name encoding
;;;; Name mapping.
;;;;
;;;; It should be possible to represent file names in a reasonable
;;;; portable manner.  Logical pathnames provide us with much of the
;;;; necessary abstraction, but there is a problem:  Only numbers,
;;;; characters, and hyphens may appear in a logical pathname.  Most
;;;; systems make use of additional characters.
;;;;
;;;; In order to deal with this in an intelligent manner, we encode
;;;; the non-conforming characters.
;;;;
;;;; Details of the encoding mechanism are in
;;;; ACM Ada Letters XIII, 5 (Sep/Oct 1993), 43-47
;;;; Strategies for the Lossless Encoding of Strings as Ada Identifiers
;;;; (See ftp://ftp.netcom.com/pub/hb/hbaker/Encode.html)
;;;;
;;;; Not the obvious choice, I admit, but read the paper and you will become
;;;; enlightened.

(defparameter *character-map*
    '((#\! . "BNG")                     ; bang.  "exclamation point" is too long
      (#\" . "QUT2")                    ; "double quote" is too long
      (#\# . "SHRP")                    ; sharp.  "pound sign" is too parochial
      (#\$ . "DLLR")                    ; dollar
      (#\% . "PCT")                     ; percent
      (#\& . "AMPR")                    ; ampersand. "and" assumes C usage.
      (#\' . "QUT1")                    ; "single quote" is too long
      (#\( . "LPAR")                    ; "left parenthesis" is too long
      (#\) . "RPAR")                    ; "right parenthesis" is too long
      (#\* . "STR")                     ; star.  "asterisk" is too long
      (#\, . "CMMA")                    ; comma
      (#\: . "CLN")                     ; colon
      (#\; . "SMICLN")    ; semicolon
      (#\< . "LSS")                     ; less.  "less than" is too long
      (#\= . "EQUL")                    ; equal. "equal to" is too long
      (#\> . "GRT")                     ; greater. "greater than" is too long
      (#\? . "QST")                     ; question. "question mark" is too long
      (#\@ . "ATSGN")                   ; at sign.  "at" is too ambiguous
      (#\[ . "LBRK")                    ; "left square bracket" is too long
      (#\\ . "BSLSH")                   ; "back slash" is too long
      (#\] . "RBRK")                    ; "right square bracket" is too long
      (#\^ . "UPARW")                   ; up arrow. "circumflex" is too pedantic
      (#\_ . "UNDR")                    ; under. "underline" and "underscore" are too long
      (#\` . "BQUOT")                   ; "back quote" is too long
      (#\{ . "LBRC")                    ; "left curly brace" is too long
      (#\| . "VBAR")                    ; "vertical bar" is too long
      (#\} . "RBRC")                    ; "right curly brace" is too long
      (#\~ . "TLD")                     ; tilde
      (#\+ . "PLS")                     ; plus
      (#\- . "MNS")                     ; minus
      (#\. . "PRD")                     ; "period" is too long
      (#\/ . "SLSH")                    ; slash.  "divided" ignores "not" (/=) connotation
      (#\space  . "SPCE")               ; space
      ;; Add more definitions here
      )
  "List of characters that cannot appear in filename and the upper-case string mapping
that is substituted in place.")


(defsubst self-mapping-char-p (char)
  "Return T iff the char is a lower-case alphabetic or a numeric character."
  (or (and (alpha-char-p char)
           (lower-case-p char))
      (digit-char-p char)))

(define-constant +quote-syllable+ "QQ" :test #'string=)
(define-constant +quote-upcase-syllable+ "QQU" :test #'string=)
(define-constant +quote-capitalize-syllable+ "QQC" :test #'string=)

(defun encode-namestring-loop (namestring start end)
  (declare (type simple-string namestring)
           (type array-index start end)
           )
  (unless (>= start end)
    (let ((scan (position-if (complement #'self-mapping-char-p)
                             namestring
                             :start start
                             :end end)))
      (if (null scan)
          (let* ((syl (string-upcase (subseq namestring start end)))
                 (probe (rassoc syl *character-map* :test #'equal)))
            (if (or probe
                    (string= syl +quote-capitalize-syllable+)
                    (string= syl +quote-upcase-syllable+)
                    (string= syl +quote-syllable+))
                (list +quote-syllable+ syl)
                (list syl)))
          (locally (declare (type array-index scan))
            (if (= scan start)
                (let* ((bogon (schar namestring scan))
                       (probe (assoc bogon *character-map*)))
                  (cond ((and (eql bogon #\-)
                              (> scan 0)
                              (< scan (1- end))
                              (self-mapping-char-p (schar namestring
                                                          (1- scan)))
                              (self-mapping-char-p (schar namestring
                                                          (1+ scan))))
                         (encode-namestring-loop namestring (1+ scan) end))
                        (probe (cons (cdr probe) (encode-namestring-loop
                                                  namestring
                                                  (1+ scan) end)))
                        ((upper-case-p bogon)
                         (let* ((scan1 (or (position-if (complement (lambda (char)
                                                                      (and (alpha-char-p char)
                                                                           (upper-case-p char))))
                                                        namestring
                                                        :start scan
                                                        :end end)
                                           end))
                                (scan2 (if (and (= scan1 (1+ scan)) ;; just one upcase character
                                                (< scan1 end))       ;; more chars
                                           (or
                                            (position-if (complement (lambda (char)
                                                                       (and (alpha-char-p char)
                                                                            (not (upper-case-p char)))))
                                                         namestring
                                                         :start scan1
                                                         :end end)
                                            end)
                                           scan)))
                           (declare (type array-index scan1 scan2))
                           (if (> scan2 scan1)
                               (list* +quote-capitalize-syllable+
                                      (string-upcase (subseq namestring start scan2))
                                      (encode-namestring-loop namestring scan2 end))
                               (list* +quote-upcase-syllable+
                                      (subseq namestring start scan1)
                                      (encode-namestring-loop namestring scan1 end)))))
                        (t (cons (string bogon)
                                 (encode-namestring-loop
                                  namestring
                                  (1+ scan) end)))))
                (let* ((syl (string-upcase (subseq namestring start scan)))
                       (probe (rassoc syl *character-map* :test #'equal)))
                  (if (or probe
                          (string= syl +quote-capitalize-syllable+)
                          (string= syl +quote-upcase-syllable+)
                          (string= syl +quote-syllable+))
                      (list* +quote-syllable+
                             syl
                             (encode-namestring-loop namestring scan end))
                      (cons syl
                            (encode-namestring-loop namestring scan end))))))))))
;;; The main entry points.
(defun encode-namestring (namestring)
  "Given a NAMESTRING that represents a pathname component, return a new string that
can be used as a logical pathname component.

This can be done in a machine independent way."
  (check-type namestring simple-string)
  (reduce (lambda (accum more)
            (concatenate 'string accum "-" more))
          (encode-namestring-loop namestring 0 (simple-string-length namestring))))

;;; !!! labels needs to be TCO.
;;;     originally used macro tail-labels
;;;     see utility-macros.lsp of the original code.
(defun decode-namestring (encoded)
  "Given an ENCODED namestring that represents a logical pathname component, return a
new string that can be used as a physical pathname component.

Note, this mapping should be done in a machine dependent way, but for
the common case, UNIX and Windows, we can get away with a generic mapping."
  (declare (type simple-string encoded))
  (labels ((generate-syllables (start end)
                  (declare (type array-index start end))
                  (let ((scan (position #\- encoded :start start :end end)))
                    (if (null scan)
                        (list (subseq encoded start end))
                      (cons (subseq encoded start scan)
                            (generate-syllables (1+ scan) end)))))

                (decode-syllables (sylls)
                  (tcond ((null sylls) nil)
                         ((string= (car sylls) +quote-syllable+)
                          (cons (string-downcase (cadr sylls))
                                (decode-syllables (cddr sylls))))
                         ((string= (car sylls) +quote-capitalize-syllable+)
                          (cons (string-capitalize (cadr sylls))
                                (decode-syllables (cddr sylls))))
                         ((string= (car sylls) +quote-upcase-syllable+)
                          (cons (cadr sylls)
                                (decode-syllables (cddr sylls))))
                         ((rassoc (car sylls) *character-map* :test #'equal)
                          => (lambda (probe)
                               (cons (string (car probe))
                                     (decode-syllables (cdr sylls)))))
                         (t (cons (string-downcase (car sylls))
                                  (decode-syllables (cdr sylls)))))))
    (reduce (lambda (accum more)
                (declare (type simple-string accum more))
                (if (and (self-mapping-char-p (schar accum (1- (length accum))))
                         (self-mapping-char-p (schar more 0)))
                    (concatenate 'string accum "-" more)
                  (concatenate 'string accum more)))
            (decode-syllables
             (generate-syllables 0 (length encoded))))))


;;; Object equality

(defvar *objects-equalp-circularity-stack* nil
  "A stack that holds a list of calls to objects-equalp to avoid
 circularity problems.")

(defgeneric objects-equalp (left right)
  (:documentation
   "Routine to use in place of EQUALP to determine if two objects are semantically equal.
    CLOS and structure types are free to overload this method as appropriate.

    DEFINE-TENN-CLASS and DEFINE-TENN-STRUCTURE should initially provide an equality method
    which is functionally equivalent to EQUALP, but this hasn't been implemented yet.  So if you don't
    overload the method, the default methods will perform EQUALP on their objects.")

  ;; Don't define this.
  ;; (:method ((left t) (right t))
  ;;   (equalp object-1 object-2))

  ;; JRM's reason for not defining the above is that it is easier to
  ;; debug the case of someone not defining a method between two
  ;; different types which are meant to be OBJECTS-EQUALP if that case
  ;; causes a NO-APPLICABLE-METHOD error.  I (naha) will humor him
  ;; even though I feel having a default method on (T T) is the right
  ;; thing to do.

  ;; I truly appreciate this.  I agree that a default method is probably
  ;; correct in the long term, but I got bitten by this and find that the
  ;; error condition helps me debug. ~jrm

  ;; Instead of having the default method, for each new
  ;; type for which we are defining behavior for OBJECTS-EQUALP, we
  ;; also need to define a pair of methods for that type and T (one
  ;; method for each specializer order.  See
  ;; DEFINE-OBJECTS-EQUALP-TYPE-MISMATCH-METHODS below.

  (:method ((left number) (right number))
    "Numbers are equalp if they are =."
    (= left right))

  ;;  (:method ((left symbol) (right symbol))
  ;;    (eq left right))

  (:method ((left cons) (right cons))
    "Recursively descend conses."
    (and (objects-equalp (car left) (car right))
         (objects-equalp (cdr left) (cdr right))))

  (:method ((left vector) (right vector))
    "Recursively descend simple vectors."
    (or (and (simple-vector-p left)
             (simple-vector-p right)
             (= (simple-vector-length left) (simple-vector-length right))
             (not (mismatch left right :test #'objects-equalp)))
        (error "Not simple vectors.")))

  (:method ((left string) (right string))
    (not (mismatch left right :test #'char=)))

  (:method ((left pathname) (right pathname))
    "This is kind of a kludge.  Should know what platform the pathnames are on."
    (equalp left right))
  )

(defmacro define-objects-equalp-type-mismatch-methods (type &optional doc
                                                       &environment env)
  "Defines methods on OBJECTS-EQUALP which return NIL if an object of type TYPE
   is compared with some arbitrary object."
  (check-type type symbol)
  (assert (find-class type nil env))
  `(PROGN
     (DEFMETHOD OBJECTS-EQUALP ( (OBJECT1 ,type) (OBJECT2 t)     ) ,doc NIL)
     (DEFMETHOD OBJECTS-EQUALP ( (OBJECT2 t)     (OBJECT1 ,type) ) ,doc NIL) ))

(define-objects-equalp-type-mismatch-methods cons
    "Conses are never equal to anything but other conses.")
(define-objects-equalp-type-mismatch-methods number
    "Numbers are never equal to anything but numbers.")
(define-objects-equalp-type-mismatch-methods string
  "Strings are never equal to anything but other strings.")
(define-objects-equalp-type-mismatch-methods symbol
    "Symbols are never equal to anything but symbols, and the
     eq test in the :around method will catch that case.")
(define-objects-equalp-type-mismatch-methods vector
    "Vectors are never equal to anything but vectors.")

(defmethod objects-equalp :around (left right)
  "First check to see if the objects are eql.  If so, no need to go further.
   Otherwise, look to see if we're recursively testing for equality on these
   some objects.  If so, the objects are equal.
   Finally, to avoid recursion death, we push the objects on the circularity
   stack so we won't test them again."
  (let (last-time)
    (cond ((eql left right) t)
          ((and (setq last-time (assoc left *objects-equalp-circularity-stack*
                                       :test #'eql))
                (eql right (cdr last-time)))
           ;; Recursive loop.  We will typically get here because some
           ;; method for an aggregate object recurses on components of
           ;; that aggregate.  Return T here.  If the objects being
           ;; compared arn't really OBJECTS-EQUALP then some other
           ;; condition in that other method will catch it.
           t)
          (t (let* ((new-entry (cons left right))
                    (new-stack (cons new-entry *objects-equalp-circularity-stack*)))
               ; (declare (dynamic-extent new-entry new-stack))
               (let ((*objects-equalp-circularity-stack* new-stack))
                 (call-next-method)))))))


(defmacro pushlast (item location)
  "Push an element onto the last location in a list.
   Traverses the list twice and makes one copy, so
   it is a tad expensive, but it is what you want
   to use for maintaining versioned lists of items."
  `(SETF ,location
         (NREVERSE (CONS ,item (REVERSE ,location)))))
