;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Copyright © 2002 ChangeSafe, LLC
;;;;          ALL RIGHTS RESERVED.
;;;;
;;;; License: BSD 3 (New BSD)
;;;; see License.txt
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; File Name:     utility-macros.lsp
;;;; Author:        Joe Marshall
;;;;
;;;; Module Description:  Utility macros
;;;;
;;;; Provides basic macros used by the utility package
;;;; (among others).  These macros are loaded before any
;;;; other macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:csf-utility)

(proclaim (standard-optimizations))




;;; Allegro doesn't obey INLINE declaimations.
;;; DEFSUBST is a macro that gets around this restriction.
;;; DEFSUBST works like DEFUN, but defines a COMPILER-MACRO that inlines the
;;; form.  Use this judiciously on small functions only.
;;; While it works with unusual lambda lists, you might get
;;; unused variable warnings if you defsubst something with keyword or
;;; optional args.

#+allegro
(defparameter *warn-if-inlining* nil "If T, be verbose about inlining when compiling.")

#+allegro
(defparameter *warn-if-not-inlining* nil "If T, be verbose about not inlining when compiling.")

#+allegro
(defmacro defsubst (name lambda-list &body body)
  "Identical to DEFUN except that it arranges for the body to be INLINED
   in the places it is called."
  (let ((form-var (gensym (symbol-name :FORM-))))
    `(PROGN (DEFUN ,name ,lambda-list ,@body)
            (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
              (DEFINE-COMPILER-MACRO ,name (&WHOLE ,form-var ,@lambda-list)
                (DECLARE (IGNORE ,@lambda-list))
                ;; Inline only if speed is >= debug
                ;; and space < 3
                (IF (AND (>= excl::.speed. excl::.debug.)
                         (< excl::.space. 3))
                    (PROGN
                      (WHEN *WARN-IF-INLINING*
                        (warn "Inlining call ~s" ,form-var))
                      `(,',`(LAMBDA ,lambda-list
                              (BLOCK ,name (LOCALLY ,@(if (stringp (car body))
                                                              (cdr body)
                                                            body))))
                           ,@(CDR ,form-var)))
                  (PROGN
                    (WHEN *WARN-IF-NOT-INLINING*
                      (warn "Not inlining call ~s" ,form-var))
                    ,form-var))))
            )))

#-allegro
(defmacro defsubst (name lambda-list &body body)
  "Identical to DEFUN except that it arranges for the body to be INLINED
   in the places it is called."
  `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (DECLAIM (INLINE ,name))
     (DEFUN ,name ,lambda-list ,@body)))

(defun split-declarations (body)
  "Given a LAMBDA body, return three values, the declarations, the doc string,
   if present, and the body stripped of the declarations and doc string."
  (labels ((scan (forms docstring declarations)
             (cl:cond ((and (consp (car forms))
                         (eq (caar forms) 'declare))
                    (scan (cdr forms) docstring (cons (car forms) declarations)))

                   ((or (null forms)
                        (not (stringp (car forms)))
                        docstring
                        (null (cdr forms)))
                    (values docstring (nreverse declarations) forms))

                   (t (scan (cdr forms) (car forms) declarations)))))

    (scan body nil nil)))


;; Tear apart a closure so that we can beta-reduce it.
(defun destructure-function-lambda (arity fl receiver if-not-function)
  "If fl is of the form (FUNCTION (LAMBDA (bound-variable-list) docstring decls body))
   invoke receiver on the bound-variable-list, docstring, decls, and the body.

   If fl is of the form (FUNCTION name), invoke receiver on a
   fake eta-expanded form.

   If fl is of the form NAME, invoke receiver on a
   fake eta-expanded form.

   Otherwise invoke if-not-function."
  (macrolet ((list-length-equals-one (list)
               `(AND (CONSP ,list)
                     (NULL (CDR ,list))))

             (list-length-greater-than-one (list)
               `(AND (CONSP ,list)
                     (CONSP (CDR ,list))))

             (is-function-form (form)
               `(AND (CONSP ,form)
                     (EQ (CAR ,form) 'FUNCTION)
                     (LIST-LENGTH-EQUALS-ONE (CDR ,form))))

             (function-form-body (function-form)
               `(CADR ,function-form))

             (is-lambda-form (form)
               `(AND (CONSP ,form)
                     (EQ (CAR ,form) 'LAMBDA)
                     (LIST-LENGTH-GREATER-THAN-ONE (CDR ,form))))

             (lambda-form-arguments (lambda-form)
               `(CADR ,lambda-form))

             (lambda-form-body (lambda-form)
               `(CDDR ,lambda-form)))

    (cl:cond ((is-function-form fl)
           (let ((pl (function-form-body fl)))
             ;; Look for `(LAMBDA ...)
             (cl:cond ((is-lambda-form pl)
                    (multiple-value-bind (docstring declarations body)
                        (split-declarations (lambda-form-body pl))
                      (funcall receiver (lambda-form-arguments pl) docstring declarations body)))

                   ;; can't fake eta expand if arity is unknown
                   ((null arity) (funcall if-not-function))

                   ((symbolp pl)                ; is something like (function foo)
                    ;; perform eta expansion
                    (let ((arglist nil))
                      (dotimes (i arity)
                        (push (gensym "ARG-") arglist))
                      (funcall receiver arglist nil nil `((,pl ,@arglist)))))

                   (t (funcall if-not-function)))))

          ;; Look for naked '(lambda ...)
          ;; treat as if it were '(function (lambda ...))
          ((is-lambda-form fl)
           (multiple-value-bind (docstring declarations body)
               (split-declarations (lambda-form-body fl))
             (funcall receiver (lambda-form-arguments fl) docstring declarations body)))

          ;; Can't fake an eta expansion if we don't know the arity.
          ((null arity) (funcall if-not-function))

          ;; Perform an ETA expansion
          ((symbolp fl)
           (let ((arglist nil))
             (dotimes (i arity)
               (push (gensym "ARG-") arglist))
             (funcall receiver arglist nil nil `((FUNCALL ,fl ,@arglist)))))

          (t (funcall if-not-function)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For when we want more fine grained control of which tail calls are
;;; optimized.

;;; Note that because of a Franz bug [spr21443],
;;; COMPILER::TAIL-CALL-NON-SELF-MERGE-SWITCH must be T for
;;; optimization of self-calls by internal functions to be optimized.
(defun expand-tcond (clause-list)
  (labels ((make-if (test consequence alternative)
             (if (eq test 't)
                 consequence
                 (if (eq test 'nil)
                     alternative
                     (if (and (consp test)
                              (eq (car test) 'NOT)
                              (consp (cdr test))
                              (not (consp (cddr test))))
                         (make-if (cadr test)
                                  alternative
                                  consequence)
                         `(IF ,test ,consequence ,alternative)))))

           (make-locally (decls body)
             (if (null decls)
                 (make-progn body)
                 `(LOCALLY ,@decls ,@body)))

           (make-progn (body)
             (if (null (cdr body))
                 (car body)
                 `(PROGN ,@body))))

    (if (consp clause-list)
        (let ((first-clause (car clause-list))
              (more-clauses (cdr clause-list)))
          (if (consp first-clause)
              (let ((test (car first-clause))
                    (consequence (cdr first-clause)))
                (if (null more-clauses)
                    (if (not (eq test 't))
                        (warn "No default clause for COND.")))
                (if (eq test 't)
                    (if more-clauses
                        (warn "Unreachable clauses in COND.")))
                (if (null consequence)
                    (let ((cv (gensym "COND-VALUE-")))
                      `(LET ((,cv ,test))
                            ,(make-if cv cv (if (null more-clauses)
                                                'NIL
                                                (expand-tcond (cdr clause-list))))))
                    (if (and (symbolp (car consequence))
                             (string-equal (symbol-name (car consequence)) "=>"))
                        (if (consp (cdr consequence))
                            (if (consp (cddr consequence))
                                (error "Only one thing may follow => in COND clause")
                                (destructure-function-lambda 1 (cadr consequence)
                                  (lambda (bvl docstring declarations body)
                                    (declare (ignore docstring))
                                    (if (null more-clauses)
                                        `(LET ((,(car bvl) ,test))
                                           ,(make-if (car bvl)
                                                     (make-locally declarations body)
                                                     NIL))
                                        `(FUNCALL (OR (LET ((,(car bvl) ,test))
                                                        ,(make-if (car bvl)
                                                                  `(LAMBDA () ,@declarations ,@body)
                                                                  NIL))
                                                      (LAMBDA () ,(expand-tcond (cdr clause-list)))))))
                                  (lambda () (error "Non function follows => in COND clause "
                                                    #|(car consequence)|#))))
                            (error "Nothing follows => in COND clause"))
                        (MAKE-IF test
                                 (make-progn consequence)
                                 (if (null more-clauses)
                                     'NIL
                                     (expand-tcond (cdr clause-list)))))))
              (error "Illegal COND clause ~s" first-clause)))
        (if (null clause-list)
            (error "Empty COND clause.")
            (error "Improper tail ~s found in COND clauses." clause-list)))))

;;; !!! tail-cond originally shadowed cond.
;;;     now only used when they have this => thingy
;;;     hope to eventually removed totally
(defmacro tcond (&rest more-clauses)
  (expand-tcond more-clauses))

(defmacro logical-pathname-p (thing)
  #+allegro `(excl::logical-pathname-p ,thing)
  #-allegro `(typep ,thing 'logical-pathname)
  )

(defmacro defvar-unbound (variable-name documentation)
  "Like DEFVAR, but the variable will be unbound rather than getting an initial value.
   This is useful for variables which should have no global value but might have a
   dynamically bound value."
  `(EVAL-WHEN (:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
              (DEFVAR ,variable-name)
              (SETF (DOCUMENTATION ',variable-name 'VARIABLE)
                    ,documentation)))


;;; This is just too damned useful.
;;; It'd be nice to overload CL:LET, but the series package
;;; is doing that already.
(defmacro named-let (name bindings &body body)
  `(LABELS ((,name ,(map 'list #'car bindings) ,@body))
     (,name ,@(map 'list #'cadr bindings))))

;;; !!!
;;; was in conditions.lsp but 
;;; but we are are not currently using changesafe's condition system.
(defmacro check-range (place min max)
  `(ASSERT (AND (<= ,min ,place)
                (< ,place ,max))
           (,place)
           "The value ~s is not in the range [~s, ~s)."
           ,place ,min ,max))


(defmacro defunimplemented (name lambda-list)
  "A macro for writing a stub function."
  (labels ((lambda-list-arguments (scan result)
             (cl:cond ((null scan) (nreverse result))
                   ((consp (car scan))
                    (lambda-list-arguments
                     (if (caddr scan)
                         (cons (caddr scan) (cdr scan))
                         (cdr scan))
                     (if (consp (caar scan))
                         (cons (cadaar scan) result)
                         (cons (caar scan) result))))
                   ((member (car scan) lambda-list-keywords)
                    (lambda-list-arguments (cdr scan) result))
                   (t (lambda-list-arguments (cdr scan) (cons (car scan) result))))))
  `(DEFUN ,name ,lambda-list
     (ERROR 'changesafe-unimplemented-function
            :subsystem ,(package-name *package*)
            :name ',name
            :arguments (list ,@(lambda-list-arguments lambda-list '()))))))
