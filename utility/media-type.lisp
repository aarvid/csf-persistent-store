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

#|(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(
            media-type/binary?
            media-type/records?
            media-type
            media-type/primary-type
            media-type/subtype
            find-media-type
            )))|#

;; Wrong, should implement.
(defun media-type/binary? (mt)
  (not (eq mt :text)))


(defclass media-type ()
  ((primary-type :initarg :primary-type
                 :reader media-type/primary-type)
   (subtype :initarg :subtype
            :reader media-type/subtype))
  (:documentation "Mime type."))

(defmacro define-media-type (primary-type subtype)
  `(EVAL-WHEN (:LOAD-TOPLEVEL :EXECUTE)
     (MAKE-INSTANCE 'MEDIA-TYPE
                    :PRIMARY-TYPE ',primary-type
                    :SUBTYPE ',subtype)))

(define-media-type :application :octet-stream) ; [RFC2045,RFC2046]

(define-media-type :text :calendar)     ; [RFC2445]
(define-media-type :text :css)          ; [RFC2318]
(define-media-type :text :directory)    ; [RFC2425]
(define-media-type :text :enriched)     ; [RFC1896]
(define-media-type :text :html)         ; [RFC2854]
(define-media-type :text :parityfec)    ; [RFC3009]
(define-media-type :text :plain)        ; [RFC2646,RFC2046]
(define-media-type :text :prs.lines.tag) ; [Lines]
(define-media-type :text :rfc822-headers) ; [RFC1892]
(define-media-type :text :richtext)     ; [RFC2045,RFC2046]
(define-media-type :text :rtf)          ; [Lindner]
(define-media-type :text :sgml)         ; [RFC1874]
(define-media-type :text :t140)         ; [RFC2793]
(define-media-type :text :tab-separated-values) ; [Lindner]
(define-media-type :text :uri-list)     ; [RFC2483]
(define-media-type :text :vnd.abc)      ; [Allen]
(define-media-type :text :vnd.curl)     ; [Byrnes]
(define-media-type :text :vnd.DMClientScript) ; [Bradley]
(define-media-type :text :vnd.fly)      ; [Gurney]
(define-media-type :text :vnd.fmi.flexstor) ; [Hurtta]
(define-media-type :text :vnd.in3d.3dml) ; [Powers]
(define-media-type :text :vnd.in3d.spot) ; [Powers]
(define-media-type :text :vnd.IPTC.NewsML) ; [IPTC]
(define-media-type :text :vnd.IPTC.NITF) ; [IPTC]
(define-media-type :text :vnd.latex-z)  ; [Lubos]
(define-media-type :text :vnd.motorola.reflex) ; [Patton]
(define-media-type :text :vnd.ms-mediapackage) ; [Nelson]
(define-media-type :text :vnd.net2phone.commcenter.command) ; [Xie]
(define-media-type :text :vnd.sun.j2me.app-descriptor) ; [G.Adams]
(define-media-type :text :vnd.wap.si)   ; [WAP-Forum]
(define-media-type :text :vnd.wap.sl)   ; [WAP-Forum]
(define-media-type :text :vnd.wap.wml)  ; [Stark]
(define-media-type :text :vnd.wap.wmlscript) ; [Stark]
(define-media-type :text :xml)          ; [RFC3023]
(define-media-type :text :xml-external-parsed-entity) ; [RFC3023]

(defunimplemented find-media-type (media-type subtype))
