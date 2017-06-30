;;;; system definition file for persistent-store utilities.

(asdf:defsystem "csf-utility"
  :serial t
  :description "Utility and Config packages for Persistent Store"
  :author "Joe Marshall, modified by Andy Peterson <andy.arvid@gmail.com>"
  :license "BSD 3"
  :depends-on ("alexandria" "series" "cl-fad" "trivial-gray-streams"
                            #+(or cmu sbcl (and ccl (not windows)))
                            "local-time")
  :components ((:file "packages")
               (:file "declarations")
               (:file "types")
               (:file "ascii")
               (:file "utility-base")
               (:file "utility-macros")
               (:file "replacement-macros")
               (:file "fixnum")
               (:file "length")
               (:file "vector")
               (:file "utils")
               (:file "pathname-utilities")
               (:file "osapi")
               (:file "rbtree")
               (:file "oset")
               (:file "hash")               
               (:file "date-time")               
               (:file "timestamp")               
               (:file "guid")
               (:file "media-type")               
               ))
