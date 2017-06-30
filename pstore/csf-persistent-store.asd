;;;; ASDF System Definition file for csf-persistent-store.asd

(asdf:defsystem "csf-persistent-store"
  :serial t
  :description "ChangeSafe's Persistent Store transformed into a library for
open source lisps. "
  :author "Joe Marshall, Andy Peterson <andy.arvid@gmail.com>"
  :license "BSD 3"
  :depends-on ("series" "closer-mop" "csf-utility" "babel")
  :components ((:file "package")
               (:file "did")
               (:file "pnode")
               (:file "symtab")               
               (:file "pdefine")               
               (:file "objmap")               
               (:file "pstore")               
               (:file "pclass")               
               (:file "ptxn")               
               (:file "serial")
               (:file "objmap-serial")
               (:file "pstore-serial")
               (:file "ptxn-serial")
               (:file "pclass-ptxn")
               (:file "phash1")
               (:file "phash2")
               ))
