(defsystem :cl-github
  :serial t
  :depends-on (:drakma :yason :alexandria)
  :components ((:file "packages")
               (:file "github")))
