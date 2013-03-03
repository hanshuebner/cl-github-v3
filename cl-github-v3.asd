(defsystem :cl-github-v3
  :serial t
  :version "0.0.1"
  :description "github V3 API library"
  :author "Hans Huebner"
  :depends-on (:drakma
               :yason
               :alexandria)
  :components ((:file "github")
               (:file "repos")
               (:file "gists")))
