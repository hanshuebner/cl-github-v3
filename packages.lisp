(in-package :cl-user)

(defpackage #:cl-github
  (:nicknames #:github)
  (:use #:cl)
  (:export #:*username* #:*password*
           #:api-command))