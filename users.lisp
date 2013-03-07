(defpackage #:github-user
  (:use :cl)
  (:import-from :cl-github #:define-github-command
                           #:booleanize-parameters
                           #:api-command
                           #:rel-path))

(in-package #:github-user)

(define-github-command get-user (id)
    (:docs "Retreive a single user, ID.")
  (api-command (rel-path "/users/~A" id) :method :get))
