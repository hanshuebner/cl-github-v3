(defpackage #:github-search
  (:use :cl)
  (:import-from :cl-github #:define-github-command
                           #:api-command
                           #:rel-path))

(in-package #:github-search)

(define-github-command search-issues (owner repo state keyword)
    (:docs "Search for issues on OWNER's REPO. STATE should be 'open' or 'closed'.
Only issues with KEYWORD in their description will be returned.
NOTE: This is a legacy API call.")
  (api-command (rel-path "/legacy/issues/search/~A/~A/~A/~A"
                         owner repo state keyword) :method :get))

(define-github-command search-repositories (keyword language page)
    (:docs "Search for repositories matching KEYWORD. If LANGUAGE is provided,
only return repos written in the given language. If PAGE is provided, return
the given PAGE of results. NOTE: This is a legacy API call.")
  (api-command (rel-path "/legacy/repos/search/~A" keyword) :method :get
               :body parameters))

(define-github-command search-users (keyword page)
    (:docs "Search for users matching KEYWORD. If PAGE is provided, return the
given PAGE of results. NOTE: This is a legacy API call.")
  (api-command (rel-path "/legacy/users/search/~A" keyword) :method :get
               :body parameters))
