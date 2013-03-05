(defpackage #:github-repo
  (:use :cl)
  (:import-from :cl-github #:define-github-command
                           #:api-command
                           #:rel-path))

(in-package #:github-repo)

(define-github-command create-repository (name org description homepage public
                                          has-issues has-wiki has-downloads)
    (:docs "Create a new repo, NAME, for the authenticated user. If ORG is provided,
creates a new repo for the given ORGANIZATION.")
  (cl-github:booleanize-parameters parameters :has-issues :has-wiki :has-downloads)
  (api-command (if org (rel-path "/orgs/~A/repos" org) "/user/repos")
               :method :post
               :body parameters))

(define-github-command list-repositories (org user)
    (:docs "List repositories for the given ORG or USER. If neither is given, list
repositories for the authenticated user.")
  (api-command (cond (org (rel-path "/orgs/~A/repos" org))
                     (user (rel-path "/users/~A/repos" user))
                     (t "/user/repos"))
               :method :get))

(define-github-command get-repository (owner repo)
    (:docs "Get the available metadat for OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A" owner repo) :method :get))

(define-github-command list-contributors (owner repo)
    (:docs "List the contributors to OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A/contributors" owner repo) :method :get))

(define-github-command list-languages (owner repo)
    (:docs "List the languages used in OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A/languages" owner repo) :method :get))

(define-github-command list-teams (owner repo)
    (:docs "List the teams involved in OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A/teams" owner repo) :method :get))

(define-github-command list-tags (owner repo)
    (:docs "List the git tags for OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A/tags" owner repo) :method :get))

(define-github-command list-branches (owner repo)
    (:docs "List the git branches for OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A/branches" owner repo) :method :get))

(define-github-command delete-repository (owner repo)
    (:docs "Delete the given OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A" owner repo) :method :delete))

(define-github-command get-branch (owner repo branch)
    (:docs "Get metadata about BRANCH from OWNER's REPO.")
  (api-command (rel-path "/repos/~A/~A/branches/~A" owner repo branch) :method :get))
