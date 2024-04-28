(defpackage :cl-github
  (:nicknames :github)
  (:use #:cl)
  (:export #:*username*
           #:*password*
           #:api-command
           #:create-repository
           #:list-repositories))

(in-package #:cl-github)

(defparameter +ratelimit-retry-max+ 8
  "Number of retries before failing")

(defparameter +ratelimit-min-wait+ 2
  "Minimum number of seconds to wait when rate limited")

(defvar *username* nil
  "Username to use for API calls")

(defvar *password* nil
  "Password to use for API calls")

(define-condition api-error (error)
  ((http-status :initarg :http-status
                :reader error-http-status)
   (http-headers :initarg :http-headers
                 :reader error-http-headers)
   (response :initarg :response
             :reader error-response))
  (:report (lambda (c stream)
             (format stream "github API error, HTTP status code ~A~%~A~@[~%~A~]"
                     (error-http-status c)
                     (error-http-headers c)
                     (error-response c)))))

(defun keyword-to-github-keyword (keyword)
  (string-downcase (substitute #\_ #\- (string keyword))))

(defun github-keyword-to-keyword (string)
  (let ((*package* (find-package :keyword)))
    (read-from-string (substitute #\- #\_ string))))

(defun plist-to-http-parameters (plist)
  (loop
     for (key value) on plist by #'cddr
     collect (cons (keyword-to-github-keyword key) value)))

(defun plist-to-hash-table (plist)
  (loop
     with hash-table = (make-hash-table :test #'equal)
     for (key value) on plist by #'cddr
     do (setf (gethash (keyword-to-github-keyword key) hash-table) value)
     finally (return hash-table)))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)
  "The Unix epoch as a universal time.")

(defun get-unix-time ()
  (- (get-universal-time) +unix-epoch+))

(defun api-command (url &key body (method :get) (username *username*) (password *password*) parameters)
  (let ((ratelimit-retry-count 0)
        (ratelimit-wait 0))

    ;; Handle rate limit errors according to Github's best practices
    ;; documentation:
    ;; https://docs.github.com/en/rest/using-the-rest-api/best-practices-for-using-the-rest-api?apiVersion=2022-11-28#handle-rate-limit-errors-appropriately

    (flet ((get-ratelimit-wait (headers)
             "Return the number of seconds to wait before retrying."
             (let ((retry-after (cdr (assoc :RETRY-AFTER headers)))
                   (ratelimit-reset (cdr (assoc :X-RATELIMIT-RESET headers))))
               (max +ratelimit-min-wait+
                    (cond
                      (retry-after
                       (parse-integer retry-after))
                      (ratelimit-reset
                       (- (parse-integer ratelimit-reset) (get-unix-time)))
                      ((< ratelimit-retry-count +ratelimit-retry-max+)
                       (incf ratelimit-retry-count)
                       (setf ratelimit-wait (max 60 (* 2 ratelimit-wait))))
                      (t (error 'api-error
                                :http-status 403
                                :http-headers headers
                                :response "Github API rate limit retries exceeded")))))))

      (tagbody
       :retry
         (multiple-value-bind
               (body status-code headers)
             (drakma:http-request (format nil "https://api.github.com~A" url)
                                  :method method
                                  :parameters (plist-to-http-parameters parameters)
                                  :basic-authorization (when username (list username password))
                                  :content-type "application/json"
                                  :content (when body
                                             (with-output-to-string (s)
                                               (yason:encode (plist-to-hash-table body) s))))
           (let* ((yason:*parse-object-as* :plist)
                  (yason:*parse-object-key-fn* 'github-keyword-to-keyword)
                  (response (when body
                              (yason:parse (flex:octets-to-string body :external-format :utf-8))))
                  (ratelimit-wait (when (= status-code 403) (get-ratelimit-wait headers))))
             (cond
               ((< status-code 300)
                (return-from api-command (values response headers)))
               (ratelimit-wait
                (format t "~&Github is rate limiting API calls. Sleeping for ~A seconds.~%" ratelimit-wait)
                (sleep ratelimit-wait)
                (go :retry))
               (t (error 'api-error
                         :http-status status-code
                         :http-headers headers
                         :response response)))))))))

(defmacro booleanize-parameters (plist &rest keys)
  ;; unhygienic
  `(setf ,plist (let (result)
                  (alexandria:doplist (key value ,plist (nreverse result))
                    (push key result)
                    (push (if (member key ',keys)
                              (if value
                                  "true"
                                  "false")
                              value)
                          result)))))

(defmacro define-github-command (name parameters &body body)
  ;; unhygienic
  `(prog1
       (defun ,name (&rest parameters &key ,@parameters)
         (declare (ignorable parameters ,@(loop for parameter in parameters
                                             collect (if (listp parameter) (first parameter) parameter))))
         ,@body)
     (export ',name)))

(define-github-command create-repository (name org description homepage public has-issues has-wiki has-downloads)
  (booleanize-parameters parameters :has-issues :has-wiki :has-downloads)
  (api-command (if org (format nil "/orgs/~A/repos" org) "/user/repos")
               :method :post
               :body parameters))

(defun do-header-links (fn headers)
  (dolist (h headers)
    (when (eq :link (car h))
      (funcall fn (cdr h)))))

(define-github-command list-repositories (org)
  (let ((url (if org
                 (format nil "/orgs/~A/repos?per_page=100" org)
                 "/user/repos?per_page=100"))
        (repos nil))
    (loop
       (unless url
         (return))
       (multiple-value-bind (list headers)
           (api-command url :method :get)
         (setf repos (append list repos))
         (setf url nil)
         (do-header-links
             (lambda (link)
               (cl-ppcre:do-register-groups (next)
                   ("<(?:https://[^/]+)(/[^<>]+)>; rel=\"next\""
                    link)
                 (format t "~&Following next link ~S~%" next)
                 (setf url next)))
           headers)))
    repos))
