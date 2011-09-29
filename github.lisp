(in-package #:cl-github)

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
  (substitute #\_ #\- (string keyword)))

(defun github-keyword-to-keyword (string)
  (intern (substitute #\- #\_ string) :keyword))

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

(defun api-command (url &key body (method :get) (username *username*) (password *password*) parameters)
  (assert (and (stringp username) (stringp password)) ()
          "Username and password strings must be supplied")
  (multiple-value-bind
        (body status-code headers)
      (drakma:http-request (format nil "https://api.github.com~A" url)
                           :method method
                           :parameters (plist-to-http-parameters parameters)
                           :basic-authorization (list username password)
                           :content-type "application/json"
                           :content (when body
                                      (with-output-to-string (s)
                                        (yason:encode (plist-to-hash-table body) s))))
    (let* ((yason:*parse-object-as* :plist)
           (yason:*parse-object-key-fn* 'github-keyword-to-keyword)
           (response (when body
                       (yason:parse (flex:octets-to-string body :external-format :utf-8)))))
      (if (< status-code 300)
          (values response headers)
          (error 'api-error
                 :http-status status-code
                 :http-headers headers
                 :response response)))))

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
         (declare (ignorable ,@(loop for parameter in parameters
                                  collect (if (listp parameter) (first parameter) parameter))))
         ,@body)
     (export ',name)))

(define-github-command create-repository (name org description homepage public has-issues has-wiki has-downloads)
  (booleanize-parameters parameters :has-issues :has-wiki :has-downloads)
  (api-command (if org (format nil "/orgs/~A/repos" org) "/user/repos")
               :method :post
               :body parameters))

(define-github-command list-repositories (org)
  (api-command (if org (format nil "/orgs/~A/repos" org) "/user/repos")
               :method :get))
