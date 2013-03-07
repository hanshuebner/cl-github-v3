(defpackage :cl-github
  (:nicknames :github)
  (:use #:cl)
  (:export #:*username*
           #:*password*
           #:define-github-command
           #:api-command
           #:booleanize-parameters))

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

(defun parse-body (body headers)
  (multiple-value-bind (main-ct sub-ct params) (drakma:get-content-type headers)
    (cond ((string= "json" sub-ct)
           (yason:parse (flex:octets-to-string body :external-format :utf-8)))
          ((string= "application" main-ct) body))))

(defun api-command (url &key body (method :get) (username *username*) (password *password*) parameters)
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
           (response (when body (parse-body body headers))))
      (if (< status-code 300)
          (values response headers)
          (error 'api-error
                 :http-status status-code
                 :http-headers headers
                 :response response)))))

(defun rel-path (format-str &rest args)
  (apply 'format nil format-str args))

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

;; TODO: We are rate-limited to 5000/reqs an hour or a little over 83 a minute. How to best comply?
(defmacro define-github-command (name parameters (&key docs) &body body)
  ;; unhygienic
  `(prog1
       (defun ,name (&rest parameters &key ,@parameters)
         ,@(when docs (list docs))
         (declare (ignorable parameters ,@(loop for parameter in parameters
                                             collect (if (listp parameter) (first parameter) parameter))))
         ,@body)
     (export ',name)))
