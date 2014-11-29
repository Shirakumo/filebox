#|
 This file is a part of TyNETv5/Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rad-user)
(define-module #:filebox
  (:use #:cl #:radiance))

(in-package #:filebox)

(define-trigger radiance:startup-done ()
  (unless (uc:config-tree :filebox :key)
    (setf (uc:config-tree :filebox :key)
          (make-random-string))
    (radiance:save-config)))

(define-trigger db:connected ()
  (db:create 'filebox-files '((name (:varchar 64))
                              (type (:varchar 32))
                              (hash (:varchar 128))
                              (attrs :text)
                              (time (:integer 5))
                              (author (:varchar 32)))))

(defun hash-password (pass)
  (cryptos:pbkdf2-hash pass (uc:config-tree :filebox :key)))

(defun to-secure-id (id)
  (cryptos:encrypt (write-to-string id) (uc:config-tree :filebox :key)))

(defun from-secure-id (id)
  (parse-integer (cryptos:decrypt id (uc:config-tree :filebox :key))))

(defun ensure-file (file)
  (etypecase file
    (dm:data-model file)
    (fixnum (or (dm:get-one 'filebox-files (db:query (:= '_id file)))
                (error "No such file found.")))
    (string (ensure-file (from-secure-id file)))))

(defun file-filename (file)
  (let ((file (ensure-file file)))
    (make-pathname :name (princ-to-string (dm:id file))
                   :type (mimes:mime-file-type (dm:field file "type")))))

(defun file-pathname (file)
  (asdf:system-relative-pathname
   :filebox (merge-pathnames "uploads/" (file-filename file))))

(defun file-accessible-p (file)
  (or (and (auth:current)
           (string-equal (user:username (auth:current))
                         (dm:field file "author")))
      (and (string/= (dm:field file "hash") "")
           (or* (post/get "password"))
           (string= (dm:field file "hash")
                    (hash-password (post/get "password"))))
      (and (string= (dm:field file "hash") "")
           (not (search "private" (dm:field file "attrs"))))))

(defun file-link (file)
  (let ((file (ensure-file file)))
    (format NIL "/file/~a" (to-secure-id (dm:id file)))))

(define-page file #@"filebox/file/(.+)" (:uri-groups (id))
  (handler-case
      (let* ((file (ensure-file id)))
        (if (file-accessible-p file)
            (serve-file (file-pathname file) (dm:field file "type"))
            (error "Not permitted.")))
    (error (err)
      (error 'request-not-found :message (princ-to-string err)))))

(define-page index #@"filebox/^$" (:access (perm filebox upload) :lquery (template "filebox.ctml"))
  (let ((files (dm:get 'filebox-files (db:query (:= 'author (user:username (auth:current)))) :sort '((time :DESC) (name :ASC)))))
    (r-clip:process
     T
     :notice (cond ((get-var "upload") (format NIL "File <a href='/file/~a' tabindex='-1'>uploaded</a>." (get-var "upload")))
                   ((get-var "notice") (format NIL "Notice: ~a" (get-var "notice"))))
     :files files)))

(define-api filebox/upload (file &optional attrs name password) (:access (perm filebox upload))
  (let ((name (or* name (second file)))
        (type (mimes:mime-lookup (second file)))
        (password (or* password))
        (model (dm:hull 'filebox-files)))
    (db:with-transaction ()
      (setf (dm:field model "name") (if (< 64 (length name)) (subseq name 0 64) name)
            (dm:field model "type") type
            (dm:field model "attrs") (or attrs "")
            (dm:field model "hash") (if password (hash-password password) "")
            (dm:field model "time") (get-universal-time)
            (dm:field model "author") (user:username (auth:current)))
      (dm:insert model)
      (uiop:copy-file
       (first file) (file-pathname model)))
    (if (string= (post/get "browser") "true")
        (redirect (format NIL "/?upload=~a" (to-secure-id (dm:id model))) 303)
        (api-output "File uploaded."))))

(define-api filebox/delete (file) (:access (perm filebox delete))
  (let ((file (ensure-file file)))
    (uiop:delete-file-if-exists
     (file-pathname file))
    (dm:delete file)
    (if (string= (post/get "browser") "true")
        (redirect (format NIL "/?notice=File%20deleted.") 303)
        (api-output "File deleted."))))
