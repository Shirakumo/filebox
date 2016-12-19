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
  (unless (config :filebox :key)
    (setf (config :filebox :key)
          (make-random-string))))

(define-trigger db:connected ()
  (db:create 'filebox-files '((name (:varchar 64))
                              (type (:varchar 32))
                              (hash (:varchar 128))
                              (attrs :text)
                              (time (:integer 5))
                              (author (:varchar 32)))))

(defun directory-size (pathname)
  #+:unix
  (parse-integer
   (uiop:run-program (format NIL "du -s ~a | awk '{print $1}'"
                             (uiop/run-program::escape-shell-token
                              (uiop:native-namestring pathname))) :output :string))
  #-:unix
  0)

(defun directory-free (pathname)
  #+:unix
  (parse-integer
   (uiop:run-program (format NIL "df -Pk ~a | tail -1 | awk '{print $4}'"
                             (uiop/run-program::escape-shell-token
                              (uiop:native-namestring pathname))) :output :string))
  #-:unix
  0)

(defun hash-password (pass)
  (cryptos:pbkdf2-hash pass (config :filebox :key)))

(defun to-secure-id (id)
  (cryptos:encrypt (write-to-string id) (config :filebox :key)))

(defun from-secure-id (id)
  (parse-integer (cryptos:decrypt id (config :filebox :key))))

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

(defun user-directory (user)
  (ensure-directories-exist
   (asdf:system-relative-pathname
    :filebox (format NIL "uploads/~a/" user))))

(defun file-directory (file)
  (user-directory (dm:field (ensure-file file) "author")))

(defun file-pathname (file)
  (let ((file (ensure-file file)))
    (merge-pathnames
     (file-directory file)
     (file-filename file))))

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
    (external-uri (format NIL "filebox/file/~a" (to-secure-id (dm:id file))))))

(define-page file "filebox/file/(.+)" (:uri-groups (id))
  (handler-case
      (let* ((file (ensure-file id)))
        (if (file-accessible-p file)
            (progn
              (setf (header "Cache-Control") "public, max-age=31536000")
              (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a\"" (dm:field file "name")))
              (serve-file (file-pathname file) (dm:field file "type")))
            (error "Not permitted.")))
    (error (err)
      (error 'request-not-found :message (princ-to-string err)))))

(define-page index "filebox/" (:access (perm filebox upload) :clip (@template "filebox.ctml"))
  (let ((username (user:username (auth:current))))
    (let ((files (dm:get 'filebox-files (db:query (:= 'author username)) :sort '((time :DESC) (name :ASC)))))
      (r-clip:process
       T
       :notice (cond ((get-var "upload") (format NIL "File <a href='~a' tabindex='-1'>uploaded</a>."
                                                 (external-uri (format NIL "filebox/file/~a" (get-var "upload")))))
                     ((get-var "notice") (format NIL "Notice: ~a" (get-var "notice"))))
       :files files
       :available (format NIL "~,,'':d" (floor (/ (directory-free (user-directory username)) 1024)))
       :occupied (format NIL "~,,'':d" (floor (/ (directory-size (user-directory username)) 1024)))))))

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
        (redirect (external-uri (format NIL "filebox/?upload=~a" (to-secure-id (dm:id model)))) 303)
        (api-output
         (alexandria:plist-hash-table
          (list :id (to-secure-id (dm:id model))
                :url (external-uri (format NIL "filebox/file/~a" (to-secure-id (dm:id model))))
                :name (dm:field model "name")
                :type (dm:field model "type")
                :attrs (cl-ppcre:split "\\s+" (dm:field model "attrs"))
                :time (dm:field model "time")
                :author (dm:field model "author")))
         :message "File uploaded."))))

(define-api filebox/delete (file) (:access (perm filebox delete))
  (let ((file (ensure-file file)))
    (uiop:delete-file-if-exists
     (file-pathname file))
    (dm:delete file)
    (if (string= (post/get "browser") "true")
        (redirect (external-uri "filebox/?notice=File%20deleted.") 303)
        (api-output "File deleted."))))
