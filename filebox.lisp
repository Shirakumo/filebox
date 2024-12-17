(in-package #:rad-user)
(define-module #:filebox
  (:use #:cl #:radiance))

(in-package #:filebox)

(define-trigger radiance:startup ()
  (unless (config :key)
    (setf (config :key)
          (make-random-string))))

(define-trigger db:connected ()
  (db:create 'files '((name (:varchar 64))
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
  (cryptos:pbkdf2-hash pass (config :key)))

(defun to-secure-id (id)
  ;; Multiply by prime to make the offsets less predictable
  ;; And add a tiny hash to the base.
  (let* ((id (* id 2521 (expt 36 4)))
         (hash (logand (sxhash id) (1- (expt 36 4)))))
    (write-to-string (+ id hash) :base 36)))

(defun from-secure-id (id)
  (db:ensure-id (truncate (parse-integer id :radix 36) (* 2521 (expt 36 4)))))

(defun ensure-b64 (thing)
  (case (mod (length thing) 4)
    (0 thing)
    ((1 3) (format NIL "~a=" thing))
    (2 (format NIL "~a==" thing))))

(defun from-secure-id-old (id)
  (db:ensure-id (cryptos:decrypt (ensure-b64 id) (config :key))))

(defun ensure-file (file)
  (etypecase file
    (dm:data-model file)
    (db:id (or (dm:get-one 'files (db:query (:= '_id file)))
                (error "No such file found.")))
    (T (ensure-file (from-secure-id file)))))

(defun file-filename (file)
  (let ((file (ensure-file file)))
    (make-pathname :name (princ-to-string (dm:id file))
                   :type (mimes:mime-file-type (dm:field file "type")))))

(defun user-directory (user)
  (ensure-directories-exist
   (environment-module-pathname
    #.*package* :data (make-pathname :directory (list :relative (princ-to-string user))))))

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
    (make-url :domains '("filebox")
              :path (format NIL "~a" (to-secure-id (dm:id file))))))

(defun file-data (file)
  (alexandria:plist-hash-table
   (list :id (to-secure-id (dm:id file))
         :url (file-link file)
         :name (dm:field file "name")
         :type (dm:field file "type")
         :attrs (cl-ppcre:split "\\s+" (dm:field file "attrs"))
         :time (dm:field file "time")
         :author (dm:field file "author"))))

(defun deliver-file (file)
  (if (file-accessible-p file)
      (progn
        (setf (header "Cache-Control") "public, max-age=31536000")
        (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a\"" (dm:field file "name")))
        (setf (header "Access-Control-Allow-Origin") "*")
        (serve-file (file-pathname file) (dm:field file "type")))
      (error "Not permitted.")))

(define-page file-payload "filebox/file/(.+)" (:uri-groups (id))
  (handler-case
      (deliver-file (ensure-file (from-secure-id-old id)))
    (error (err)
      (error 'request-not-found :message (princ-to-string err)))))

(define-page file-payload-new "filebox/(.+)" (:uri-groups (id))
  (handler-case
      (deliver-file (ensure-file id))
    (error (err)
      (error 'request-not-found :message (princ-to-string err)))))

(define-page index "filebox/^$" (:access (perm filebox upload) :clip "filebox.ctml")
  (let ((username (user:username (auth:current))))
    (let ((files (dm:get 'files (db:query (:= 'author username)) :sort '((time :DESC) (name :ASC)) :amount 50)))
      (r-clip:process
       T
       :notice (cond ((get-var "upload") (format NIL "File <a href='~a' tabindex='-1'>uploaded</a>."
                                                 (make-url :domains '("filebox") :path (format NIL "/file/~a" (get-var "upload")))))
                     ((get-var "notice") (format NIL "Notice: ~a" (get-var "notice"))))
       :files files
       :available (format NIL "~,,'':d" (floor (/ (directory-free (user-directory username)) 1024)))
       :occupied (format NIL "~,,'':d" (floor (/ (directory-size (user-directory username)) 1024)))))))

(define-api filebox/upload (file &optional attrs name password) (:access (perm filebox upload))
  (let ((name (or* name (second file)))
        (type (mimes:mime-lookup (second file)))
        (password (or* password))
        (model (dm:hull 'files)))
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
        (redirect (make-url :domains '("filebox") :query `(("upload" . ,(to-secure-id (dm:id model))))) :as-is 303)
        (api-output
         (file-data model)
         :message "File uploaded."))))

(define-api filebox/delete (file) (:access (perm filebox delete))
  (let ((file (ensure-file file)))
    (uiop:delete-file-if-exists
     (file-pathname file))
    (dm:delete file)
    (if (string= (post/get "browser") "true")
        (redirect (make-url :domains '("filebox") :query '(("notice" . "File deleted"))) :as-is 303)
        (api-output "File deleted."))))

(define-api filebox/list (&optional amount start) (:access (perm filebox upload)) 
  (api-output (mapcar #'file-data (dm:get 'files (db:query (:= 'author (user:username (auth:current))))
                                          :sort '((time :DESC) (name :ASC))
                                          :amount (min 50 (parse-integer (or* amount "50")))
                                          :skip (parse-integer (or* start "0"))))))

(define-version-migration filebox (NIL 1.2.0)
  (let ((old (make-pathname :name NIL
                            :type NIL
                            :defaults (merge-pathnames "uploads/"
                                                       (mconfig-pathname #.*package*))))
        (new (environment-module-directory #.*package* :data)))
    (when (uiop:directory-exists-p old)
      (when (uiop:directory-exists-p new)
        (uiop:delete-directory-tree new :validate (constantly T)))
      (rename-file old new))))
