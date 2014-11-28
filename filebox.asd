#|
This file is a part of TyNETv5/Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:filebox
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "filebox"))
  :depends-on ((:interface :auth)))
