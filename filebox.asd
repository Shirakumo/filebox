#|
This file is a part of TyNETv5/Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:filebox
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :version "1.0.1"
  :description "Simple file storage for Radiance"
  :homepage "https://github.com/Shirakumo/filebox"
  :components ((:file "filebox"))
  :depends-on ((:interface :auth)
               :r-data-model
               :r-clip
               :trivial-mimes
               :ubiquitous))
