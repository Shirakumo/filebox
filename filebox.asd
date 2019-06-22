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
  :license "zlib"
  :version "1.2.0"
  :description "Simple file storage for Radiance"
  :homepage "https://Shirakumo.github.io/filebox/"
  :bug-tracker "https://github.com/Shirakumo/filebox/issues"
  :source-control (:git "https://github.com/Shirakumo/filebox.git")
  :components ((:file "filebox"))
  :depends-on ((:interface :auth)
               :r-data-model
               :r-clip
               :trivial-mimes
               :ubiquitous))
