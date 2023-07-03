(asdf:defsystem #:filebox
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
               :i-json
               :trivial-mimes
               :ubiquitous))
