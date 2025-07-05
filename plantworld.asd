(defsystem :plantworld
  :serial t
  :depends-on (#:sdl2
               #:alexandria
               #:livesupport
               #:cl-hash-util
               #:noisy
               #:cl-ppcre
               #:str)
  :components ((:file "package")
               (:file "helpers")
               (:file "geometry")
               (:file "plantworld")
               (:file "parser")))
