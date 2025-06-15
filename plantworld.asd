(defsystem :plantworld
  :serial t
  :depends-on (#:sdl2 #:alexandria #:livesupport #:cl-hash-util)
  :components ((:file "package")
               (:file "helpers")
               (:file "geometry")
               (:file "plantworld")))
