(defsystem plantworld
  :serial t
  :depends-on (#:sdl2 #:alexandria #:livesupport)
  :components ((:file "helpers")
               (:file "geometry")
               (:file "plantworld")))
