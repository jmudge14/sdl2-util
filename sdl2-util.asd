;;;; sdl2-util.asd

(asdf:defsystem #:sdl2-util
  :description "Describe sdl2-util here"
  :author "Jack Mudge <jmudge14@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:jmutil #:sdl2 #:sdl2-ttf #:bordeaux-threads #:alexandria)
  :components ((:file "package")
               (:file "sdl2-util")
               (:file "sdl2-timer")))
