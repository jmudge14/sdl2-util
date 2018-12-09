;;;; package.lisp

(defpackage #:sdl2-util
  (:use #:common-lisp
        #:alexandria
        #:sdl2
        #:bordeaux-threads)
  (:export ; From sdl2-timer.lisp
           #:make-sdl-timer
           #:make-sdl-userevent-timer
           #:remove-timers
           #:all-timers
           #:stop-timers
           #:start-timers
           #:set-timers-delay
           #:do-timers
           ; From sdl-util.lisp
           #:with-initialized-sdl
           #:with-sdl-thread
           #:rgb
           #:clear-surface
           #:draw-text
           #:scancode-case
           #:get-font
           ))



