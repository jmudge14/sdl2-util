(in-package #:sdl2-util)
;;;; SDL2-TIMER - Repeating event timer
;;;;              Pushes user events at regular intervals defined in milliseconds.
;;;;
;;;; Note - register event types with (sdl2:register-user-event-type) before starting timers.

(defvar *sdl-timers* nil)

(defclass sdl-timer () 
  ((delay     :accessor timer-delay
              :initarg :delay
              :initform 0    ;Default - update every tick
              :documentation "Delay in milliseconds between ticks")
   (next-tick :initform (sdl2:get-ticks))
   (stopped   :initarg :stopped
              :initform nil  ;Default - not stopped
              :documentation "If T, timer will not tick")
   (callback  :initform (lambda ())
              :initarg :callback
              :accessor timer-callback
              :documentation "Function to call on ticks")
   (ident     :initform nil
              :initarg :ident
              :accessor timer-ident
              :documentation "Optional non-unique identifier compared with EQUALP for stop-timer-by-ident"))
  (:documentation "Implements a repeating timer in SDL ticks."))

(defmethod timer-tick ((timer sdl-timer))
  (with-slots (delay next-tick callback stopped) timer
    (let ((current-tick (sdl2:get-ticks)))
      (when stopped (return-from timer-tick))
      (when (<= next-tick current-tick)
        (setf next-tick (+ next-tick delay))
        (funcall callback)))))

(defmethod timer-start ((timer sdl-timer))
  (with-slots (delay next-tick stopped) timer
    (setf stopped nil
          next-tick (+ (sdl2:get-ticks) delay))))

(defmethod timer-stop ((timer sdl-timer))
  (with-slots (stopped) timer
    (setf stopped t)))

(defun map-timers (func &key (ident nil))
  "Call func on each timer matching ident, or all if nil"
  (let ((timer-list (if ident
                        (remove-if-not (alexandria:curry #'equal ident)
                                       *sdl-timers*
                                       :key #'timer-ident)
                        *sdl-timers*)))
    (dolist (timer timer-list)
      (funcall func timer))))


(defun sdl2-user-event-pusher (event &key (datum nil))
  (lambda () (sdl2:push-user-event event datum)))


;;; Exported Items

(defun do-timers ()
  "Ticks all timers; run in SDL idle event."
  (map-timers #'timer-tick))

(defun make-sdl-timer (delay callback &key (stopped nil) (ident nil))
  "Creates a timer which executes the given callback each tick.
   If IDENT is not null and such a timer already exists, the delay and callback are updated."
  (if (and ident 
           (find ident *sdl-timers* :key #'timer-ident))
      (let ((timer (find ident *sdl-timers* :key #'timer-ident)))
        (setf (timer-delay timer) delay
              (timer-callback timer) callback))
      (push (make-instance 'sdl-timer
                           :delay delay
                           :stopped stopped
                           :callback callback
                           :ident ident)
            *sdl-timers*)))

(defun make-sdl-userevent-timer (delay event &key (stopped nil) (ident nil) (datum nil))
  "Creates a timer which pushes the given user event each tick, with optional datum"
  (make-sdl-timer delay
                  (sdl2-user-event-pusher event :datum datum)
                  :stopped stopped
                  :ident ident))

(defun remove-timers (ident)
  "Remove timers identified by ident. If ident is nil, this will be all anonymous timers."
  (setf *sdl-timers* 
        (remove-if (alexandria:curry #'equal ident)
                   *sdl-timers*
                   :key #'timer-ident)))

(defun all-timers ()
  "Return list of timer identifiers"
  (remove-duplicates (map 'list #'timer-ident *sdl-timers*)
                     :test #'equal))

(defun stop-timers (&key (ident nil))
  "Stopps all timers, or all matching ident if provided"
  (map-timers #'timer-stop
             :ident ident))

(defun start-timers (&key (ident nil))
  "Start all timers, or matching ident if provided"
  (map-timers #'timer-start :ident ident))

(defun set-timers-delay (delay &key (ident nil) (delay-func nil))
  "Update delay time for all timers, or all matching ident if provided.
   If delay-func is not nil, update by calling with current delay"
  (map-timers (lambda (timer) 
                (setf (timer-delay timer) 
                      (if delay-func
                          (funcall delay-func (timer-delay timer))
                          delay)))
              :ident ident))


