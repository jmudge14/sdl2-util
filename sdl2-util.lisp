;;;; sdl2-util.lisp

(in-package #:sdl2-util)

(defun rgb (surface r g b)
  (sdl2:map-rgb (sdl2:surface-format surface) r g b))

(defun clear-surface (surface &optional (color nil))
  (let ((c (or color (rgb surface 0 0 0))))
    (sdl2:fill-rect surface nil c)))

(defmacro with-initialized-sdl (options &body body)
  "Executes BODY within an SDL context with some useful exports
   (with-sdl (:title title :w w :h h))
   Binds win, rend"
  (jmutil:with-interned-symbols (win rend)
    (destructuring-bind (&key (title "") (w 500) (h 500)) options 
      `(sdl2:with-init (:everything)
         (sdl2:with-window (,win :title ,title :w ,w :h ,h :flags '(:shown))
           (sdl2:with-renderer (,rend ,win #| :flags nil |#)
             ,@body))))))

(defmacro with-sdl-thread (options &body body)
  "Executes BODY within an SDL context in a new thread"
  `(bt:make-thread (lambda () (with-initialized-sdl ,options ,@body)) :name "macro sdl thread"))


(defun get-font (name size)
  "Return an SDL2-TTF font for later use in rendering"
  (unless (> 0 (sdl2-ttf:was-init))
    (sdl2-ttf:init))
  (sdl2-ttf:open-font name size))

(defun draw-text (rend txt font pos-x pos-y &optional (r 255) (g 0) (b 0) (a 255))
  "Draw the given text on the provided window and surface, optionally provide a color code (default to red)"
  (unless (> 0 (sdl2-ttf:was-init))
    (sdl2-ttf:init))
  (let* ((txt-surface (sdl2-ttf:render-text-solid font txt r g b a))
         (destination-rect (sdl2:make-rect pos-x
                                           pos-y 
                                           (sdl2:surface-width txt-surface)
                                           (sdl2:surface-height txt-surface)))
         (texture (sdl2:create-texture-from-surface rend txt-surface)))
    (sdl2:render-copy rend texture :dest-rect destination-rect)
    ; Release temporary data here
    ; (sdl2:free-surface txt-surface) ;finalized in sdl2-ttf
    (sdl2:destroy-texture texture)
    (values)))

(defmacro scancode-case (scancode &rest cases)
  "Case statement for SDL scancodes"
  (let ((gscancode (gensym)))
    (labels ((scancode-equals-case (this-case)
               (let* ((keyname (first this-case))
                      (body (rest this-case))
                      (klist (if (listp keyname)
                                 keyname
                                 (list keyname))))
                 (loop for k in klist
                       collect `(sdl2:scancode= ,gscancode ,k) into l
                       finally (return `((or ,@l) ,@body))))))
      (let ((cases-list (mapcar #'scancode-equals-case cases)))
        `(let ((,gscancode ,scancode)) (cond ,@cases-list))))))
