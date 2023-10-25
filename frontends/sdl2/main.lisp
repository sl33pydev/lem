(defpackage :lem-sdl2
  (:use :cl
        :lem-sdl2/sdl2
        :lem-sdl2/keyboard
        :lem-sdl2/font
        :lem-sdl2/icon
        :lem-sdl2/platform
        :lem-sdl2/resource
        :lem-sdl2/log
        :lem-sdl2/mouse)
  (:export :change-font
           :set-keyboard-layout
           :render
           :current-renderer))
(in-package :lem-sdl2)

(defconstant +display-width+ 100)
(defconstant +display-height+ 40)

(defvar +lem-x11-wm-class+ "Lem SDL2")

;; this is SDL2 way
;; if the stable version of SDL is 3, set WM_CLASS is set via hint SDL_HINT_APP_ID
;;
;; cf.
;; - how SDL3 gets WM_CLASS:
;;     - https://github.com/libsdl-org/SDL/blob/d3f2de7f297d761a7dc5b0dda3c7b5d7bd49eac9/src/video/x11/SDL_x11window.c#L633C40-L633C40
;; - how to set WM_CLASS in here:
;;     - SDL_SetHint() function with key SDL_HINT_APP_ID
;;     - https://wiki.libsdl.org/SDL2/SDL_SetHint
;;     - https://github.com/libsdl-org/SDL/blob/d3f2de7f297d761a7dc5b0dda3c7b5d7bd49eac9/src/core/unix/SDL_appid.c#L63C45-L63C45
(defun set-x11-wm-class (classname)
  (setf (uiop:getenv "SDL_VIDEO_X11_WMCLASS") classname))

(defvar *display*)

(defun char-width () (lem-sdl2/display::display-char-width *display*))
(defun char-height () (lem-sdl2/display::display-char-height *display*))
(defun current-renderer () (lem-sdl2/display::display-renderer *display*))

(defclass view ()
  ((window
    :initarg :window
    :reader view-window)
   (x
    :initarg :x
    :accessor view-x)
   (y
    :initarg :y
    :accessor view-y)
   (width
    :initarg :width
    :accessor view-width)
   (height
    :initarg :height
    :accessor view-height)
   (use-modeline
    :initarg :use-modeline
    :reader view-use-modeline)
   (texture
    :initarg :texture
    :accessor view-texture)
   (last-cursor-x
    :initform nil
    :accessor view-last-cursor-x)
   (last-cursor-y
    :initform nil
    :accessor view-last-cursor-y)))

(defmethod last-cursor-x ((view view))
  (or (view-last-cursor-x view)
      ;; fallback to v1
      (* (lem:last-print-cursor-x (view-window view))
         (lem-sdl2/display::display-char-width *display*))))

(defmethod last-cursor-y ((view view))
  (or (view-last-cursor-y view)
      ;; fallback to v1
      (* (lem:last-print-cursor-y (view-window view))
         (lem-sdl2/display::display-char-height *display*))))

(defun create-view (window x y width height use-modeline)
  (when use-modeline (incf height))
  (make-instance 'view
                 :window window
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline use-modeline
                 :texture (lem-sdl2/display::create-view-texture *display* width height)))

(defmethod delete-view ((view view))
  (when (view-texture view)
    (sdl2:destroy-texture (view-texture view))
    (setf (view-texture view) nil)))

(defmethod render-clear ((view view))
  (sdl2:set-render-target (lem-sdl2/display::display-renderer *display*) (view-texture view))
  (lem-sdl2/display::set-render-color *display* (lem-sdl2/display::display-background-color *display*))
  (sdl2:render-clear (lem-sdl2/display::display-renderer *display*)))

(defmethod resize ((view view) width height)
  (when (view-use-modeline view) (incf height))
  (setf (view-width view) width
        (view-height view) height)
  (sdl2:destroy-texture (view-texture view))
  (setf (view-texture view)
        (lem-sdl2/display::create-view-texture *display* width height)))

(defmethod move-position ((view view) x y)
  (setf (view-x view) x
        (view-y view) y))

(defmethod draw-window-border (view (window lem:floating-window))
  (when (and (lem:floating-window-border window)
             (< 0 (lem:floating-window-border window)))
    (sdl2:set-render-target (lem-sdl2/display::display-renderer *display*) (lem-sdl2/display::display-texture *display*))
    (lem-sdl2/display::render-border *display*
                   (lem:window-x window)
                   (lem:window-y window)
                   (lem:window-width window)
                   (lem:window-height window)
                   :without-topline (eq :drop-curtain (lem:floating-window-border-shape window)))))

(defmethod draw-window-border (view (window lem:window))
  (when (< 0 (lem:window-x window))
    (sdl2:set-render-target (lem-sdl2/display::display-renderer *display*) (lem-sdl2/display::display-texture *display*))
    (lem-sdl2/display::render-margin-line *display*
                        (lem:window-x window)
                        (lem:window-y window)
                        (lem:window-height window))))

(defmethod render-border-using-view ((view view))
  (draw-window-border view (view-window view)))

(defun on-mouse-button-down (button x y clicks)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4))))
    (when button
      (let ((char-x (lem-sdl2/display::scaled-char-width *display* x))
            (char-y (lem-sdl2/display::scaled-char-height *display* y)))
        (lem:send-event
          (lambda ()
            (lem:receive-mouse-button-down char-x char-y x y button
                                           clicks)))))))

(defun on-mouse-button-up (button x y)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4)))
        (char-x (lem-sdl2/display::scaled-char-width *display* x))
        (char-y (lem-sdl2/display::scaled-char-height *display* y)))
    (lem:send-event
     (lambda ()
       (lem:receive-mouse-button-up char-x char-y x y button)))))

(defun on-mouse-motion (x y state)
  (show-cursor)
  (let ((button (if (= sdl2-ffi:+sdl-button-lmask+ (logand state sdl2-ffi:+sdl-button-lmask+))
                    :button-1
                    nil)))
    (let ((char-x (lem-sdl2/display::scaled-char-width *display* x))
          (char-y (lem-sdl2/display::scaled-char-height *display* y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-motion char-x char-y x y button))))))

(defun on-mouse-wheel (wheel-x wheel-y which direction)
  (declare (ignore which direction))
  (show-cursor)
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (let ((char-x (lem-sdl2/display::scaled-char-width *display* x))
          (char-y (lem-sdl2/display::scaled-char-height *display* y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-wheel char-x char-y x y wheel-x wheel-y)
         (when (= 0 (lem:event-queue-length))
           (lem:redraw-display)))))))

(defun on-textediting (text)
  (handle-textediting (get-platform) text)
  (lem:send-event #'lem:redraw-display))

(defun on-textinput (value)
  (hide-cursor)
  (let ((text (etypecase value
                (integer (string (code-char value)))
                (string value))))
    (handle-text-input (get-platform) text)))

(defun on-keydown (key-event)
  (hide-cursor)
  (handle-key-down (get-platform) key-event))

(defun on-keyup (key-event)
  (handle-key-up (get-platform) key-event))

(defun on-windowevent (event)
  (alexandria:switch (event)
    (sdl2-ffi:+sdl-windowevent-shown+
     (lem-sdl2/display::notify-required-redisplay *display*))
    (sdl2-ffi:+sdl-windowevent-exposed+
     (lem-sdl2/display::notify-required-redisplay *display*))
    (sdl2-ffi:+sdl-windowevent-resized+
     (lem-sdl2/display::update-texture *display*)
     (lem-sdl2/display::notify-required-redisplay *display*))
    (sdl2-ffi:+sdl-windowevent-focus-gained+
     (setf (lem-sdl2/display::display-focus-p *display*) t))
    (sdl2-ffi:+sdl-windowevent-focus-lost+
     (setf (lem-sdl2/display::display-focus-p *display*) nil))))

(defun event-loop ()
  (sdl2:with-event-loop (:method :wait)
    (:quit ()
     #+windows
     (cffi:foreign-funcall "_exit")
     t)
    (:textinput (:text text)
     (on-textinput text))
    (:textediting (:text text)
     (on-textediting text))
    (:keydown (:keysym keysym)
     (on-keydown (keysym-to-key-event keysym)))
    (:keyup (:keysym keysym)
     (on-keyup (keysym-to-key-event keysym)))
    (:mousebuttondown (:button button :x x :y y :clicks clicks)
     (on-mouse-button-down button x y clicks))
    (:mousebuttonup (:button button :x x :y y)
     (on-mouse-button-up button x y))
    (:mousemotion (:x x :y y :state state)
     (on-mouse-motion x y state))
    (:mousewheel (:x x :y y :which which :direction direction)
     (on-mouse-wheel x y which direction))
    (:windowevent (:event event)
     (on-windowevent event))))

(defun init-application-icon (window)
  (let ((image (sdl2-image:load-image (get-resource-pathname "resources/icon.png"))))
    (sdl2-ffi.functions:sdl-set-window-icon window image)
    (sdl2:free-surface image)))

(defun adapt-high-dpi-display-scale ()
  (with-debug ("adapt-high-dpi-display-scale")
    (lem-sdl2/display::with-renderer (*display*)
      (multiple-value-bind (renderer-width renderer-height)
          (sdl2:get-renderer-output-size (lem-sdl2/display::display-renderer *display*))
        (let* ((window-width (lem-sdl2/display::display-window-width *display*))
               (window-height (lem-sdl2/display::display-window-height *display*))
               (scale-x (/ renderer-width window-width))
               (scale-y (/ renderer-height window-height)))
          (setf (lem-sdl2/display::display-scale *display*) (list scale-x scale-y)))))))

(defun adapt-high-dpi-font-size ()
  (with-debug ("adapt-high-dpi-font-size")
    (lem-sdl2/display::with-renderer (*display*)
      (let ((font-config (lem-sdl2/display::display-font-config *display*))
            (ratio (round (first (lem-sdl2/display::display-scale *display*)))))
        (lem-sdl2/display::change-font *display*
                                       (change-size font-config
                                                    (* ratio (lem:config :sdl2-font-size lem-sdl2/font::*default-font-size*)))
                                       nil)))))

(defun create-display (function)
  (set-x11-wm-class +lem-x11-wm-class+)
  (sdl2:with-init (:video)
    (sdl2-ttf:init)
    (sdl2-image:init '(:png))
    (unwind-protect
         (let* ((font-config (make-font-config))
                (font (open-font font-config))
                (char-width (font-char-width font))
                (char-height (font-char-height font)))
           (let ((window-width (* +display-width+ char-width))
                 (window-height (* +display-height+ char-height)))
             (sdl2:with-window (window :title "Lem"
                                       :w window-width
                                       :h window-height
                                       :flags '(:shown :resizable #+darwin :allow-highdpi))
               (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
                 (let* (#+darwin (renderer-size (multiple-value-list
                                                 (sdl2:get-renderer-output-size renderer)))
                        #+darwin (renderer-width (first renderer-size))
                        #+darwin(renderer-height (second renderer-size))
                        (scale-x #-darwin 1 #+darwin (/ renderer-width window-width))
                        (scale-y #-darwin 1 #+darwin (/ renderer-height window-height))
                        (texture (lem-sdl2/utils:create-texture renderer
                                                                (* scale-x window-width)
                                                                (* scale-y window-height))))
                   (setf *display*
                         (make-instance
                          'lem-sdl2/display::display
                          :font-config font-config
                          :font font
                          :renderer renderer
                          :window window
                          :texture texture
                          :char-width (font-char-width font)
                          :char-height (font-char-height font)
                          :scale (list scale-x scale-y)))
                   (init-application-icon window)
                   #+darwin
                   (adapt-high-dpi-font-size)
                   (sdl2:start-text-input)
                   (funcall function)
                   (event-loop))))))
      (sdl2-ttf:quit)
      (sdl2-image:quit))))

(defun sbcl-on-darwin-p ()
  (or #+(and sbcl darwin)
      t
      nil))

(defmethod lem-if:invoke ((implementation sdl2) function)
  (flet ((thunk ()
           (let ((editor-thread
                   (funcall function
                            ;; initialize
                            (lambda ())
                            ;; finalize
                            (lambda (report)
                              (when report
                                (do-log report))
                              (sdl2:push-quit-event)))))
             (declare (ignore editor-thread))
             nil)))
    (progn
      ;; called *before* any sdl windows are created
      (sdl2:set-hint :video-mac-fullscreen-spaces
		     ;; the sdl2 library expects zero or one NOTE since this
		     ;; is a preference let's not change the default here
		     ;; because it's easy enough to change it via a user's
		     ;; config
		     (if (lem:config :darwin-use-native-fullscreen) 1 0))
      (sdl2:make-this-thread-main (lambda ()
				    (create-display #'thunk)
				    (when (sbcl-on-darwin-p)
				      (cffi:foreign-funcall "_exit")))))))

(defmethod lem-if:get-background-color ((implementation sdl2))
  (with-debug ("lem-if:get-background-color")
    (lem-sdl2/display::display-background-color *display*)))

(defmethod lem-if:get-foreground-color ((implementation sdl2))
  (with-debug ("lem-if:get-foreground-color")
    (lem-sdl2/display::display-foreground-color *display*)))

(defmethod lem-if:update-foreground ((implementation sdl2) color)
  (with-debug ("lem-if:update-foreground" color)
    (setf (lem-sdl2/display::display-foreground-color *display*) (lem:parse-color color))))

(defmethod lem-if:update-background ((implementation sdl2) color)
  (with-debug ("lem-if:update-background" color)
    (setf (lem-sdl2/display::display-background-color *display*) (lem:parse-color color))))

(defmethod lem-if:display-width ((implementation sdl2))
  (with-debug ("lem-if:display-width")
    (lem-sdl2/display::with-renderer (*display*)
      (floor (lem-sdl2/display::display-width *display*) (lem-sdl2/display::display-char-width *display*)))))

(defmethod lem-if:display-height ((implementation sdl2))
  (with-debug ("lem-if:display-height")
    (lem-sdl2/display::with-renderer (*display*)
      (floor (lem-sdl2/display::display-height *display*) (lem-sdl2/display::display-char-height *display*)))))

(defmethod lem-if:display-title ((implementation sdl2))
  (with-debug ("lem-if:display-title")
    (sdl2:get-window-title (lem-sdl2/display::display-window *display*))))

(defmethod lem-if:set-display-title ((implementation sdl2) title)
  (with-debug ("lem-if:set-display-title")
    (sdl2:in-main-thread ()
      (lem-sdl2/display::with-renderer (*display*)
        (sdl2:set-window-title (lem-sdl2/display::display-window *display*) title)
        ;; return the title instead of nil
        title))))

(defmethod lem-if:display-fullscreen-p ((implementation sdl2))
  (with-debug ("lem-if:display-fullscreen-p")
    (not (null (member :fullscreen (sdl2:get-window-flags (lem-sdl2/display::display-window *display*)))))))

(defmethod lem-if:set-display-fullscreen-p ((implementation sdl2) fullscreen-p)
  (with-debug ("lem-if:set-display-fullscreen-p")
    (sdl2:in-main-thread ()
      (lem-sdl2/display::with-renderer (*display*)
        ;; always send :desktop over :fullscreen due to weird bugs on macOS
        (sdl2:set-window-fullscreen (lem-sdl2/display::display-window *display*)
                                    (if fullscreen-p :desktop))))))

(defmethod lem-if:make-view ((implementation sdl2) window x y width height use-modeline)
  (with-debug ("lem-if:make-view" window x y width height use-modeline)
    (lem-sdl2/display::with-renderer (*display*)
      (create-view window x y width height use-modeline))))

(defmethod lem-if:delete-view ((implementation sdl2) view)
  (with-debug ("lem-if:delete-view")
    (lem-sdl2/display::with-renderer (*display*)
      (delete-view view))))

(defmethod lem-if:clear ((implementation sdl2) view)
  (with-debug ("lem-if:clear" view)
    (lem-sdl2/display::with-renderer (*display*)
      (render-clear view))))

(defmethod lem-if:set-view-size ((implementation sdl2) view width height)
  (with-debug ("lem-if:set-view-size" view width height)
    (lem-sdl2/display::with-renderer (*display*)
      (resize view width height))))

(defmethod lem-if:set-view-pos ((implementation sdl2) view x y)
  (with-debug ("lem-if:set-view-pos" view x y)
    (lem-sdl2/display::with-renderer (*display*)
      (move-position view x y))))

(defmethod lem-if:redraw-view-before ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-before" view)
    (lem-sdl2/display::with-renderer (*display*)
      (render-border-using-view view))))

(defun render-view-texture-to-display (view)
  (sdl2:set-render-target (lem-sdl2/display::display-renderer *display*) (lem-sdl2/display::display-texture *display*))
  (sdl2:with-rects ((dest-rect (* (view-x view) (lem-sdl2/display::display-char-width *display*))
                               (* (view-y view) (lem-sdl2/display::display-char-height *display*))
                               (* (view-width view) (lem-sdl2/display::display-char-width *display*))
                               (* (view-height view) (lem-sdl2/display::display-char-height *display*))))
    (sdl2:render-copy (lem-sdl2/display::display-renderer *display*)
                      (view-texture view)
                      :dest-rect dest-rect)))

(defgeneric render (texture window buffer))

(defmethod lem-if:redraw-view-after ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-after" view)
    (lem-sdl2/display::with-renderer (*display*)
      (sdl2:with-rects ((view-rect 0
                                   0
                                   (* (view-width view) (lem-sdl2/display::display-char-width *display*))
                                   (* (1- (view-height view)) (lem-sdl2/display::display-char-height *display*))))
        (sdl2:render-set-viewport (lem-sdl2/display::display-renderer *display*) view-rect)
        (render (view-texture view)
                (view-window view)
                (lem:window-buffer (view-window view)))
        (sdl2:render-set-viewport (lem-sdl2/display::display-renderer *display*) nil))
      (render-view-texture-to-display view))))

(defmethod lem-if:will-update-display ((implementation sdl2))
  (with-debug ("will-update-display")
    (lem-sdl2/display::with-renderer (*display*)
      (sdl2:set-render-target (lem-sdl2/display::display-renderer *display*) (lem-sdl2/display::display-texture *display*))
      (lem-sdl2/display::set-render-color *display* (lem-sdl2/display::display-background-color *display*))
      (sdl2:render-clear (lem-sdl2/display::display-renderer *display*)))))

(defun set-input-method ()
  (let* ((view (lem:window-view (lem:current-window)))
         (cursor-x (last-cursor-x view))
         (cursor-y (last-cursor-y view))
         (text lem-sdl2/keyboard::*textediting-text*)
         (x (+ (* (view-x view) (lem-sdl2/display::display-char-width *display*))
               cursor-x))
         (y (+ (* (view-y view) (lem-sdl2/display::display-char-height *display*))
               cursor-y)))
    (sdl2:with-rects ((rect x y (* (lem-sdl2/display::display-char-width *display*) (lem:string-width text)) (lem-sdl2/display::display-char-height *display*)))
      (sdl2-ffi.functions:sdl-set-text-input-rect rect)
      (when (plusp (length text))
        (let* ((color (lem-sdl2/display::display-foreground-color *display*))
               (surface (sdl2-ttf:render-utf8-blended (lem-sdl2/display::display-cjk-normal-font *display*)
                                                      text
                                                      (lem:color-red color)
                                                      (lem:color-green color)
                                                      (lem:color-blue color)
                                                      0))
               (texture (sdl2:create-texture-from-surface (lem-sdl2/display::display-renderer *display*) surface)))
          (sdl2:with-rects ((rect x y (sdl2:surface-width surface) (sdl2:surface-height surface)))
            (lem-sdl2/display::set-render-color *display* (lem-sdl2/display::display-background-color *display*))
            (sdl2:render-fill-rect (lem-sdl2/display::display-renderer *display*) rect)
            (sdl2:render-copy (lem-sdl2/display::display-renderer *display*) texture :dest-rect rect))
          (sdl2:destroy-texture texture))))))

(defmethod lem-if:update-display ((implementation sdl2))
  (with-debug ("lem-if:update-display")
    (lem-sdl2/display::with-renderer (*display*)
      (setf (lem-sdl2/display::display-redraw-at-least-once-p *display*) t)
      (sdl2:set-render-target (lem-sdl2/display::display-renderer *display*) nil)
      (sdl2:render-copy (lem-sdl2/display::display-renderer *display*) (lem-sdl2/display::display-texture *display*))
      (set-input-method)
      (lem-sdl2/display::update-display *display*))))

(defmethod lem-if:increase-font-size ((implementation sdl2))
  (with-debug ("increase-font-size")
    (lem-sdl2/display::with-renderer (*display*)
      (let ((font-config (lem-sdl2/display::display-font-config *display*))
            (ratio (round (first (lem-sdl2/display::display-scale *display*)))))
        (lem-sdl2/display::change-font *display*
                     (change-size font-config
                                  (+ (font-config-size font-config) ratio)))))))

(defmethod lem-if:decrease-font-size ((implementation sdl2))
  (with-debug ("decrease-font-size")
    (lem-sdl2/display::with-renderer (*display*)
      (let ((font-config (lem-sdl2/display::display-font-config *display*))
            (ratio (round (first (lem-sdl2/display::display-scale *display*)))))
        (lem-sdl2/display::change-font *display*
                     (change-size font-config
                                  (- (font-config-size font-config) ratio)))))))

(defmethod lem-if:resize-display-before ((implementation sdl2))
  (with-debug ("resize-display-before")
    (lem-sdl2/display::with-renderer (*display*)
      (lem-sdl2/display::clear *display*))))

(defmethod lem-if:get-font-list ((implementation sdl2))
  (get-font-list (get-platform)))

(defmethod lem-if:get-mouse-position ((implementation sdl2))
  (if (not (cursor-shown-p))
      (values 0 0)
      (multiple-value-bind (x y bitmask)
          (sdl2:mouse-state)
        (declare (ignore bitmask))
        (values (lem-sdl2/display::scaled-char-width *display* x)
                (lem-sdl2/display::scaled-char-height *display* y)))))

(defmethod lem-if:get-char-width ((implementation sdl2))
  (lem-sdl2/display::display-char-width *display*))

(defmethod lem-if:get-char-height ((implementation sdl2))
  (lem-sdl2/display::display-char-height *display*))

#-windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (lem-sdl2/log:with-debug ("clipboard-paste")
    (lem-sdl2/display::with-renderer (*display*)
      (sdl2-ffi.functions:sdl-get-clipboard-text))))

#+windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (lem-sdl2/log:with-debug ("clipboard-paste")
    (lem-sdl2/display::with-renderer (*display*)
      (with-output-to-string (out)
        (let ((text (sdl2-ffi.functions:sdl-get-clipboard-text)))
          (loop :for string :in (split-sequence:split-sequence #\newline text)
                :do (if (and (< 0 (length string))
                             (char= #\return (char string (1- (length string)))))
                        (write-line (subseq string 0 (1- (length string))) out)
                        (write-string string out))))))))

(defmethod lem-if:clipboard-copy ((implementation sdl2) text)
  (lem-sdl2/log:with-debug ("clipboard-copy")
    (lem-sdl2/display::with-renderer (*display*)
      (sdl2-ffi.functions:sdl-set-clipboard-text text))))

(lem:enable-clipboard)
