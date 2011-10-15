;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Utility functions
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2011 Philippe Brochard <hocwp@free.fr>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

;; Window states
(defconstant +withdrawn-state+ 0)
(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)


(defparameter *window-events* '(:structure-notify
				:property-change
				:colormap-change
				:focus-change
				:enter-window
				:exposure)
  "The events to listen for on managed windows.")


(defparameter +netwm-supported+
  '(:_NET_SUPPORTING_WM_CHECK
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    :_NET_WM_WINDOW_TYPE
    :_NET_CLIENT_LIST)
  "Supported NETWM properties.
Window types are in +WINDOW-TYPES+.")

(defparameter +netwm-window-types+
  '((:_NET_WM_WINDOW_TYPE_DESKTOP . :desktop)
    (:_NET_WM_WINDOW_TYPE_DOCK . :dock)
    (:_NET_WM_WINDOW_TYPE_TOOLBAR . :toolbar)
    (:_NET_WM_WINDOW_TYPE_MENU . :menu)
    (:_NET_WM_WINDOW_TYPE_UTILITY . :utility)
    (:_NET_WM_WINDOW_TYPE_SPLASH . :splash)
    (:_NET_WM_WINDOW_TYPE_DIALOG . :dialog)
    (:_NET_WM_WINDOW_TYPE_NORMAL . :normal))
  "Alist mapping NETWM window types to keywords.")


(defmacro with-xlib-protect (&body body)
  "Prevent Xlib errors"
  `(handler-case
       (with-simple-restart (top-level "Return to clfswm's top level")
	 ,@body)
     ((or xlib:match-error xlib:window-error xlib:drawable-error) (c)
       (progn
	 (dbg "Ignore Xlib Error" c ',body)
	 (unassoc-keyword-handle-event)
	 (assoc-keyword-handle-event 'main-mode)
	 (setf *in-second-mode* nil)))))


(defmacro with-x-pointer (&body body)
  "Bind (x y) to mouse pointer positions"
  `(multiple-value-bind (x y)
       (xlib:query-pointer *root*)
     ,@body))



(declaim (inline window-x2 window-y2))
(defun window-x2 (window)
  (+ (xlib:drawable-x window) (xlib:drawable-width window)))

(defun window-y2 (window)
  (+ (xlib:drawable-y window) (xlib:drawable-height window)))



;;;
;;; Events management functions.
;;;
(defparameter *unhandled-events* nil)
(defparameter *current-event-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keyword->handle-event (mode keyword)
    (symb 'handle-event-fun "-" mode "-" keyword)))

(defun handle-event->keyword (symbol)
  (let* ((name (string-downcase (symbol-name symbol)))
	 (pos (search "handle-event-fun-" name)))
    (when (and pos (zerop pos))
      (let ((pos-mod (search "mode" name)))
	(when pos-mod
	  (values (intern (string-upcase (subseq name (+ pos-mod 5))) :keyword)
		  (subseq name (length "handle-event-fun-") (1- pos-mod))))))))

(defparameter *handle-event-fun-symbols* nil)

(defun fill-handle-event-fun-symbols ()
  (with-all-internal-symbols (symbol :clfswm)
    (let ((pos (symbol-search "handle-event-fun-" symbol)))
      (when (and pos (zerop pos))
	(pushnew symbol *handle-event-fun-symbols*)))))


(defmacro with-handle-event-symbol ((mode) &body body)
  "Bind symbol to all handle event functions available in mode"
  `(let ((pattern (format nil "handle-event-fun-~A" ,mode)))
     (dolist (symbol *handle-event-fun-symbols*)
       (let ((pos (symbol-search pattern symbol)))
	 (when (and pos (zerop pos))
	   ,@body)))))


(defun find-handle-event-function (&optional (mode ""))
  "Print all handle event functions available in mode"
  (with-handle-event-symbol (mode)
    (print symbol)))

(defun assoc-keyword-handle-event (mode)
  "Associate all keywords in mode to their corresponding handle event functions.
For example: main-mode :key-press is bound to handle-event-fun-main-mode-key-press"
  (setf *current-event-mode* mode)
  (with-handle-event-symbol (mode)
    (let ((keyword (handle-event->keyword symbol)))
      (when (fboundp symbol)
	#+:event-debug
	(progn
	  (format t "~&Associating: ~S with ~S~%" symbol keyword)
	  (force-output))
	(setf (symbol-function keyword) (symbol-function symbol))))))

(defun unassoc-keyword-handle-event (&optional (mode ""))
  "Unbound all keywords from their corresponding handle event functions."
  (setf *current-event-mode* nil)
  (with-handle-event-symbol (mode)
    (let ((keyword (handle-event->keyword symbol)))
      (when (fboundp keyword)
	#+:event-debug
	(progn
	  (format t "~&Unassociating: ~S  ~S~%" symbol keyword)
	  (force-output))
	(fmakunbound keyword)))))

(defmacro define-handler (mode keyword args &body body)
  "Like a defun but with a name expanded as handle-event-fun-'mode'-'keyword'
For example (define-handler main-mode :key-press (args) ...)
Expand in handle-event-fun-main-mode-key-press"
  `(defun ,(keyword->handle-event mode keyword) (&rest event-slots &key #+:event-debug event-key ,@args &allow-other-keys)
     (declare (ignorable event-slots))
     #+:event-debug (print (list *current-event-mode* event-key))
     ,@body))



(defun handle-event (&rest event-slots &key event-key &allow-other-keys)
  (with-xlib-protect
    (if (fboundp event-key)
	(apply event-key event-slots)
	#+:event-debug (pushnew (list *current-event-mode* event-key) *unhandled-events* :test #'equal))
    (xlib:display-finish-output *display*))
  t)





(defun parse-display-string (display)
  "Parse an X11 DISPLAY string and return the host and display from it."
  (let* ((colon (position #\: display))
	 (host (subseq display 0 colon))
	 (rest (subseq display (1+ colon)))
	 (dot (position #\. rest))
	 (num (parse-integer (subseq rest 0 dot))))
    (values host num)))


(defun banish-pointer ()
  "Move the pointer to the lower right corner of the screen"
  (with-placement (*banish-pointer-placement* x y)
    (xlib:warp-pointer *root* x y)))



(defun window-state (win)
  "Get the state (iconic, normal, withdrawn) of a window."
  (first (xlib:get-property win :WM_STATE)))


(defun set-window-state (win state)
  "Set the state (iconic, normal, withdrawn) of a window."
  (xlib:change-property win
			:WM_STATE
			(list state)
			:WM_STATE
			32))

(defsetf window-state set-window-state)



(defun window-hidden-p (window)
  (eql (window-state window) +iconic-state+))


(defun null-size-window-p (window)
  (let ((hints (xlib:wm-normal-hints window)))
    (and hints
	 (not (or (xlib:wm-size-hints-width hints)
		  (xlib:wm-size-hints-height hints)
		  (xlib:wm-size-hints-win-gravity hints)))
	 (xlib:wm-size-hints-user-specified-position-p hints))))






(defun unhide-window (window)
  (when window
    (when (window-hidden-p window)
      (xlib:map-window window)
      (setf (window-state window) +normal-state+
	    (xlib:window-event-mask window) *window-events*))))


(defun map-window (window)
  (when window
    (xlib:map-window window)))


(defun delete-window (window)
  (send-client-message window :WM_PROTOCOLS
		       (xlib:intern-atom *display* "WM_DELETE_WINDOW")))

(defun destroy-window (window)
  (xlib:kill-client *display* (xlib:window-id window)))


;;(defconstant +exwm-atoms+
;;  (list "_NET_SUPPORTED"              "_NET_CLIENT_LIST"
;;	"_NET_CLIENT_LIST_STACKING"   "_NET_NUMBER_OF_DESKTOPS"
;;	"_NET_CURRENT_DESKTOP"        "_NET_DESKTOP_GEOMETRY"
;;	"_NET_DESKTOP_VIEWPORT"       "_NET_DESKTOP_NAMES"
;;	"_NET_ACTIVE_WINDOW"          "_NET_WORKAREA"
;;	"_NET_SUPPORTING_WM_CHECK"    "_NET_VIRTUAL_ROOTS"
;;	"_NET_DESKTOP_LAYOUT"
;;
;;        "_NET_RESTACK_WINDOW"         "_NET_REQUEST_FRAME_EXTENTS"
;;        "_NET_MOVERESIZE_WINDOW"      "_NET_CLOSE_WINDOW"
;;        "_NET_WM_MOVERESIZE"
;;
;;	"_NET_WM_SYNC_REQUEST"        "_NET_WM_PING"
;;
;;	"_NET_WM_NAME"                "_NET_WM_VISIBLE_NAME"
;;	"_NET_WM_ICON_NAME"           "_NET_WM_VISIBLE_ICON_NAME"
;;	"_NET_WM_DESKTOP"             "_NET_WM_WINDOW_TYPE"
;;	"_NET_WM_STATE"               "_NET_WM_STRUT"
;;	"_NET_WM_ICON_GEOMETRY"       "_NET_WM_ICON"
;;	"_NET_WM_PID"                 "_NET_WM_HANDLED_ICONS"
;;	"_NET_WM_USER_TIME"           "_NET_FRAME_EXTENTS"
;;        ;; "_NET_WM_MOVE_ACTIONS"
;;
;;	"_NET_WM_WINDOW_TYPE_DESKTOP" "_NET_WM_STATE_MODAL"
;;	"_NET_WM_WINDOW_TYPE_DOCK"    "_NET_WM_STATE_STICKY"
;;	"_NET_WM_WINDOW_TYPE_TOOLBAR" "_NET_WM_STATE_MAXIMIZED_VERT"
;;	"_NET_WM_WINDOW_TYPE_MENU"    "_NET_WM_STATE_MAXIMIZED_HORZ"
;;	"_NET_WM_WINDOW_TYPE_UTILITY" "_NET_WM_STATE_SHADED"
;;	"_NET_WM_WINDOW_TYPE_SPLASH"  "_NET_WM_STATE_SKIP_TASKBAR"
;;	"_NET_WM_WINDOW_TYPE_DIALOG"  "_NET_WM_STATE_SKIP_PAGER"
;;	"_NET_WM_WINDOW_TYPE_NORMAL"  "_NET_WM_STATE_HIDDEN"
;;	                              "_NET_WM_STATE_FULLSCREEN"
;;				      "_NET_WM_STATE_ABOVE"
;;				      "_NET_WM_STATE_BELOW"
;;				      "_NET_WM_STATE_DEMANDS_ATTENTION"
;;
;;	"_NET_WM_ALLOWED_ACTIONS"
;;	"_NET_WM_ACTION_MOVE"
;;	"_NET_WM_ACTION_RESIZE"
;;	"_NET_WM_ACTION_SHADE"
;;	"_NET_WM_ACTION_STICK"
;;	"_NET_WM_ACTION_MAXIMIZE_HORZ"
;;	"_NET_WM_ACTION_MAXIMIZE_VERT"
;;	"_NET_WM_ACTION_FULLSCREEN"
;;	"_NET_WM_ACTION_CHANGE_DESKTOP"
;;	"_NET_WM_ACTION_CLOSE"
;;
;;	))
;;
;;
;;(defun intern-atoms (display)
;;  (declare (type xlib:display display))
;;  (mapcar #'(lambda (atom-name) (xlib:intern-atom display atom-name))
;;	  +exwm-atoms+)
;;  (values))
;;
;;
;;
;;(defun get-atoms-property (window property-atom atom-list-p)
;;  "Returns a list of atom-name (if atom-list-p is t) otherwise returns
;;   a list of atom-id."
;;  (xlib:get-property window property-atom
;;		     :transform (when atom-list-p
;;				  (lambda (id)
;;				    (xlib:atom-name (xlib:drawable-display window) id)))))
;;
;;(defun set-atoms-property (window atoms property-atom &key (mode :replace))
;;  "Sets the property designates by `property-atom'. ATOMS is a list of atom-id
;;   or a list of keyword atom-names."
;;  (xlib:change-property window property-atom atoms :ATOM 32
;;			:mode mode
;;			:transform (unless (integerp (car atoms))
;;				     (lambda (atom-key)
;;				       (xlib:find-atom (xlib:drawable-display window) atom-key)))))
;;
;;
;;
;;
;;(defun net-wm-state (window)
;;  (get-atoms-property window :_NET_WM_STATE t))
;;
;;(defsetf net-wm-state (window &key (mode :replace)) (states)
;;  `(set-atoms-property ,window ,states :_NET_WM_STATE :mode ,mode))


(defun hide-window (window)
  (when window
    (setf (window-state window) +iconic-state+
	  (xlib:window-event-mask window) (remove :structure-notify *window-events*))
    (xlib:unmap-window window)
    (setf (xlib:window-event-mask window) *window-events*)))



(defun window-type (window)
  "Return one of :desktop, :dock, :toolbar, :utility, :splash,
:dialog, :transient, :maxsize and :normal."
  (or (and (let ((hints (xlib:wm-normal-hints window)))
             (and hints (or (xlib:wm-size-hints-max-width hints)
                            (xlib:wm-size-hints-max-height hints)
                            (xlib:wm-size-hints-min-aspect hints)
                            (xlib:wm-size-hints-max-aspect hints))))
           :maxsize)
      (let ((net-wm-window-type (xlib:get-property window :_NET_WM_WINDOW_TYPE)))
        (when net-wm-window-type
          (dolist (type-atom net-wm-window-type)
            (when (assoc (xlib:atom-name *display* type-atom) +netwm-window-types+)
              (return (cdr (assoc (xlib:atom-name *display* type-atom) +netwm-window-types+)))))))
      (and (xlib:get-property window :WM_TRANSIENT_FOR)
           :transient)
      :normal))





;;; Stolen from Eclipse
(defun send-configuration-notify (window x y w h bw)
  "Send a synthetic configure notify event to the given window (ICCCM 4.1.5)"
  (xlib:send-event window :configure-notify (xlib:make-event-mask :structure-notify)
                   :event-window window
                   :window window
                   :x x :y y
                   :width w
                   :height h
                   :border-width bw
                   :propagate-p nil))



(defun send-client-message (window type &rest data)
  "Send a client message to a client's window."
  (xlib:send-event window
		   :client-message nil
		   :window window
		   :type type
		   :format 32
		   :data data))





(defun raise-window (window)
  "Map the window if needed and bring it to the top of the stack. Does not affect focus."
  (when (xlib:window-p window)
    (when (window-hidden-p window)
      (unhide-window window))
    (setf (xlib:window-priority window) :above)))

(defun focus-window (window)
  "Give the window focus."
  (when (xlib:window-p window)
    (xlib:set-input-focus *display* window :parent)))

(defun raise-and-focus-window (window)
  "Raise and focus."
  (raise-window window)
  (focus-window window))

(defun no-focus ()
  "don't focus any window but still read keyboard events."
  (xlib:set-input-focus *display* *no-focus-window* :pointer-root))


(defun lower-window (window sibling)
  "Map the window if needed and bring it just above sibling. Does not affect focus."
  (when (xlib:window-p window)
    (when (window-hidden-p window)
      (unhide-window window))
    (setf (xlib:window-priority window sibling) :below)))




(let ((cursor-font nil)
      (cursor nil)
      (pointer-grabbed nil))
  (defun free-grab-pointer ()
    (when cursor
      (xlib:free-cursor cursor)
      (setf cursor nil))
    (when cursor-font
      (xlib:close-font cursor-font)
      (setf cursor-font nil)))

  (defun xgrab-init-pointer ()
    (setf pointer-grabbed nil))

    (defun xgrab-pointer-p ()
      pointer-grabbed)

    (defun xgrab-pointer (root cursor-char cursor-mask-char
			  &optional (pointer-mask '(:enter-window :pointer-motion
						    :button-press :button-release)) owner-p)
      "Grab the pointer and set the pointer shape."
      (when pointer-grabbed
	(xungrab-pointer))
      (setf pointer-grabbed t)
      (let* ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
	     (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)))
	(cond (cursor-char
	       (setf cursor-font (xlib:open-font *display* "cursor")
		     cursor (xlib:create-glyph-cursor :source-font cursor-font
						      :source-char (or cursor-char 68)
						      :mask-font cursor-font
						      :mask-char (or cursor-mask-char 69)
						      :foreground black
						      :background white))
	       (xlib:grab-pointer root pointer-mask
				       :owner-p owner-p  :sync-keyboard-p nil :sync-pointer-p nil :cursor cursor))
	      (t
	       (xlib:grab-pointer root pointer-mask
				       :owner-p owner-p  :sync-keyboard-p nil :sync-pointer-p nil)))))

    (defun xungrab-pointer ()
      "Remove the grab on the cursor and restore the cursor shape."
      (setf pointer-grabbed nil)
      (xlib:ungrab-pointer *display*)
      (xlib:display-finish-output *display*)
      (free-grab-pointer)))


(let ((keyboard-grabbed nil))
  (defun xgrab-init-keyboard ()
    (setf keyboard-grabbed nil))

  (defun xgrab-keyboard-p ()
    keyboard-grabbed)

  (defun xgrab-keyboard (root)
    (setf keyboard-grabbed t)
    (xlib:grab-keyboard root :owner-p nil :sync-keyboard-p nil :sync-pointer-p nil))


  (defun xungrab-keyboard ()
    (setf keyboard-grabbed nil)
    (xlib:ungrab-keyboard *display*)))






(defun ungrab-all-buttons (window)
  (xlib:ungrab-button window :any :modifiers :any))

(defun grab-all-buttons (window)
  (ungrab-all-buttons window)
  (xlib:grab-button window :any '(:button-press :button-release :pointer-motion)
		    :modifiers :any
		    :owner-p nil
		    :sync-pointer-p t
		    :sync-keyboard-p nil))

(defun ungrab-all-keys (window)
  (xlib:ungrab-key window :any :modifiers :any))


(defun stop-button-event ()
  (xlib:allow-events *display* :sync-pointer))

(defun replay-button-event ()
  (xlib:allow-events *display* :replay-pointer))









;;; Mouse action on window
(let (add-fn add-arg dx dy window)
  (define-handler move-window-mode :motion-notify (root-x root-y)
    (unless (compress-motion-notify)
      (if add-fn
          (multiple-value-bind (move-x move-y)
              (apply add-fn add-arg)
            (when move-x
              (setf (xlib:drawable-x window) (+ root-x dx)))
            (when move-y
              (setf (xlib:drawable-y window) (+ root-y dy))))
          (setf (xlib:drawable-x window) (+ root-x dx)
                (xlib:drawable-y window) (+ root-y dy)))))

  (define-handler move-window-mode :key-release ()
    (throw 'exit-move-window-mode nil))

  (define-handler move-window-mode :button-release ()
    (throw 'exit-move-window-mode nil))

  (defun move-window (orig-window orig-x orig-y &optional additional-fn additional-arg)
    (setf window orig-window
	  add-fn additional-fn
	  add-arg additional-arg
	  dx (- (xlib:drawable-x window) orig-x)
	  dy (- (xlib:drawable-y window) orig-y)
	  (xlib:window-border window) (get-color *color-move-window*))
    (raise-window window)
    (let ((pointer-grabbed-p (xgrab-pointer-p)))
      (unless pointer-grabbed-p
	(xgrab-pointer *root* nil nil))
      (when additional-fn
	(apply additional-fn additional-arg))
      (generic-mode 'move-window-mode 'exit-move-window-mode
		    :original-mode '(main-mode))
      (unless pointer-grabbed-p
	(xungrab-pointer)))))


(let (add-fn add-arg window
	     o-x o-y
	     orig-width orig-height
	     min-width max-width
	     min-height max-height)
  (define-handler resize-window-mode :motion-notify (root-x root-y)
    (unless (compress-motion-notify)
      (if add-fn
          (multiple-value-bind (resize-w resize-h)
              (apply add-fn add-arg)
            (when resize-w
              (setf (xlib:drawable-width window) (min (max (+ orig-width (- root-x o-x)) 10 min-width) max-width)))
            (when resize-h
              (setf (xlib:drawable-height window) (min (max (+ orig-height (- root-y o-y)) 10 min-height) max-height))))
          (setf (xlib:drawable-width window) (min (max (+ orig-width (- root-x o-x)) 10 min-width) max-width)
                (xlib:drawable-height window) (min (max (+ orig-height (- root-y o-y)) 10 min-height) max-height)))))

  (define-handler resize-window-mode :key-release ()
    (throw 'exit-resize-window-mode nil))

  (define-handler resize-window-mode :button-release ()
    (throw 'exit-resize-window-mode nil))

  (defun resize-window (orig-window orig-x orig-y &optional additional-fn additional-arg)
    (let* ((pointer-grabbed-p (xgrab-pointer-p))
	   (hints (xlib:wm-normal-hints orig-window)))
      (setf window orig-window
	    add-fn additional-fn
	    add-arg additional-arg
	    o-x orig-x
	    o-y orig-y
	    orig-width (xlib:drawable-width window)
	    orig-height (xlib:drawable-height window)
	    min-width (or (and hints (xlib:wm-size-hints-min-width hints)) 0)
	    min-height (or (and hints (xlib:wm-size-hints-min-height hints)) 0)
	    max-width (or (and hints (xlib:wm-size-hints-max-width hints)) most-positive-fixnum)
	    max-height (or (and hints (xlib:wm-size-hints-max-height hints)) most-positive-fixnum)
	    (xlib:window-border window) (get-color *color-move-window*))
      (raise-window window)
      (unless pointer-grabbed-p
	(xgrab-pointer *root* nil nil))
      (when additional-fn
	(apply additional-fn additional-arg))
      (generic-mode 'resize-window-mode 'exit-resize-window-mode
		    :original-mode '(main-mode))
      (unless pointer-grabbed-p
	(xungrab-pointer)))))


(define-handler wait-mouse-button-release-mode :button-release ()
  (throw 'exit-wait-mouse-button-release-mode nil))

(defun wait-mouse-button-release (&optional cursor-char cursor-mask-char)
  (let ((pointer-grabbed-p (xgrab-pointer-p)))
    (unless pointer-grabbed-p
      (xgrab-pointer *root* cursor-char cursor-mask-char))
    (generic-mode 'wait-mouse-button-release 'exit-wait-mouse-button-release-mode)
    (unless pointer-grabbed-p
      (xungrab-pointer))))




(let ((color-hash (make-hash-table :test 'equal)))
  (defun get-color (color)
    (multiple-value-bind (val foundp)
	(gethash color color-hash)
      (if foundp
	  val
	  (setf (gethash color color-hash)
		(xlib:alloc-color (xlib:screen-default-colormap *screen*) color))))))



(defgeneric ->color (color))

(defmethod ->color ((color-name string))
  color-name)

(defmethod ->color ((color integer))
  (labels ((hex->float (color)
	     (/ (logand color #xFF) 256.0)))
    (xlib:make-color :blue (hex->float color)
		     :green (hex->float (ash color -8))
		     :red (hex->float  (ash color -16)))))

(defmethod ->color ((color list))
  (destructuring-bind (red green blue) color
    (xlib:make-color :blue red :green green :red blue)))

(defmethod ->color ((color xlib:color))
  color)

(defmethod ->color (color)
  (format t "Wrong color type: ~A~%" color)
  "White")


(defun color->rgb (color)
  (multiple-value-bind (r g b)
      (xlib:color-rgb color)
    (+ (ash (round (* 256 r)) +16)
       (ash (round (* 256 g)) +8)
       (round (* 256 b)))))





(defmacro my-character->keysyms (ch)
  "Convert a char to a keysym"
  ;; XLIB:CHARACTER->KEYSYMS should probably be implemented in NEW-CLX
  ;; some day.  Or just copied from MIT-CLX or some other CLX
  ;; implementation (see translate.lisp and keysyms.lisp).  For now,
  ;; we do like this.  It suffices for modifiers and ASCII symbols.
  (if (fboundp 'xlib:character->keysyms)
      `(xlib:character->keysyms ,ch)
      `(list
       (case ,ch
	 (:character-set-switch #xFF7E)
	 (:left-shift #xFFE1)
	 (:right-shift #xFFE2)
	 (:left-control #xFFE3)
	 (:right-control #xFFE4)
	 (:caps-lock #xFFE5)
	 (:shift-lock #xFFE6)
	 (:left-meta #xFFE7)
	 (:right-meta #xFFE8)
	 (:left-alt #xFFE9)
	 (:right-alt #xFFEA)
	 (:left-super #xFFEB)
	 (:right-super #xFFEC)
	 (:left-hyper #xFFED)
	 (:right-hyper #xFFEE)
	 (t
	  (etypecase ,ch
	    (character
	     ;; Latin-1 characters have their own value as keysym
	     (if (< 31 (char-code ,ch) 256)
		 (char-code ,ch)
		 (error "Don't know how to get keysym from ~A" ,ch)))))))))




(defun char->keycode (char)
  "Convert a character to a keycode"
  (xlib:keysym->keycodes *display* (first (my-character->keysyms char))))


(defun keycode->char (code state)
  (xlib:keysym->character *display* (xlib:keycode->keysym *display* code 0) state))

(defun modifiers->state (modifier-list)
  (apply #'xlib:make-state-mask modifier-list))

(defun state->modifiers (state)
  (xlib:make-state-keys state))

(defun keycode->keysym (code modifiers)
  (xlib:keycode->keysym *display* code (cond ((member :shift modifiers) 1)
					     ((member :mod-5 modifiers) 4)
					     (t 0))))


(defmacro with-grab-keyboard-and-pointer ((cursor mask old-cursor old-mask) &body body)
  `(let ((pointer-grabbed (xgrab-pointer-p))
	 (keyboard-grabbed (xgrab-keyboard-p)))
     (xgrab-pointer *root* ,cursor ,mask)
     (unless keyboard-grabbed
       (xgrab-keyboard *root*))
     (unwind-protect
	  (progn
	    ,@body)
       (if pointer-grabbed
	   (xgrab-pointer *root* ,old-cursor ,old-mask)
	   (xungrab-pointer))
       (unless keyboard-grabbed
	 (xungrab-keyboard)))))






(let ((modifier-list nil))
  (defun init-modifier-list ()
    (dolist (name '("Shift_L" "Shift_R" "Control_L" "Control_R"
		    "Alt_L" "Alt_R" "Meta_L" "Meta_R" "Hyper_L" "Hyper_R"
		    "Mode_switch" "script_switch" "ISO_Level3_Shift"
		    "Caps_Lock" "Scroll_Lock" "Num_Lock"))
      (awhen (xlib:keysym->keycodes *display* (keysym-name->keysym name))
	(push it modifier-list))))

  (defun modifier-p (code)
    (member code modifier-list)))

(defun wait-no-key-or-button-press ()
  (with-grab-keyboard-and-pointer (66 67 66 67)
    (loop
       (let ((key (loop for k across (xlib:query-keymap *display*)
		     for code from 0
		     when (and (plusp k) (not (modifier-p code)))
		     return t))
	     (button (loop for b in (xlib:make-state-keys (nth-value 4 (xlib:query-pointer *root*)))
			when (member b '(:button-1 :button-2 :button-3 :button-4 :button-5))
			return t)))
	 (when (and (not key) (not button))
	   (loop while (xlib:event-case (*display* :discard-p t :peek-p nil :timeout 0)
			 (:motion-notify () t)
			 (:key-press () t)
			 (:key-release () t)
			 (:button-press () t)
			 (:button-release () t)
			 (t nil)))
	   (return))))))


(defun wait-a-key-or-button-press ()
  (with-grab-keyboard-and-pointer (24 25 66 67)
    (loop
     (let ((key (loop for k across (xlib:query-keymap *display*)
		      unless (zerop k) return t))
	   (button (loop for b in (xlib:make-state-keys (nth-value 4 (xlib:query-pointer *root*)))
			 when (member b '(:button-1 :button-2 :button-3 :button-4 :button-5))
			 return t)))
       (when (or key button)
	 (return))))))



(defun compress-motion-notify ()
  (when *have-to-compress-notify*
    (loop while (xlib:event-cond (*display* :timeout 0)
		  (:motion-notify () t)))))


(defun display-all-cursors (&optional (display-time 1))
  "Display all X11 cursors for display-time seconds"
  (loop for i from 0 to 152 by 2
     do (xgrab-pointer *root* i (1+ i))
       (dbg i)
       (sleep display-time)
       (xungrab-pointer)))




;;; Double buffering tools
(defun clear-pixmap-buffer (window gc)
  (rotatef (xlib:gcontext-foreground gc) (xlib:gcontext-background gc))
  (xlib:draw-rectangle *pixmap-buffer* gc
		       0 0 (xlib:drawable-width window) (xlib:drawable-height window)
		       t)
  (rotatef (xlib:gcontext-foreground gc) (xlib:gcontext-background gc)))

(defun copy-pixmap-buffer (window gc)
  (xlib:copy-area *pixmap-buffer* gc
		  0 0 (xlib:drawable-width window) (xlib:drawable-height window)
		  window 0 0))


(defun is-a-key-pressed-p ()
  (loop for k across (xlib:query-keymap *display*)
     when (plusp k)
     return t))

;;; Windows wm class and name tests
(defmacro defun-equal-wm-class (symbol class)
  `(defun ,symbol (window)
     (when (xlib:window-p window)
       (string-equal (xlib:get-wm-class window) ,class))))

(defmacro defun-equal-wm-name (symbol name)
  `(defun ,symbol (window)
     (when (xlib:window-p window)
       (string-equal (xlib:wm-name window) ,name))))
