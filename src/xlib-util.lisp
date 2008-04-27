;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Utility functions
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2005 Philippe Brochard <hocwp@free.fr>
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
       (progn
	 ,@body)
     ((or xlib:match-error xlib:window-error xlib:drawable-error) (c)
       (declare (ignore c)))))
       ;;(dbg c ',body))))



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
  (xlib:warp-pointer *root*
		     (1- (xlib:screen-width *screen*))
		     (1- (xlib:screen-height *screen*))))





(defun window-state (win)
  "Get the state (iconic, normal, withdraw of a window."
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

  

(defun unhide-window (window)
  (when window
    (with-xlib-protect
      (when (window-hidden-p window)
	(xlib:map-window window)
	(setf (window-state window) +normal-state+
	      (xlib:window-event-mask window) *window-events*)))))














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
;;
;;
;;
;;(defun hide-window (window)
;;  (when window
;;    (with-xlib-protect
;;      (let ((net-wm-state (net-wm-state window)))
;;	(dbg net-wm-state)
;;	(pushnew :_net_wm_state_hidden net-wm-state)
;;	(setf (net-wm-state window) net-wm-state)
;;	(dbg (net-wm-state window)))
;;      (setf (window-state window) +iconic-state+
;;	    (xlib:window-event-mask window) (remove :structure-notify *window-events*))
;;      (xlib:unmap-window window)
;;      (setf (xlib:window-event-mask window) *window-events*))))


(defun hide-window (window)
  (when window
    (with-xlib-protect
      (setf (window-state window) +iconic-state+
	    (xlib:window-event-mask window) (remove :structure-notify *window-events*))
      (xlib:unmap-window window)
      (setf (xlib:window-event-mask window) *window-events*))))



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





;; Stolen from Eclipse
(defun send-configuration-notify (window)
  "Send a synthetic configure notify event to the given window (ICCCM 4.1.5)"
  (multiple-value-bind (x y)
      (xlib:translate-coordinates window 0 0 (xlib:drawable-root window))
    (xlib:send-event window
		     :configure-notify
		     (xlib:make-event-mask :structure-notify)
		     :event-window window :window window
		     :x x :y y
		     :override-redirect-p nil
		     :border-width (xlib:drawable-border-width window)
		     :width (xlib:drawable-width window)
		     :height (xlib:drawable-height window)
		     :propagate-p nil)))


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
  (when window
    (with-xlib-protect
      (when (window-hidden-p window)
	(unhide-window window))
      (setf (xlib:window-priority window) :top-if))))

(defun focus-window (window)
  "Give the window focus."
  (when window
    (with-xlib-protect
      (raise-window window)
      (xlib:set-input-focus *display* window :parent))))







(defun no-focus ()
  "don't focus any window but still read keyboard events."
  (xlib:set-input-focus *display* *no-focus-window* :pointer-root))
  



(let ((cursor-font nil)
      (cursor nil)
      (pointer-grabbed nil))
  (labels ((free-grab-pointer ()
	     (when cursor
	       (xlib:free-cursor cursor)
	       (setf cursor nil))
	     (when cursor-font
	       (xlib:close-font cursor-font)
	       (setf cursor-font nil))))
    (defun xgrab-init-pointer ()
      (setf pointer-grabbed nil))

    (defun xgrab-pointer-p ()
      pointer-grabbed)
    
    (defun xgrab-pointer (root cursor-char cursor-mask-char
			  &optional (pointer-mask '(:enter-window :pointer-motion
						    :button-press :button-release)) owner-p)
      "Grab the pointer and set the pointer shape."
      (free-grab-pointer)
      (setf pointer-grabbed t)
      (let* ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
	     (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)))
	(setf cursor-font (xlib:open-font *display* "cursor")
	      cursor (xlib:create-glyph-cursor :source-font cursor-font
					       :source-char cursor-char
					       :mask-font cursor-font
					       :mask-char cursor-mask-char
					       :foreground black
					       :background white))
	(xlib:grab-pointer root pointer-mask
			   :owner-p owner-p  :sync-keyboard-p nil :sync-pointer-p nil :cursor cursor)))

    (defun xungrab-pointer ()
      "Remove the grab on the cursor and restore the cursor shape."
      (setf pointer-grabbed nil)
      (xlib:ungrab-pointer *display*)
      (free-grab-pointer))))


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

;;(defun grab-all-keys (window)
;;  (ungrab-all-keys window)
;;  (dolist (modifiers '(:control :mod-1 :shift))
;;    (xlib:grab-key window :any
;;		   :modifiers (list modifiers)
;;		   :owner-p nil
;;		   :sync-pointer-p nil
;;		   :sync-keyboard-p t)))

;;(defun grab-all-keys (window)
;;  (ungrab-all-keys window)
;;  (xlib:grab-key window :any
;;		 :modifiers :any
;;		 :owner-p nil
;;		 :sync-pointer-p nil
;;		 :sync-keyboard-p t))




;;(defun stop-keyboard-event ()
;;  (xlib:allow-events *display* :sync-keyboard))
;;
;;(defun replay-keyboard-event ()
;;  (xlib:allow-events *display* :replay-keyboard))


(defun stop-button-event ()
  (xlib:allow-events *display* :sync-pointer))

(defun replay-button-event ()
  (xlib:allow-events *display* :replay-pointer))





(defun get-color (color)
  (xlib:alloc-color (xlib:screen-default-colormap *screen*) color))




(defun my-character->keysyms (ch)
  "Convert a char to a keysym"
  ;; XLIB:CHARACTER->KEYSYMS should probably be implemented in NEW-CLX
  ;; some day.  Or just copied from MIT-CLX or some other CLX
  ;; implementation (see translate.lisp and keysyms.lisp).  For now,
  ;; we do like this.  It suffices for modifiers and ASCII symbols.
  (if (fboundp 'xlib:character->keysyms)
      (xlib:character->keysyms ch)
      (list
       (case ch
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
	  (etypecase ch
	    (character
	     ;; Latin-1 characters have their own value as keysym
	     (if (< 31 (char-code ch) 256)
		 (char-code ch)
		 (error "Don't know how to get keysym from ~A" ch)))))))))



(defun char->keycode (char)
  "Convert a character to a keycode"
  (xlib:keysym->keycodes *display* (first (my-character->keysyms char))))


(defun keycode->char (code state)
  (xlib:keysym->character *display* (xlib:keycode->keysym *display* code 0) state))

(defun modifiers->state (modifier-list)
  (apply #'xlib:make-state-mask modifier-list))

(defun state->modifiers (state)
  (xlib:make-state-keys state))




(defun wait-no-key-or-button-press ()
  (loop
     (let ((key (loop for k across (xlib:query-keymap *display*)
		   unless (zerop k) return t))
	   (button (plusp (nth-value 4 (xlib:query-pointer *root*)))))
       (when (and (not key) (not button))
	 (loop while (xlib:event-case (*display* :discard-p t :peek-p nil :timeout 0)
		       (:motion-notify () t)
		       (:key-press () t)
		       (:button-press () t)
		       (:button-release () t)
		       (t nil)))
	 (return-from wait-no-key-or-button-press nil)))))



(defun compress-motion-notify ()
  (when *have-to-compress-notify*
    (xlib:event-case (*display* :discard-p nil :peek-p t :timeout 0)
      (:motion-notify () t))))

