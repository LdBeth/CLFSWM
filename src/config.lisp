;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Configuration file
;;;
;;; Change this file to your own needs or update some of this variables in
;;; your ~/.clfswmrc
;;; Some simple hack can be done in the code begining with the word CONFIG
;;; (you can do a 'grep CONFIG *.lisp' to see what you can configure)
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


;;; CONFIG - Compress motion notify ?
;; This variable may be useful to speed up some slow version of CLX.
;; It is particulary useful with CLISP/MIT-CLX.
(setf *have-to-compress-notify* t)



;;; CONFIG - Default modifiers
(defparameter *default-modifiers* '()
  "Config(): Default modifiers list to append to explicit modifiers
Example: :mod-2 for num_lock, :lock for Caps_lock...")




;;; CONFIG - Never managed window list
(defparameter *never-managed-window-list*
  '((xlib:get-wm-class "ROX-Pinboard")
    (xlib:get-wm-class "xvkbd")
    (xlib:wm-name "clfswm-terminal"))
  "Config(): CLFSWM will never manage windows of this type.
A list of (predicate-function-on-window expected-string)")


;;; CONFIG - Screen size
(defun get-fullscreen-size ()
  "Return the size of root child (values rx ry rw rh)
You can tweak this to what you want"
  (values -2 -2 (+ (xlib:screen-width *screen*) 2) (+ (xlib:screen-height *screen*) 2)))
  ;;(values -1 -1 (xlib:screen-width *screen*) (xlib:screen-height *screen*)))
;; (values -1 -1 1024 768))
;;  (values 100 100 800 600))


(defparameter  *corner-size* 3
  "Config(Corner group): The size of the corner square")


;;; CONFIG: Corner actions - See in clfswm-corner.lisp for
;;;   allowed functions
(defparameter *corner-main-mode-left-button*
  '((:top-left open-menu)
    (:top-right present-virtual-keyboard)
    (:bottom-right present-windows)
    (:bottom-left nil))
  "Config(Corner group): Actions on corners in the main mode with the left mouse button")

(defparameter *corner-main-mode-middle-button*
  '((:top-left help-on-clfswm)
    (:top-right ask-close/kill-current-window)
    (:bottom-right nil)
    (:bottom-left nil))
  "Config(Corner group): Actions on corners in the main mode with the middle mouse button")

(defparameter *corner-main-mode-right-button*
  '((:top-left present-clfswm-terminal)
    (:top-right ask-close/kill-current-window)
    (:bottom-right present-all-windows)
    (:bottom-left nil))
  "Config(Corner group): Actions on corners in the main mode with the right mouse button")

(defparameter *corner-second-mode-left-button*
  '((:top-left nil)
    (:top-right nil)
    (:bottom-right present-windows)
    (:bottom-left nil))
  "Config(Corner group): Actions on corners in the second mode with the left mouse button")

(defparameter *corner-second-mode-middle-button*
  '((:top-left help-on-clfswm)
    (:top-right nil)
    (:bottom-right nil)
    (:bottom-left nil))
  "Config(Corner group): Actions on corners in the second mode with the middle mouse button")

(defparameter *corner-second-mode-right-button*
  '((:top-left nil)
    (:top-right nil)
    (:bottom-right present-all-windows)
    (:bottom-left nil))
  "Config(Corner group): Actions on corners in the second mode with the right mouse button")


(defparameter *virtual-keyboard-cmd* "xvkbd"
  "Config(Corner group): The command to display the virtual keybaord
  Here is an ~/.Xresources example for xvkbd:
    xvkbd.windowGeometry: 300x100-0-0
    xvkbd*Font: 6x12
    xvkbd.modalKeytop: true
    xvkbd.customization: -french
    xvkbd.keypad: false
  And make it always on top")
(defparameter *virtual-keyboard-kill-cmd* "pkill xvkbd"
  "Config(Corner group): The command to stop the virtual keyboard")

(defparameter *clfswm-terminal-name* "clfswm-terminal"
  "Config(Corner group): The clfswm terminal name")
(defparameter *clfswm-terminal-cmd* (format nil "xterm -T ~A" *clfswm-terminal-name*)
  "Config(Corner group): The clfswm terminal command.
This command must set the window title to *clfswm-terminal-name*")




;;; Hook definitions
;;;
;;; A hook is a function, a symbol or a list of functions with a rest
;;; arguments.
;;;
;;; This hooks are set in clfswm.lisp, you can overwrite them or extend
;;; them with a hook list.
;;;
;;; See clfswm.lisp for hooks examples.

(defparameter *init-hook* 'default-init-hook
  "Config(Hook group): Init hook. This hook is run just after the first root frame is created")

(defparameter *default-nw-hook* 'default-frame-nw-hook
  "Config(Hook group): Default action to do on newly created windows")




;;; CONFIG
(defparameter *create-frame-on-root* nil
  "Config(): Create frame on root.
Set this variable to true if you want to allow to create a new frame
on the root window in the main mode with the mouse")


;;; CONFIG: Main mode colors
(defparameter *color-selected* "Red"
  "Config(Main mode group): Color of selected window")
(defparameter *color-unselected* "Blue"
  "Config(Main mode group): Color of unselected color")
(defparameter *color-maybe-selected* "Yellow"
  "Config(Main mode group): Color of maybe selected windows")


;;; CONFIG: Frame colors
(defparameter *frame-background* "Black"
  "Config(Frame colors group): Frame background")
(defparameter *frame-foreground* "Green"
  "Config(Frame colors group): Frame foreground")
(defparameter *frame-foreground-root* "Red"
  "Config(Frame colors group): Frame foreground when the frame is the root frame")
(defparameter *frame-foreground-hidden* "Darkgreen"
  "Config(Frame colors group): Frame foreground for hidden windows")

;;; CONFIG: Default window size
(defparameter *default-window-width* 400
  "Config(): Default window width")
(defparameter *default-window-height* 300
  "Config(): Default window height")

;;; CONFIG: Second mode colors and fonts
(defparameter *sm-border-color* "Green"
  "Config(Second mode group): Second mode window border color")
(defparameter *sm-background-color* "Black"
  "Config(Second mode group): Second mode window background color")
(defparameter *sm-foreground-color* "Red"
  "Config(Second mode group): Second mode window foreground color")
(defparameter *sm-font-string* *default-font-string*
  "Config(Second mode group): Second mode window font string")
(defparameter *sm-width* 300
  "Config(Second mode group): Second mode window width")
(defparameter *sm-height* 25
  "Config(Second mode group): Second mode window height")





;;; CONFIG - Identify key colors
(defparameter *identify-font-string* *default-font-string*
  "Config(Identify key group): Identify window font string")
(defparameter *identify-background* "black"
  "Config(Identify key group): Identify window background color")
(defparameter *identify-foreground* "green"
  "Config(Identify key group): Identify window foreground color")
(defparameter *identify-border* "red"
  "Config(Identify key group): Identify window border color")

;;; CONFIG - Query string colors
(defparameter *query-font-string* *default-font-string*
  "Config(Query string group): Query string window font string")
(defparameter *query-background* "black"
  "Config(Query string group): Query string window background color")
(defparameter *query-foreground* "green"
  "Config(Query string group): Query string window foreground color")
(defparameter *query-border* "red"
  "Config(Query string group): Query string window border color")


;;; CONFIG - Info mode
(defparameter *info-background* "black"
  "Config(Info mode group): Info window background color")
(defparameter *info-foreground* "green"
  "Config(Info mode group): Info window foreground color")
(defparameter *info-border* "red"
  "Config(Info mode group): Info window border color")
(defparameter *info-line-cursor* "white"
  "Config(Info mode group): Info window line cursor color color")
(defparameter *info-selected-background* "blue"
  "Config(Info mode group): Info selected item background color")
(defparameter *info-font-string* *default-font-string*
  "Config(Info mode group): Info window font string")

(defparameter *info-click-to-select* t
  "Config(Info mode group): If true, click on info window select item. Otherwise, click to drag the menu")

;;; CONFIG - Circulate string colors
(defparameter *circulate-font-string* *default-font-string*
  "Config(Circulate mode group): Circulate string window font string")
(defparameter *circulate-background* "black"
  "Config(Circulate mode group): Circulate string window background color")
(defparameter *circulate-foreground* "green"
  "Config(Circulate mode group): Circulate string window foreground color")
(defparameter *circulate-border* "red"
  "Config(Circulate mode group): Circulate string window border color")
(defparameter *circulate-width* 400
  "Config(Circulate mode group): Circulate mode window width")
(defparameter *circulate-height* 15
  "Config(Circulate mode group): Circulate mode window height")


(defparameter *circulate-text-limite* 30
  "Config(Circulate mode group): Maximum text limite in the circulate window")



;;; CONFIG - Show key binding colors
(defparameter *info-color-title* "Magenta"
  "Config(Info mode group): Colored info title color")
(defparameter *info-color-underline* "Yellow"
  "Config(Info mode group): Colored info underline color")
(defparameter *info-color-first* "Cyan"
  "Config(Info mode group): Colored info first color")
(defparameter *info-color-second* "lightblue"
  "Config(Info mode group): Colored info second color")


;;; CONFIG - Menu colors
;;; Set *info-foreground* to change the default menu foreground
(defparameter *menu-color-submenu* "Cyan"
  "Config(Menu group): Submenu color in menu")
(defparameter *menu-color-comment* "Yellow"
  "Config(Menu group): Comment color in menu")
(defparameter *menu-color-key* "Magenta"
  "Config(Menu group): Key color in menu")
(defparameter *menu-color-menu-key* (->color #xFF9AFF)
  "Config(Menu group): Menu key color in menu")

