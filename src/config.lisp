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


;;; CONFIG - Default modifiers
(defconfig *default-modifiers* '() nil
           "Default modifiers list to append to explicit modifiers
Example: :mod-2 for num_lock, :lock for Caps_lock...")


(defun-equal-wm-class equal-wm-class-rox-pinboard "ROX-Pinboard")
(defun-equal-wm-class equal-wm-class-xvkbd "xvkbd")

;;; CONFIG - Never managed window list
(defconfig *never-managed-window-list*
    (list (list 'equal-wm-class-rox-pinboard nil)
          (list 'equal-wm-class-xvkbd 'raise-window)
          (list 'equal-clfswm-terminal 'raise-and-focus-window))
  nil "CLFSWM will never manage windows of this type.
A list of (list match-function handle-function)")



(defconfig *hide-unmanaged-window* t nil
           "Hide or not unmanaged windows when a child is deselected.")

(defconfig *snap-size* 0.02 nil
           "Snap size when move or resize frame is constrained")


;;; CONFIG - Screen size
(defun get-fullscreen-size ()
  "Return the size of root child (values rx ry rw rh)
You can tweak this to what you want"
  (values (- *border-size*) (- *border-size*)
          (xlib:screen-width *screen*)
          (xlib:screen-height *screen*)))
  ;;(values -1 -1 (xlib:screen-width *screen*) (xlib:screen-height *screen*)))
;; (values -1 -1 1024 768))
;;  (values 100 100 800 600))


(defconfig  *corner-size* 3 'Corner
            "The size of the corner square")

;;; CONFIG: Corner actions - See in clfswm-corner.lisp for
;;;   allowed functions
(defconfig *corner-main-mode-left-button*
    '((:top-left open-menu)
      (:top-right present-virtual-keyboard)
      (:bottom-right expose-windows-mode)
      (:bottom-left nil))
  'Corner "Actions on corners in the main mode with the left mouse button")

(defconfig *corner-main-mode-middle-button*
    '((:top-left help-on-clfswm)
      (:top-right ask-close/kill-current-window)
      (:bottom-right nil)
      (:bottom-left nil))
  'Corner "Actions on corners in the main mode with the middle mouse button")

(defconfig *corner-main-mode-right-button*
    '((:top-left present-clfswm-terminal)
      (:top-right ask-close/kill-current-window)
      (:bottom-right expose-all-windows-mode)
      (:bottom-left nil))
  'Corner "Actions on corners in the main mode with the right mouse button")

(defconfig *corner-second-mode-left-button*
    '((:top-left nil)
      (:top-right nil)
      (:bottom-right expose-windows-mode)
      (:bottom-left nil))
  'Corner "Actions on corners in the second mode with the left mouse button")

(defconfig *corner-second-mode-middle-button*
    '((:top-left help-on-clfswm)
      (:top-right nil)
      (:bottom-right nil)
      (:bottom-left nil))
  'Corner "Actions on corners in the second mode with the middle mouse button")

(defconfig *corner-second-mode-right-button*
    '((:top-left nil)
      (:top-right nil)
      (:bottom-right expose-all-windows-mode)
      (:bottom-left nil))
  'Corner "Actions on corners in the second mode with the right mouse button")


(defconfig *virtual-keyboard-cmd* "xvkbd"
  'Corner "The command to display the virtual keybaord
  Here is an ~/.Xresources example for xvkbd:
    xvkbd.windowGeometry: 300x100-0-0
    xvkbd*Font: 6x12
    xvkbd.modalKeytop: true
    xvkbd.customization: -french
    xvkbd.keypad: false
  And make it always on top")

(defconfig *clfswm-terminal-name* "clfswm-terminal"
  'Corner "The clfswm terminal name")
;;(defparameter *clfswm-terminal-cmd* (format nil "xterm -T ~A -e /bin/bash --noprofile --norc" *clfswm-terminal-name*)
;;(defparameter *clfswm-terminal-cmd* (format nil "urxvt -name ~A" *clfswm-terminal-name*)
(defconfig *clfswm-terminal-cmd* (format nil "xterm -T ~A" *clfswm-terminal-name*)
  'Corner "The clfswm terminal command.
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

(defconfig *init-hook* '(default-init-hook display-hello-window)
  'Hook "Init hook. This hook is run just after the first root frame is created")

(defconfig *close-hook* '(close-notify-window close-clfswm-terminal close-virtual-keyboard)
  'Hook "Close hook. This hook is run just before closing the display")

(defconfig *default-nw-hook* 'default-frame-nw-hook
  'Hook "Default action to do on newly created windows")




;;; CONFIG
(defconfig *create-frame-on-root* nil
  nil "Create frame on root.
Set this variable to true if you want to allow to create a new frame
on the root window in the main mode with the mouse")


;;; CONFIG: Main mode colors
(defconfig *color-selected* "Red"
  'Main-mode "Color of selected window")
(defconfig *color-unselected* "Blue"
  'Main-mode "Color of unselected color")
(defconfig *color-maybe-selected* "Yellow"
  'Main-mode "Color of maybe selected windows")


;;; CONFIG: Frame colors
(defconfig *frame-background* "Black"
  'Frame-colors "Frame background")
(defconfig *frame-foreground* "Green"
  'Frame-colors "Frame foreground")
(defconfig *frame-foreground-root* "Red"
  'Frame-colors "Frame foreground when the frame is the root frame")
(defconfig *frame-foreground-hidden* "Darkgreen"
  'Frame-colors "Frame foreground for hidden windows")

;;; CONFIG: Default window size
(defconfig *default-window-width* 400
  nil "Default window width")
(defconfig *default-window-height* 300
  nil "Default window height")

;;; CONFIG: Second mode colors and fonts
(defconfig *sm-border-color* "Green"
  'Second-mode "Second mode window border color")
(defconfig *sm-background-color* "Black"
  'Second-mode "Second mode window background color")
(defconfig *sm-foreground-color* "Red"
  'Second-mode "Second mode window foreground color")
(defconfig *sm-font-string* *default-font-string*
  'Second-mode "Second mode window font string")
(defconfig *sm-width* 300
  'Second-mode "Second mode window width")
(defconfig *sm-height* 25
  'Second-mode "Second mode window height")





;;; CONFIG - Identify key colors
(defconfig *identify-font-string* *default-font-string*
  'Identify-key "Identify window font string")
(defconfig *identify-background* "black"
  'Identify-key "Identify window background color")
(defconfig *identify-foreground* "green"
  'Identify-key "Identify window foreground color")
(defconfig *identify-border* "red"
  'Identify-key "Identify window border color")

;;; CONFIG - Query string colors
(defconfig *query-font-string* *default-font-string*
  'Query-string "Query string window font string")
(defconfig *query-background* "black"
  'Query-string "Query string window background color")
(defconfig *query-message-color* "yellow"
  'Query-string "Query string window message color")
(defconfig *query-foreground* "green"
  'Query-string "Query string window foreground color")
(defconfig *query-cursor-color* "white"
  'Query-string "Query string window foreground cursor color")
(defconfig *query-parent-color* "blue"
  'Query-string "Query string window parenthesis color")
(defconfig *query-parent-error-color* "red"
  'Query-string "Query string window parenthesis color when no match")
(defconfig *query-border* "red"
  'Query-string "Query string window border color")


;;; CONFIG - Info mode
(defconfig *info-background* "black"
  'Info-mode "Info window background color")
(defconfig *info-foreground* "green"
  'Info-mode "Info window foreground color")
(defconfig *info-border* "red"
  'Info-mode "Info window border color")
(defconfig *info-line-cursor* "white"
  'Info-mode "Info window line cursor color color")
(defconfig *info-selected-background* "blue"
  'Info-mode "Info selected item background color")
(defconfig *info-font-string* *default-font-string*
  'Info-mode "Info window font string")

(defconfig *info-click-to-select* t
  'Info-mode "If true, click on info window select item. Otherwise, click to drag the menu")

;;; CONFIG - Circulate string colors
(defconfig *circulate-font-string* *default-font-string*
  'Circulate-mode "Circulate string window font string")
(defconfig *circulate-background* "black"
  'Circulate-mode "Circulate string window background color")
(defconfig *circulate-foreground* "green"
  'Circulate-mode "Circulate string window foreground color")
(defconfig *circulate-border* "red"
  'Circulate-mode "Circulate string window border color")
(defconfig *circulate-width* 400
  'Circulate-mode "Circulate mode window width")
(defconfig *circulate-height* 15
  'Circulate-mode "Circulate mode window height")


(defconfig *circulate-text-limite* 30
  'Circulate-mode "Maximum text limite in the circulate window")


;;; CONFIG - Expose string colors
(defconfig *expose-font-string* *default-font-string*
  'Expose-mode "Expose string window font string")
(defconfig *expose-background* "black"
  'Expose-mode "Expose string window background color")
(defconfig *expose-foreground* "green"
  'Expose-mode "Expose string window foreground color")
(defconfig *expose-border* "red"
  'Expose-mode "Expose string window border color")
(defconfig *expose-valid-on-key* t
  'Expose-mode "Valid expose mode when an accel key is pressed")
(defconfig *expose-show-window-title* t
  'Expose-mode "Show the window title on accel window")



;;; CONFIG - Show key binding colors
(defconfig *info-color-title* "Magenta"
  'Info-mode "Colored info title color")
(defconfig *info-color-underline* "Yellow"
  'Info-mode "Colored info underline color")
(defconfig *info-color-first* "Cyan"
  'Info-mode "Colored info first color")
(defconfig *info-color-second* "lightblue"
  'Info-mode "Colored info second color")


;;; CONFIG - Menu colors
;;; Set *info-foreground* to change the default menu foreground
(defconfig *menu-color-submenu* "Cyan"
  'Menu "Submenu color in menu")
(defconfig *menu-color-comment* "Yellow"
  'Menu "Comment color in menu")
(defconfig *menu-color-key* "Magenta"
  'Menu "Key color in menu")
(defconfig *menu-color-menu-key* (->color #xFF9AFF)
  'Menu "Menu key color in menu")


;;; CONFIG - Notify window string colors
(defconfig *notify-window-font-string* *default-font-string*
  'Notify-Window "Notify window font string")
(defconfig *notify-window-background* "black"
  'Notify-Window "Notify Window background color")
(defconfig *notify-window-foreground* "green"
  'Notify-Window "Notify Window foreground color")
(defconfig *notify-window-border* "red"
  'Notify-Window "Notify Window border color")
(defconfig *notify-window-delay* 10
  'Notify-Window "Notify Window display delay")


