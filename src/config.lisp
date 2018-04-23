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
;;; (C) 2005-2015 Philippe Brochard <pbrochard@common-lisp.net>
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
(defconfig *default-modifiers* '()
  nil "Default modifiers list to append to explicit modifiers
Example: :mod-2 for num_lock, :lock for Caps_lock...")


;;; Standard menu entry based on the XDG specifications
(defconfig *xdg-section-list* (append
                               '(TextEditor FileManager WebBrowser)
                               '(AudioVideo Audio Video Development Education Game Graphics Network
                                 Office Settings System Utility)
                               '(TerminalEmulator Screensaver))
  'Menu "Standard menu sections")



(defun-equal-wm-class equal-wm-class-rox-pinboard "ROX-Pinboard")
(defun-equal-wm-class equal-wm-class-xvkbd "xvkbd")

;;; CONFIG - Never managed window list
(defconfig *never-managed-window-list*
    (list (list 'equal-wm-class-rox-pinboard nil)
          (list 'equal-wm-class-xvkbd 'raise-window)
          (list 'equal-clfswm-terminal 'raise-and-focus-window))
  nil "CLFSWM will never manage windows of this type.
A list of (list match-function handle-function)")


(defconfig *steal-focus* t
  nil "Allow to steal the focus on configure request")

(defconfig *hide-unmanaged-window* t
  nil "Hide or not unmanaged windows when a child is deselected.")

(defconfig *snap-size* 5
  nil "Snap size (in % of parent size) when move or resize frame is constrained")

(defconfig *spatial-move-delay-before* 0.2
  nil "Delay to display the current child before doing a spatial move")

(defconfig *spatial-move-delay-after* 0.5
  nil "Delay to display the new child after doing a spatial move")


(defconfig  *corner-size* 3
  'Corner "The size of the corner square")

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

(defconfig *corner-error-message-color* "red"
  'Corner "Error message color")
(defconfig *corner-error-message-delay* 5
  'Corner "Time to display the error message on commad error")
(defconfig *corner-command-try-delay* 0.2
  'Corner "Time to wait before checking window in query tree")
(defconfig *corner-command-try-number* 10
  'Corner "Number of try to wait the window in query tree")


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

(defconfig *query-key-press-hook* nil
  'Hook "Query hook. Hook called on each key press event in query loop")
(defconfig *query-button-press-hook* nil
  'Hook "Query hook. Hook called on each button press event in query loop")

;;; CONFIG: Root
(defconfig *create-frame-on-root* nil
  'Root "Create frame on root.
Set this variable to true if you want to allow to create a new frame
on the root window in the main mode with the mouse")
(defconfig *have-to-show-current-root* t
  'Root "Show the current root if true")
(defconfig *show-current-root-delay* 1
  'Root "Delay to show the current root")
(defconfig *show-current-root-placement* 'middle-middle-root-placement
  'Root "Current root notify window placement")
(defconfig *show-current-root-message* "Current root"
  'Root "Current root notify window message")


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
(defconfig *frame-transparency* 0.6
  'Frame-colors "Frame background transparency")

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
(defconfig *sm-font-string* "genera-cptfontbi"
  'Second-mode "Second mode window font string")
(defconfig *sm-width* 300
  'Second-mode "Second mode window width")
(defconfig *sm-height* 25
  'Second-mode "Second mode window height")
(defconfig *sm-transparency* *default-transparency*
  'Second-mode "Second mode background transparency")




;;; CONFIG - Identify key colors
(defconfig *identify-font-string* "genera-hl10"
  'Identify-key "Identify window font string")
(defconfig *identify-background* "black"
  'Identify-key "Identify window background color")
(defconfig *identify-foreground* "green"
  'Identify-key "Identify window foreground color")
(defconfig *identify-border* "red"
  'Identify-key "Identify window border color")
(defconfig *identify-transparency* *default-transparency*
  'Identify-key "Identify window background transparency")

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
(defconfig *query-transparency* *default-transparency*
  'Query-string "Query string window background transparency")
(defconfig *query-max-complet-length* 100
  'Query-string "Query maximum length of completion list")
(defconfig *query-min-complet-char* 2
  'Query-string "Query minimum input length for completion")


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
(defconfig *info-font-string* "genera-cptfontcc"
  'Info-mode "Info window font string")
(defconfig *info-transparency* *default-transparency*
  'Info-mode "Info window background transparency")

(defconfig *info-click-to-select* t
  'Info-mode "If true, click on info window select item. Otherwise, click to drag the menu")

;;; CONFIG - Circulate string colors
(defconfig *circulate-font-string* "genera-cptfonti"
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
(defconfig *circulate-transparency* *default-transparency*
  'Circulate-mode "Circulate window background transparency")


(defconfig *circulate-text-limite* 30
  'Circulate-mode "Maximum text limite in the circulate window")


;;; CONFIG - Expose string colors
(defconfig *expose-font-string* *default-font-string*
  'Expose-mode "Expose string window font string")
(defconfig *expose-background* "grey10"
  'Expose-mode "Expose string window background color")
(defconfig *expose-foreground* "grey50"
  'Expose-mode "Expose string window foreground color")
(defconfig *expose-foreground-letter* "red"
  'Expose-mode "Expose string window foreground color for letters")
(defconfig *expose-foreground-letter-nok* "grey30"
  'Expose-mode "Expose string window foreground color for letter not selected")
(defconfig *expose-background-letter-match* "green"
  'Expose-mode "Expose string window background color for matching letters")
(defconfig *expose-border* "grey20"
  'Expose-mode "Expose string window border color")
(defconfig *expose-valid-on-key* t
  'Expose-mode "Valid expose mode when an accel key is pressed")
(defconfig *expose-show-window-title* t
  'Expose-mode "Show the window title on accel window")
(defconfig *expose-transparency* 0.9
  'Expose-mode "Expose string window background transparency")
(defconfig *expose-direct-select* t
  'Expose-mode "Immediately select child if they can be directly accessed")


;;; CONFIG - Fastswitch string colors
(defconfig *fastswitch-font-string* "genera-13fgb"
  'Fastswitch-mode "Fastswitch string window font string")
(defconfig *fastswitch-background* "grey10"
  'Fastswitch-mode "Fastswitch string window background color")
(defconfig *fastswitch-foreground* "grey50"
  'Fastswitch-mode "Fastswitch string window foreground color")
(defconfig *fastswitch-foreground-letter* "red"
  'Fastswitch-mode "Fastswitch string window foreground color for letters")
(defconfig *fastswitch-foreground-letter-second* "magenta"
  'Fastswitch-mode "Fastswitch string window foreground color for letters")
(defconfig *fastswitch-foreground-letter-second-frame* "yellow"
  'Fastswitch-mode "Fastswitch string window foreground color for letters for frames")
(defconfig *fastswitch-foreground-childname* "grey70"
  'Fastswitch-mode "Fastswitch string window foreground color for childname")
(defconfig *fastswitch-border* "grey20"
  'Fastswitch-mode "Fastswitch string window border color")
(defconfig *fastswitch-transparency* 0.9
  'Fastswitch-mode "Fastswitch string window background transparency")
(defconfig *fastswitch-show-frame-p* t
  'Fastswitch-mode "Fastswitch show frame in mini window")
(defconfig *fastswitch-adjust-window-p* t
  'Fastswitch-mode "Fastswitch adjust window to show all children names")
(defconfig *fastswitch-display-mode* 'Tree
  'Fastswitch-mode "Fastswitch display mode (one of LINE or TREE)")



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
(defconfig *menu-key-bound-color* "gray50"
  'Menu "Key bound min menu color")


;;; CONFIG - Notify window string colors
(defconfig *notify-window-font-string* "genera-sail12"
  'Notify-Window "Notify window font string")
(defconfig *notify-window-background* "black"
  'Notify-Window "Notify Window background color")
(defconfig *notify-window-foreground* "green"
  'Notify-Window "Notify Window foreground color")
(defconfig *notify-window-border* "red"
  'Notify-Window "Notify Window border color")
(defconfig *notify-window-delay* 10
  'Notify-Window "Notify Window display delay")
(defconfig *notify-window-transparency* *default-transparency*
  'Notify-window "Notify window background transparency")
