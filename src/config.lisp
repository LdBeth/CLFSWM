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
(defparameter *have-to-compress-notify* nil
  "This variable may be useful to speed up some slow version of CLX.
It is particulary useful with CLISP/MIT-CLX.")
  


;;; CONFIG - Screen size
(defun get-fullscreen-size ()
  "Return the size of root child (values rx ry rw rh)
You can tweak this to what you want"
  (values -2 -2 (+ (xlib:screen-width *screen*) 2) (+ (xlib:screen-height *screen*) 2)))
  ;;(values -1 -1 (xlib:screen-width *screen*) (xlib:screen-height *screen*)))
;; (values -1 -1 1024 768))
;;  (values 100 100 800 600))


(defparameter  *corner-size* 3
  "The size of the corner square")




;;; Hook definitions
;;;
;;; A hook is a function, a symbol or a list of functions with a rest
;;; arguments.
;;;
;;; This hooks are set in clfswm.lisp, you can overwrite them or extend
;;; them with a hook list.
;;;
;;; See clfswm.lisp for hooks examples.

(defun default-init-hook ()
  (let ((frame (add-frame (create-frame :name "Default"
                                        :layout nil :x 0.05 :y 0.05
                                        :w 0.9 :h 0.9) *root-frame*)))
    (setf *current-child* frame)))

(defparameter *init-hook* 'default-init-hook
  "Init hook. This hook is run just after the first root frame is created")

(defparameter *default-nw-hook* 'default-frame-nw-hook
  "Default action to do on newly created windows")




;;; CONFIG
(defparameter *create-frame-on-root* nil
  "Set this variable to true if you want to allow to create a new frame
on the root window in the main mode with the mouse")


;;; CONFIG: Corner where to present windows (An expose like)
(defparameter *present-windows-corner* :bottom-right
  "Which corner enable the mouse present windows.
One of :bottom-right :bottom-left :top-right :top-left")

(defparameter *present-all-windows-corner* :bottom-left
  "Which corner enable the mouse present all windows
One of :bottom-right :bottom-left :top-right :top-left")





;;; CONFIG: Main mode colors
(defparameter *color-selected* "Red")
(defparameter *color-unselected* "Blue")
(defparameter *color-maybe-selected* "Yellow")

;;; CONFIG: Default window size
(defparameter *default-window-width* 400)
(defparameter *default-window-height* 300)

;;; CONFIG: Second mode colors and fonts
(defparameter *sm-border-color* "Green")
(defparameter *sm-background-color* "Black")
(defparameter *sm-foreground-color* "Red")
(defparameter *sm-font-string* "9x15bold")
(defparameter *sm-width* 300)
(defparameter *sm-height* 25)





;;; CONFIG - Identify key colors
(defparameter *identify-font-string* "9x15")
(defparameter *identify-background* "black")
(defparameter *identify-foreground* "green")
(defparameter *identify-border* "red")

;;; CONFIG - Query string colors
(defparameter *query-font-string* "9x15")
(defparameter *query-background* "black")
(defparameter *query-foreground* "green")
(defparameter *query-border* "red")


;;; CONFIG - Info mode

(defparameter *info-background* "black")
(defparameter *info-foreground* "green")
(defparameter *info-border* "red")
(defparameter *info-line-cursor* "white")
(defparameter *info-font-string* "9x15")



;;; Tiling to side parameters
(defparameter *tile-workspace-function* 'tile-workspace-top)
(defparameter *tile-border-size* 200)
