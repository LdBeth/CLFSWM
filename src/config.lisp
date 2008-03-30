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
  "Return the size of root child (values rx ry rw rh raise-p)
You can tweak this to what you want"
  (values -1 -1 (xlib:screen-width *screen*) (xlib:screen-height *screen*) nil))
;; (values -1 -1 1024 768))
;;  (values 100 100 800 600))


;;; CONFIG
(defparameter *create-frame-on-root* nil
  "Set this variable to true if you want to allow to create a new frame
on root window in the main mode")


;;; CONFIG: Main mode colors
(defparameter *color-selected* "Red")
(defparameter *color-unselected* "Blue")
(defparameter *color-maybe-selected* "Yellow")

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
