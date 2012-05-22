;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Toolbar
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
;;; Documentation: If you want to use this file, just add this line in
;;; your configuration file:
;;;
;;;   (load-contrib "toolbar.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading Toolbar code... ")

(defstruct toolbar root-x root-y direction size thickness placement autohide modules font window gc)

(defparameter *toolbar-list* nil)

;;; CONFIG - Toolbar window string colors
(defconfig *toolbar-window-font-string* *default-font-string*
  'Toolbar-Window "Toolbar window font string")
(defconfig *toolbar-window-background* "black"
  'Toolbar-Window "Toolbar Window background color")
(defconfig *toolbar-window-foreground* "green"
  'Toolbar-Window "Toolbar Window foreground color")
(defconfig *toolbar-window-border* "grey70"
  'Toolbar-Window "Toolbar Window border color")
(defconfig *toolbar-window-transparency* *default-transparency*
  'Toolbar-window "Toolbar window background transparency")
(defconfig *toolbar-default-thickness* 10
  'toolbar-window "Toolbar default thickness")

(defconfig *toolbar-window-placement* 'top-left-placement
  'Placement "Toolbar window placement")


(let ((windows-list nil))
  (defun is-toolbar-window-p (win)
    (and (xlib:window-p win) (member win windows-list :test 'xlib:window-equal)))

  ;;    (defun refresh-toolbar-window ()
  ;;      (add-timer 0.1 #'refresh-toolbar-window :refresh-toolbar-window)
  ;;      (raise-window window)
  ;;      (let ((text-height (- (xlib:font-ascent font) (xlib:font-descent font))))
  ;;	(loop for tx in text
  ;;	   for i from 1 do
  ;;	     (setf (xlib:gcontext-foreground gc) (text-color tx))
  ;;	     (xlib:draw-glyphs window gc
  ;;			       (truncate (/ (- width (* (xlib:max-char-width font) (length (text-string tx)))) 2))
  ;;			       (* text-height i 2)
  ;;			       (text-string tx)))))
  ;;
  ;;    (defun close-toolbar-window ()
  ;;      (erase-timer :refresh-toolbar-window)
  ;;      (setf *never-managed-window-list*
  ;;	    (remove (list #'is-toolbar-window-p 'raise-window)
  ;;		    *never-managed-window-list* :test #'equal))
  ;;      (when gc
  ;;	(xlib:free-gcontext gc))
  ;;      (when window
  ;;	(xlib:destroy-window window))
  ;;      (when font
  ;;	(xlib:close-font font))
  ;;      (xlib:display-finish-output *display*)
  ;;      (setf window nil
  ;;	    gc nil
  ;;	    font nil))

  (defun open-toolbar (toolbar)
    (dbg toolbar)
    (let ((root (find-root-by-coordinates (toolbar-root-x toolbar) (toolbar-root-y toolbar))))
      (when (root-p root)
        (let ((*get-current-root-fun* (lambda () root)))
          (setf (toolbar-font toolbar) (xlib:open-font *display* *toolbar-window-font-string*))
          (let* ((width (if (equal (toolbar-direction toolbar) :horiz)
                            (round (/ (* (root-w root) (toolbar-size toolbar)) 100))
                            (toolbar-thickness toolbar)))
                 (height (if (equal (toolbar-direction toolbar) :horiz)
                             (toolbar-thickness toolbar)
                             (round (/ (* (root-h root) (toolbar-size toolbar)) 100)))))
            (dbg width height)
            (with-placement ((toolbar-placement toolbar) x y width height)
              (dbg x y width height)
              (setf (toolbar-window toolbar) (xlib:create-window :parent *root*
                                                                 :x x
                                                                 :y y
                                                                 :width width
                                                                 :height height
                                                                 :background (get-color *toolbar-window-background*)
                                                                 :border-width *border-size*
                                                                 :border (get-color *toolbar-window-border*)
                                                                 :colormap (xlib:screen-default-colormap *screen*)
                                                                 :event-mask '(:exposure :key-press))
                    (toolbar-gc toolbar) (xlib:create-gcontext :drawable (toolbar-window toolbar)
                                                               :foreground (get-color *toolbar-window-foreground*)
                                                               :background (get-color *toolbar-window-background*)
                                                               :font (toolbar-font toolbar)
                                                               :line-style :solid))
              (push (toolbar-window toolbar) windows-list)
              (setf (window-transparency (toolbar-window toolbar)) *toolbar-window-transparency*)
              (push (list #'is-toolbar-window-p 'raise-window) *never-managed-window-list*)
              (map-window (toolbar-window toolbar))
              (raise-window (toolbar-window toolbar))
              ;;(refresh-toolbar-window)
              (xlib:display-finish-output *display*))))))))


;;(defun open-toolbar (toolbar)
;;  ;;(open-toolbar-window '("toto plop")))
;;  (dbg toolbar)
;;  )

(defun open-all-toolbars ()
  "Open all toolbars"
  (dolist (toolbar *toolbar-list*)
    (open-toolbar toolbar)))

(defun add-toolbar (root-x root-y direction size placement autohide &rest modules)
  "Add a new toolbar.
     root-x, root-y: root coordinates
     direction: one of :horiz or :vert
     size: toolbar size in percent of root size"
  (let ((toolbar (make-toolbar :root-x root-x :root-y root-y
                      :direction direction :size size
                      :thickness *toolbar-default-thickness*
                      :placement placement
                      :autohide autohide
                      :modules modules)))
    (push toolbar *toolbar-list*)
    toolbar))


(add-hook *init-hook* 'open-all-toolbars)


(format t "done~%")
