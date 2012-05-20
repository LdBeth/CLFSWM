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

;;; CONFIG - Toolbar window string colors
(defconfig *toolbar-window-font-string* *default-font-string*
  'Toolbar-Window "Toolbar window font string")
(defconfig *toolbar-window-background* "black"
  'Toolbar-Window "Toolbar Window background color")
(defconfig *toolbar-window-foreground* "green"
  'Toolbar-Window "Toolbar Window foreground color")
(defconfig *toolbar-window-border* "red"
  'Toolbar-Window "Toolbar Window border color")
(defconfig *toolbar-window-delay* 10
  'Toolbar-Window "Toolbar Window display delay")
(defconfig *toolbar-window-transparency* *default-transparency*
  'Toolbar-window "Toolbar window background transparency")

(defconfig *toolbar-window-placement* 'top-left-placement
  'Placement "Toolbar window placement")


(let (font
      window
      gc
      width height
      text
      current-child)
  (labels ((text-string (tx)
	     (typecase tx
	       (cons (first tx))
	       (t tx)))
	   (text-color (tx)
	     (get-color (typecase tx
			  (cons (second tx))
			  (t *toolbar-window-foreground*)))))
    (defun is-toolbar-window-p (win)
      (when (and (xlib:window-p win) (xlib:window-p window))
	(xlib:window-equal win window)))

    (defun refresh-toolbar-window ()
      (add-timer 0.1 #'refresh-toolbar-window :refresh-toolbar-window)
      (raise-window window)
      (let ((text-height (- (xlib:font-ascent font) (xlib:font-descent font))))
	(loop for tx in text
	   for i from 1 do
	     (setf (xlib:gcontext-foreground gc) (text-color tx))
	     (xlib:draw-glyphs window gc
			       (truncate (/ (- width (* (xlib:max-char-width font) (length (text-string tx)))) 2))
			       (* text-height i 2)
			       (text-string tx)))))

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

    (defun open-toolbar-window (text-list)
;;      (close-toolbar-window)
      (setf font (xlib:open-font *display* *toolbar-window-font-string*))
      (let ((text-height (- (xlib:font-ascent font) (xlib:font-descent font))))
	(setf text text-list)
	(setf width (* (xlib:max-char-width font) (+ (loop for tx in text-list
							maximize (length (text-string tx))) 2))
	      height (+ (* text-height (length text-list) 2) text-height))
	(with-placement (*toolbar-window-placement* x y width height)
	  (setf window (xlib:create-window :parent *root*
					   :x x
					   :y y
					   :width width
					   :height height
					   :background (get-color *toolbar-window-background*)
					   :border-width *border-size*
					   :border (get-color *toolbar-window-border*)
					   :colormap (xlib:screen-default-colormap *screen*)
					   :event-mask '(:exposure :key-press))
		gc (xlib:create-gcontext :drawable window
					 :foreground (get-color *toolbar-window-foreground*)
					 :background (get-color *toolbar-window-background*)
					 :font font
					 :line-style :solid))
          (setf (window-transparency window) *toolbar-window-transparency*)
	  (when (frame-p (current-child))
	    (setf current-child (current-child)))
          (push (list #'is-toolbar-window-p 'raise-window) *never-managed-window-list*)
	  (map-window window)
	  (refresh-toolbar-window)
	  (xlib:display-finish-output *display*))))))


(defun open-toolbar ()
  "Open the toolbar mode"
  (open-toolbar-window '("toto plop")))


(add-hook *init-hook* 'open-toolbar)


(format t "done~%")
