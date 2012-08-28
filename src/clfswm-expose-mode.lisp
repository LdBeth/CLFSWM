;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Expose functions - An expose like.
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2012 Philippe Brochard <pbrochard@common-lisp.net>
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

(defparameter *expose-font* nil)
(defparameter *expose-windows-list* nil)
(defparameter *expose-selected-child* nil)

(defun leave-expose-mode ()
  "Leave the expose mode"
  (throw 'exit-expose-loop nil))

(defun valid-expose-mode ()
  "Valid the expose mode"
  (throw 'exit-expose-loop t))

(defun mouse-leave-expose-mode (window root-x root-y)
  "Leave the expose mode"
  (declare (ignore window root-x root-y))
  (throw 'exit-expose-loop nil))

(defun mouse-valid-expose-mode (window root-x root-y)
  "Valid the expose mode"
  (declare (ignore window root-x root-y))
  (throw 'exit-expose-loop t))


(defun expose-draw-letter ()
  (dolist (lwin *expose-windows-list*)
    (destructuring-bind (window gc string child letter) lwin
      (declare (ignore child))
      (clear-pixmap-buffer window gc)
      (xlib:with-gcontext (gc :foreground (get-color (if (substring-equal *query-string* letter)
                                                         *expose-foreground-letter*
                                                         *expose-foreground-letter-nok*))
                              :background (get-color (if (string-equal *query-string* letter)
                                                         *expose-background-letter-match*
                                                         *expose-background*)))
        (xlib:draw-image-glyphs *pixmap-buffer* gc
                                (xlib:max-char-width *expose-font*)
                                (+ (xlib:font-ascent *expose-font*) (xlib:font-descent *expose-font*))
                                letter))
      (xlib:draw-glyphs *pixmap-buffer* gc
                        (xlib:max-char-width *expose-font*)
                        (+ (* 2 (xlib:font-ascent *expose-font*)) (xlib:font-descent *expose-font*) 1)
                        string)
      (copy-pixmap-buffer window gc))))

(defun expose-create-window (child n)
  (with-current-child (child)
    (let* ((string (format nil "~A"
                           (if *expose-show-window-title*
                               (ensure-printable (child-fullname child))
                               "")))
           (width (if *expose-show-window-title*
                      (min (* (xlib:max-char-width *expose-font*) (+ (length string) 2))
                           (- (child-width child) 4))
                      (* (xlib:max-char-width *expose-font*) 3)))
           (height (* (xlib:font-ascent *expose-font*) 3)))
      (with-placement (*expose-mode-placement* x y width height)
        (let* ((window (xlib:create-window :parent *root*
                                           :x x   :y y
                                           :width width   :height height
                                           :background (get-color *expose-background*)
                                           :border-width *border-size*
                                           :border (get-color *expose-border*)
                                           :colormap (xlib:screen-default-colormap *screen*)
                                           :event-mask '(:exposure :key-press)))
               (gc (xlib:create-gcontext :drawable window
                                         :foreground (get-color *expose-foreground*)
                                         :background (get-color *expose-background*)
                                         :font *expose-font*
                                         :line-style :solid)))
          (setf (window-transparency window) *expose-transparency*)
          (map-window window)
          (push (list window gc string child (number->letter n)) *expose-windows-list*))))))




(defun expose-query-key-press-hook (code state)
  (declare (ignore code state))
  (expose-draw-letter)
  (when (and *expose-direct-select* (<= (length *expose-windows-list*) 26))
    (leave-query-mode :return)))

(defun expose-query-button-press-hook (code state x y)
  (declare (ignore state))
  (when (= code 1)
    (setf *expose-selected-child* (find-child-under-mouse x y)))
  (leave-query-mode :click))


(defun expose-init ()
  (setf *expose-font* (xlib:open-font *display* *expose-font-string*)
	*expose-windows-list* nil
	*expose-selected-child* nil
        *query-string* "")
  (xlib:warp-pointer *root* (truncate (/ (xlib:screen-width *screen*) 2))
		     (truncate (/ (xlib:screen-height *screen*) 2)))
  (add-hook *query-key-press-hook* 'expose-query-key-press-hook)
  (add-hook *query-button-press-hook* 'expose-query-button-press-hook))

(defun expose-present-windows ()
  (with-all-root-child (root)
    (with-all-frames (root frame)
      (setf (frame-data-slot frame :old-layout) (frame-layout frame)
            (frame-layout frame) #'tile-space-layout)))
  (show-all-children t))

(defun expose-mode-display-accel-windows ()
  (let ((n -1))
    (with-all-root-child (root)
      (with-all-children-reversed (root child)
        (if (or (frame-p child)
                (managed-window-p child (find-parent-frame child *root-frame*)))
            (expose-create-window child (incf n))
            (hide-child child))))
    (setf *expose-windows-list* (nreverse *expose-windows-list*))
    (expose-draw-letter)))

(defun expose-find-child-from-letters (letters)
  (fourth (find letters *expose-windows-list* :test #'string-equal :key #'fifth)))

(defun expose-select-child ()
  (let ((*query-mode-placement* *expose-query-placement*))
    (multiple-value-bind (letters return)
        (query-string "Which child ?")
      (let ((child (case return
                     (:return (expose-find-child-from-letters letters))
                     (:click *expose-selected-child*))))
        (when (find-child-in-all-root child)
          child)))))

(defun expose-restore-windows ()
  (remove-hook *query-key-press-hook* 'expose-query-key-press-hook)
  (remove-hook *query-button-press-hook* 'expose-query-button-press-hook)
  (dolist (lwin *expose-windows-list*)
    (awhen (first lwin)
      (xlib:destroy-window it))
    (awhen (second lwin)
      (xlib:free-gcontext it)))
  (when *expose-font*
    (xlib:close-font *expose-font*))
  (setf *expose-windows-list* nil)
  (with-all-root-child (root)
    (with-all-frames (root frame)
      (setf (frame-layout frame) (frame-data-slot frame :old-layout)
            (frame-data-slot frame :old-layout) nil))))

(defun expose-focus-child (child)
  (let ((parent (typecase child
                  (xlib:window (find-parent-frame child))
                  (frame child))))
    (when (and child parent)
      (change-root (find-root parent) parent)
      (setf (current-child) child)
      (focus-all-children child parent t))))

(defun expose-do-main ()
  (stop-button-event)
  (expose-init)
  (expose-present-windows)
  (expose-mode-display-accel-windows)
  (let ((child (expose-select-child)))
    (expose-restore-windows)
    child))

(defun expose-windows-mode ()
  "Present all windows in currents roots (An expose like)"
  (awhen (expose-do-main)
    (expose-focus-child it))
  (show-all-children)
  t)


(defun expose-all-windows-mode ()
  "Present all windows in all frames (An expose like)"
  (let ((child nil))
    (with-saved-root-list ()
      (dolist (root (get-root-list))
        (change-root root (root-original root)))
      (setf child (expose-do-main)))
    (when child
      (expose-focus-child child)))
  (show-all-children)
  t)



