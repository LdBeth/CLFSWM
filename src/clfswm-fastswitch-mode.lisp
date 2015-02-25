;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Fast switch mode - Like expose mode but faster since
;;; children are not moved/resized. Shortcut key is associated to Xid for
;;; windows and to numbers for frames.
;;; A window or a frame will always have the same shortcut.
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

(defparameter *fastswitch-window* nil)
(defparameter *fastswitch-gc* nil)
(defparameter *fastswitch-font* nil)
(defparameter *fastswitch-string* "")
(defparameter *fastswitch-match-child* nil)
(defparameter *fastswitch-msg* nil)

(defun leave-fastswitch-mode ()
  "Leave the fastswitch mode"
  (throw 'exit-fastswitch-loop nil))


(defun fastswitch-draw-child-name (posx posy ex-child)
  (let ((placey (* posy (+ (xlib:font-ascent *fastswitch-font*)
                           (xlib:font-descent *fastswitch-font*) 1))))
    (xlib:with-gcontext (*fastswitch-gc*
                         :foreground (get-color (if (frame-p (expose-child-child ex-child))
                                                    *fastswitch-foreground-letter-second-frame*
                                                    *fastswitch-foreground-letter-second*)))
      (xlib:draw-glyphs *pixmap-buffer* *fastswitch-gc*
                        (* (xlib:max-char-width *fastswitch-font*) posx)
                        placey
                        (expose-child-key ex-child)))
    (incf posx (length (expose-child-key ex-child)))
    (xlib:draw-glyphs *pixmap-buffer* *fastswitch-gc*
                      (* (xlib:max-char-width *fastswitch-font*) posx)
                      placey
                      ":")
    (incf posx 1)
    (xlib:with-gcontext (*fastswitch-gc* :foreground (get-color *fastswitch-foreground-childname*))
      (xlib:draw-glyphs *pixmap-buffer* *fastswitch-gc*
                        (* (xlib:max-char-width *fastswitch-font*) posx)
                        placey
                        (ensure-printable (child-fullname (expose-child-child ex-child))))
      (incf posx (1+ (length (child-fullname (expose-child-child ex-child))))))
    posx))

(defun fastswitch-draw-window ()
  (labels ((display-match-child ()
             (let ((posx 1)
                   (posy 2))
               (dolist (ex-child *fastswitch-match-child*)
                 (when (or *fastswitch-show-frame-p* (not (frame-p (expose-child-child ex-child))))
                   (setf posx (fastswitch-draw-child-name posx posy ex-child))
                   (when (> (* posx (xlib:max-char-width *fastswitch-font*))
                            (x-drawable-width *fastswitch-window*))
                     (if *fastswitch-adjust-window-p*
                         (setf posx 1
                               posy (1+ posy))
                         (return)))))))
           (adjust-window ()
             (setf (x-drawable-height *fastswitch-window*) (* (xlib:font-ascent *fastswitch-font*) 3))
             (let ((posx 1)
                   (inc 0))
               (dolist (ex-child *fastswitch-match-child*)
                 (when (or *fastswitch-show-frame-p* (not (frame-p (expose-child-child ex-child))))
                   (incf posx (length (expose-child-key ex-child)))
                   (incf posx)
                   (incf posx (1+ (length (child-fullname (expose-child-child ex-child)))))
                   (when (> (* posx (xlib:max-char-width *fastswitch-font*))
                            (x-drawable-width *fastswitch-window*))
                     (setf posx 1)
                     (incf inc (+ (xlib:font-ascent *fastswitch-font*)
                                  (xlib:font-descent *fastswitch-font*) 1)))))
               (incf (x-drawable-height *fastswitch-window*) inc))))
    (when *fastswitch-adjust-window-p*
      (adjust-window))
    (clear-pixmap-buffer *fastswitch-window* *fastswitch-gc*)
    (when *fastswitch-msg*
      (xlib:draw-image-glyphs *pixmap-buffer* *fastswitch-gc*
                              (xlib:max-char-width *fastswitch-font*)
                              (+ (xlib:font-ascent *fastswitch-font*) (xlib:font-descent *fastswitch-font*))
                              *fastswitch-msg*))
    (xlib:with-gcontext (*fastswitch-gc* :foreground (get-color *fastswitch-foreground-letter*)
                                         :background (get-color *fastswitch-background*))
      (xlib:draw-image-glyphs *pixmap-buffer* *fastswitch-gc*
                              (* (xlib:max-char-width *fastswitch-font*)
                                 (if *fastswitch-msg*
                                     (1+ (length *fastswitch-msg*))
                                     1))
                              (+ (xlib:font-ascent *fastswitch-font*) (xlib:font-descent *fastswitch-font*))
                              *fastswitch-string*))
    (display-match-child)
    (copy-pixmap-buffer *fastswitch-window* *fastswitch-gc*)))

(defun fastswitch-draw-window-tree ()
  (let ((posy 2))
    (labels ((display-match-child (child space)
               (let ((ex-child (find child *expose-child-list* :test #'child-equal-p :key #'expose-child-child)))
                 (when ex-child
                   (fastswitch-draw-child-name space posy ex-child)
                   (incf posy)))
               (when (frame-p child)
                 (dolist (c (frame-child child))
                   (display-match-child c (+ space 2))))))
      (setf (x-drawable-height *fastswitch-window*)
            (+ (* (xlib:font-ascent *fastswitch-font*) 3)
               (* (1- (length *expose-child-list*))
                  (+ (xlib:font-ascent *fastswitch-font*)
                     (xlib:font-descent *fastswitch-font*) 1))))
      (clear-pixmap-buffer *fastswitch-window* *fastswitch-gc*)
      (when *fastswitch-msg*
        (xlib:draw-image-glyphs *pixmap-buffer* *fastswitch-gc*
                                (xlib:max-char-width *fastswitch-font*)
                                (+ (xlib:font-ascent *fastswitch-font*) (xlib:font-descent *fastswitch-font*))
                                *fastswitch-msg*))
      (xlib:with-gcontext (*fastswitch-gc* :foreground (get-color *fastswitch-foreground-letter*)
                                           :background (get-color *fastswitch-background*))
        (xlib:draw-image-glyphs *pixmap-buffer* *fastswitch-gc*
                                (* (xlib:max-char-width *fastswitch-font*)
                                   (if *fastswitch-msg*
                                       (1+ (length *fastswitch-msg*))
                                       1))
                                (+ (xlib:font-ascent *fastswitch-font*) (xlib:font-descent *fastswitch-font*))
                                *fastswitch-string*))
      (display-match-child *root-frame* 0)
      (copy-pixmap-buffer *fastswitch-window* *fastswitch-gc*))))


(defun fastswitch-draw-window-generic ()
  (if (eq *fastswitch-display-mode* 'TREE)
      (fastswitch-draw-window-tree)
      (fastswitch-draw-window)))



(defun fastswitch-init ()
  (setf *fastswitch-font* (xlib:open-font *display* *fastswitch-font-string*)
        *fastswitch-string* ""
        *fastswitch-match-child* (string-match *fastswitch-string* *expose-child-list* #'expose-child-key))
  (let* ((width (- (screen-width) 2))
         (height (* (xlib:font-ascent *fastswitch-font*) 3)))
    (with-placement (*fastswitch-mode-placement* x y width height)
      (setf *fastswitch-window* (xlib:create-window :parent *root*
                                                    :x x   :y y
                                                    :width width   :height height
                                                    :background (get-color *fastswitch-background*)
                                                    :border-width *border-size*
                                                    :border (get-color *fastswitch-border*)
                                                    :colormap (xlib:screen-default-colormap *screen*)
                                                    :event-mask '(:exposure :key-press))
            *fastswitch-gc* (xlib:create-gcontext :drawable *fastswitch-window*
                                                  :foreground (get-color *fastswitch-foreground*)
                                                  :background (get-color *fastswitch-background*)
                                                  :font *fastswitch-font*
                                                  :line-style :solid))
      (setf (window-transparency *fastswitch-window*) *fastswitch-transparency*)
      (map-window *fastswitch-window*)))
  (fastswitch-draw-window-generic))


(defun fastswitch-enter-function ()
  (stop-button-event)
  (fastswitch-init))


(defun fastswitch-leave-function ()
  (when *fastswitch-gc*
    (xlib:free-gcontext *fastswitch-gc*))
  (when *fastswitch-window*
    (xlib:destroy-window *fastswitch-window*))
  (when *expose-font*
    (xlib:close-font *expose-font*))
  (setf *fastswitch-window* nil
  	*fastswitch-gc* nil
  	*fastswitch-font* nil)
  (xlib:display-finish-output *display*))


(defun fastswitch-loop-function ()
  (unless (is-a-key-pressed-p)
    (leave-fastswitch-mode)))

(define-handler fastswitch-mode :key-press (code state)
  (let ((char (keycode->char code state)))
    (when char
      (setf *fastswitch-string* (format nil "~A~A" *fastswitch-string* char)
            *fastswitch-match-child* (string-match *fastswitch-string* *expose-child-list* #'expose-child-key))
      (unless *fastswitch-match-child*
        (setf *fastswitch-string* ""
              *fastswitch-match-child* (string-match *fastswitch-string* *expose-child-list* #'expose-child-key)))
      (fastswitch-draw-window-generic))))


(defun fastswitch-select-child ()
  (with-grab-keyboard-and-pointer (92 93 66 67 t)
    (generic-mode 'fastswitch-mode 'exit-fastswitch-loop
                  :enter-function #'fastswitch-enter-function
                  :loop-function #'fastswitch-loop-function
                  :leave-function #'fastswitch-leave-function
                  :original-mode '(main-mode))
    (fastswitch-leave-function))
  (expose-find-child-from-letters *fastswitch-string*))



(defun fastswitch-mode ()
  "Switch between children with expose shortcut"
  (setf *expose-child-list* (expose-associate-keys))
  (setf *fastswitch-msg* "Select child:  ")
  (let ((ex-child (fastswitch-select-child)))
    (when (and ex-child (expose-child-child ex-child))
      (expose-focus-child (expose-child-child ex-child))))
  (show-all-children)
  t)



;;; Fastswitch move mode
(defun fastswitch-move-mode ()
  "Move children with expose shortcut"
  (let ((window nil))
    (with-focus-window (win)
      (setf window win))
    (no-focus)
    (setf *expose-child-list* (expose-associate-keys))
    (setf *fastswitch-msg* (if window
                               (format nil "Move focused child [~A] with:  "
                                       (child-fullname window))
                               "No child to move...  "))
    (let ((ex-child (fastswitch-select-child)))
      (when (and window ex-child (expose-child-child ex-child))
        (let ((from (find-parent-frame window))
              (to (typecase (expose-child-child ex-child)
                    (xlib:window (find-parent-frame (expose-child-child ex-child)))
                    (frame (expose-child-child ex-child)))))
          (when (and (frame-p from) (frame-p to))
            (remove-child-in-frame window from)
            (pushnew window (frame-child to) :test #'child-equal-p)
            (focus-all-children from from)))))
    (show-all-children))
  t)
