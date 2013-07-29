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

(defparameter *fastswitch-window* nil)
(defparameter *fastswitch-gc* nil)
(defparameter *fastswitch-font* nil)
(defparameter *fastswitch-string* "")
(defparameter *fastswitch-match-child* nil)


(defun leave-fastswitch-mode ()
  "Leave the fastswitch mode"
  (throw 'exit-fastswitch-loop nil))



(defun fastswitch-draw-window ()
  (labels ((display-match-child ()
             (let ((pos 1))
               (dolist (ex-child *fastswitch-match-child*)
                 (xlib:with-gcontext (*fastswitch-gc* :foreground (get-color *fastswitch-foreground-letter-second*))
                   (xlib:draw-glyphs *pixmap-buffer* *fastswitch-gc*
                                     (* (xlib:max-char-width *fastswitch-font*) pos)
                                     (+ (* 2 (xlib:font-ascent *fastswitch-font*)) (xlib:font-descent *fastswitch-font*) 1)
                                     (expose-child-key ex-child)))
                 (incf pos (length (expose-child-key ex-child)))
                 (xlib:draw-glyphs *pixmap-buffer* *fastswitch-gc*
                                   (* (xlib:max-char-width *fastswitch-font*) pos)
                                   (+ (* 2 (xlib:font-ascent *fastswitch-font*)) (xlib:font-descent *fastswitch-font*) 1)
                                   ":")
                 (incf pos)
                 (xlib:with-gcontext (*fastswitch-gc* :foreground (get-color *fastswitch-foreground-childname*))
                   (xlib:draw-glyphs *pixmap-buffer* *fastswitch-gc*
                                     (* (xlib:max-char-width *fastswitch-font*) pos)
                                     (+ (* 2 (xlib:font-ascent *fastswitch-font*)) (xlib:font-descent *fastswitch-font*) 1)
                                     (child-fullname (expose-child-child ex-child)))
                   (incf pos (1+ (length (child-fullname (expose-child-child ex-child))))))))))
    (clear-pixmap-buffer *fastswitch-window* *fastswitch-gc*)
    (xlib:with-gcontext (*fastswitch-gc* :foreground (get-color *fastswitch-foreground-letter*)
                                         :background (get-color *fastswitch-background*))
      (xlib:draw-image-glyphs *pixmap-buffer* *fastswitch-gc*
                              (xlib:max-char-width *fastswitch-font*)
                              (+ (xlib:font-ascent *fastswitch-font*) (xlib:font-descent *fastswitch-font*))
                              *fastswitch-string*))
    (display-match-child)
    (copy-pixmap-buffer *fastswitch-window* *fastswitch-gc*)))



(defun fastswitch-init ()
  (setf *fastswitch-font* (xlib:open-font *display* *fastswitch-font-string*)
        *fastswitch-string* ""
        *fastswitch-match-child* (string-match *fastswitch-string* *expose-child-list* #'expose-child-key))
  (let* ((width (- (xlib:screen-width *screen*) 2)) ;;(* (xlib:max-char-width *fastswitch-font*) 3))
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
  (fastswitch-draw-window))


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
      (fastswitch-draw-window))))


(defun fastswitch-do-main ()
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
  (let ((ex-child (fastswitch-do-main)))
    (when (and ex-child (expose-child-child ex-child))
      (expose-focus-child (expose-child-child ex-child))))
  (show-all-children)
  t)



