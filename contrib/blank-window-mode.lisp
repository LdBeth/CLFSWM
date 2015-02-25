;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Blank window mode to place blank window on screen and manage
;;;  them with the keyboard or the mouse.
;;;  This is useful when you want to hide some part of the screen (for example
;;;  in school class for interactive presentation).
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2015 Philippe Brochard <pbrochard@common-lisp.net>
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
;;; Documentation: Blank window mode to place blank window on screen.
;;;   If you want to use this file, just add this line in your configuration
;;;   file:
;;;
;;;   (load-contrib "blank-window-mode.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading Blank Window Mode code... ")

(defconfig *blank-window-width* 50 'blank-window "Blank window width")
(defconfig *blank-window-height* 20 'blank-window "Blank window height")
(defconfig *blank-window-color* "white" 'blank-window "Blank window color")
(defconfig *blank-window-border* "magenta" 'blank-window "Blank window border color")


(defparameter *blank-window-list* nil)
(defparameter *in-blank-window-mode* nil)
(defparameter *blank-window-show-current* nil)

(defparameter *blank-window-keys* nil)
(defparameter *blank-window-mouse* nil)


(define-init-hash-table-key *blank-window-keys* "Blank-Window mode keys")
(define-init-hash-table-key *blank-window-mouse* "Blank-Window mode mouse button")

(define-define-key "blank-window" *blank-window-keys*)
(define-define-mouse "blank-window-mouse" *blank-window-mouse*)

(add-hook *binding-hook* 'init-*blank-window-keys*)


(defun leave-blank-window-mode (&optional window root-x root-y)
  "Leave the blank-window mode"
  (declare (ignore window root-x root-y))
  (when *in-blank-window-mode*
    (throw 'exit-blank-window-loop nil)))



(defun bwm-enter-function ()
  (setf *in-blank-window-mode* t)
  (ungrab-main-keys)
  (xgrab-keyboard *root*)
  (xgrab-pointer *root* 66 67)
  (dolist (window *blank-window-list*)
    (raise-window window)))


(defun bwm-leave-function ()
  (setf *in-blank-window-mode* nil)
  (xungrab-keyboard)
  (xungrab-pointer)
  (grab-main-keys)
  (wait-no-key-or-button-press))



(define-handler blank-window-mode :key-press (code state)
  (funcall-key-from-code *blank-window-keys* code state))

(define-handler blank-window-mode :button-press (code state window root-x root-y)
  (funcall-button-from-code *blank-window-mouse* code state window root-x root-y *fun-press*))



(defun blank-window-mode ()
  "Blank window mode"
  (generic-mode 'blank-window-mode
		'exit-blank-window-loop
		:enter-function #'bwm-enter-function
		;;:loop-function #'bwm-loop-function
		:leave-function #'bwm-leave-function
                :original-mode 'main-mode))




(defun create-new-blank-window (&rest args)
  "Create a new blank window"
  (declare (ignore args))
  (with-x-pointer
    (push (xlib:create-window :parent *root*
                              :x (- x 50) :y y
                              :width *blank-window-width* :height *blank-window-height*
                              :background (get-color *blank-window-color*)
                              :border-width 0
                              :border (get-color *blank-window-border*)
                              :colormap (xlib:screen-default-colormap *screen*)
                              :event-mask '(:exposure))
          *blank-window-list*))
  (map-window (first *blank-window-list*)))

(defun clear-all-blank-window ()
  "Clear all blank window"
  (dolist (window *blank-window-list*)
    (hide-window window)
    (xlib:destroy-window window))
  (setf *blank-window-list* nil))

(defmacro with-current-blank-window ((window) &body body)
  `(let ((,window (first *blank-window-list*)))
     (when ,window
       ,@body)))

(defun blank-window-fill-width ()
  "Current blank window fill all width screen"
  (with-current-blank-window (window)
    (setf (xlib:drawable-x window) 0
          (xlib:drawable-width window) (xlib:drawable-width *root*))))

(defun blank-window-fill-height ()
  "Current blank window fill all height screen"
  (with-current-blank-window (window)
    (setf (xlib:drawable-y window) 0
          (xlib:drawable-height window) (xlib:drawable-height *root*))))

(defun blank-window-down (dy)
  "Move current blank window down"
  (with-current-blank-window (window)
    (incf (xlib:drawable-y window) dy)))

(defun blank-window-right (dx)
  "Move current blank window right"
  (with-current-blank-window (window)
    (incf (xlib:drawable-x window) dx)))

(defun blank-window-inc-width (dw)
  "Change current blank window width"
  (with-current-blank-window (window)
    (decf (xlib:drawable-x window) dw)
    (incf (xlib:drawable-width window) (* dw 2))))

(defun blank-window-inc-height (dh)
  "Change current blank window height"
  (with-current-blank-window (window)
    (decf (xlib:drawable-y window) dh)
    (incf (xlib:drawable-height window) (* dh 2))))


(defun select-next-blank-window ()
  "Select next blank window"
  (with-current-blank-window (window)
    (setf (xlib:drawable-border-width window) 0))
  (setf *blank-window-list* (rotate-list *blank-window-list*))
  (when *blank-window-show-current*
    (with-current-blank-window (window)
      (setf (xlib:drawable-border-width window) 1))))

(defun toggle-show-current-blank-window ()
  (setf *blank-window-show-current* (not *blank-window-show-current*))
  (with-current-blank-window (window)
    (setf (xlib:drawable-border-width window) (if *blank-window-show-current* 1 0))))

(defun remove-current-blank-window ()
  (let ((window (pop *blank-window-list*)))
    (when window
      (hide-window window)
      (xlib:destroy-window window)))
  (with-current-blank-window (window)
    (setf (xlib:drawable-border-width window) (if *blank-window-show-current* 1 0))))

(defun find-blank-window-under-mouse ()
  "Return the blank window under the mouse pointer if any"
  (with-x-pointer
    (dolist (win *blank-window-list*)
      (when (in-window win x y)
        (with-current-blank-window (window)
          (setf (xlib:drawable-border-width window) 0))
        (setf *blank-window-list* (remove win *blank-window-list* :test #'xlib:window-equal))
        (push win *blank-window-list*)
        (when *blank-window-show-current*
          (with-current-blank-window (window)
            (setf (xlib:drawable-border-width window) 1)))
        (return-from find-blank-window-under-mouse win)))))

(defun move-blank-window (window root-x root-y)
  "Move blank window with the mouse"
  (declare (ignore window))
  (let ((window (find-blank-window-under-mouse)))
    (when window
      (move-window window root-x root-y))))

(defun resize-blank-window (window root-x root-y)
  "Resize blank window with the mouse"
  (declare (ignore window))
  (let ((window (find-blank-window-under-mouse)))
    (when window
      (resize-window window root-x root-y))))

(defun hide-unhide-current-blank-window ()
  "Hide or unhide the current blank window"
  (with-current-blank-window (window)
    (if (window-hidden-p window)
        (unhide-window window)
        (hide-window window))))


(defun blank-black-window ()
  "Open a black window. ie light of the screen"
  (let ((black-win (xlib:create-window :parent *root*
                                       :x 0 :y 0
                                       :width (xlib:drawable-width *root*)
                                       :height (xlib:drawable-height *root*)
                                       :background (get-color "black")
                                       :border-width 0
                                       :border (get-color "black")
                                       :colormap (xlib:screen-default-colormap *screen*)
                                       :event-mask '(:exposure))))
    (map-window black-win)
    (wait-no-key-or-button-press)
    (wait-a-key-or-button-press)
    (xlib:destroy-window black-win)
    (wait-no-key-or-button-press)))



(defun set-default-blank-window-keys ()
  ;;(define-blank-window-key ("Return") 'leave-blank-window-mode)
  (define-blank-window-key ("Escape") 'leave-blank-window-mode)
  (define-blank-window-key ("twosuperior") 'leave-blank-window-mode)
  (define-blank-window-key ("Return") 'create-new-blank-window)
  (define-blank-window-key ("BackSpace" :control) 'clear-all-blank-window)
  (define-blank-window-key ("Tab") 'select-next-blank-window)
  (define-blank-window-key ("w") 'blank-window-fill-width)
  (define-blank-window-key ("h") 'blank-window-fill-height)
  (define-blank-window-key ("Down") 'blank-window-down 5)
  (define-blank-window-key ("Down" :shift) 'blank-window-down 1)
  (define-blank-window-key ("Down" :control) 'blank-window-down 20)
  (define-blank-window-key ("Up") 'blank-window-down -5)
  (define-blank-window-key ("Up" :shift) 'blank-window-down -1)
  (define-blank-window-key ("Up" :control) 'blank-window-down -20)
  (define-blank-window-key ("Right") 'blank-window-right 5)
  (define-blank-window-key ("Right" :shift) 'blank-window-right 1)
  (define-blank-window-key ("Right" :control) 'blank-window-right 20)
  (define-blank-window-key ("Left") 'blank-window-right -5)
  (define-blank-window-key ("Left" :shift) 'blank-window-right -1)
  (define-blank-window-key ("Left" :control) 'blank-window-right -20)
  (define-blank-window-key ("c") 'toggle-show-current-blank-window)
  (define-blank-window-key ("p") 'blank-window-inc-width 1)
  (define-blank-window-key ("o") 'blank-window-inc-height 1)
  (define-blank-window-key ("m") 'blank-window-inc-width -1)
  (define-blank-window-key ("l") 'blank-window-inc-height -1)
  (define-blank-window-key ("Delete") 'remove-current-blank-window)
  (define-blank-window-key ("t") 'hide-unhide-current-blank-window)
  (define-blank-window-key ("Control_R") 'banish-pointer)
  (define-blank-window-key ("b") 'banish-pointer)
  (define-blank-window-key ("x") 'blank-black-window)

  (define-blank-window-mouse (1) 'move-blank-window)
  (define-blank-window-mouse (2) 'create-new-blank-window)
  (define-blank-window-mouse (3) 'resize-blank-window))



(add-hook *binding-hook* 'set-default-blank-window-keys)



(format t "done~%")
