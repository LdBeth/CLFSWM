;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Volume mode
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2011 Desmond O. Chang <dochang@gmail.com>
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
;;; Documentation: A volume mode.
;;;   If you want to use this file, just add this line in
;;; your configuration file:
;;;
;;;   (load-contrib "volume-mode.lisp")
;;; And with the alsa mixer:
;;;   (load-contrib "amixer.lisp")
;;;
;;;  This mode is inspired by the emms volume package.  When you change the
;;;  volume in main mode or second mode, clfswm will enter volume mode and
;;;  set a timer to leave this mode.  Changing volume in volume mode will
;;;  reset the timer.  You can also leave volume mode manually by return,
;;;  escape or control-g.
;;;
;;;  Special variable *VOLUME-MODE-TIMEOUT* controls the timeout in
;;;  seconds.  If it's positive, volume mode will exit when timeout occurs;
;;;  if it's 0, volume mode will exit right now; if it's negative, volume
;;;  will not exit even if timeout occurs.  Default timeout is 3 seconds.
;;;
;;;  Volume mode uses three special variables to control the mixer:
;;;  *VOLUME-MUTE-FUNCTION*, *VOLUME-LOWER-FUNCTION* and
;;;  *VOLUME-RAISE-FUNCTION*.  Their values are functions which must accept
;;;  no arguments and return two values indicating the mixer state.  The
;;;  first value is the volume ratio whose type must be (real 0 1).  If the
;;;  mixer is mute, the second value should be true, otherwise it should be
;;;  false.  If volume controller cannot get the mixer state, it must
;;;  return NIL.
;;;
;;;  Volume mode shows a mute sign, a percentage and a ratio bar on the
;;;  screen.  A plus sign '+' means it's unmute and a minus sign '-' means
;;;  it's mute now.  If volume mode doesn't know the mixer state, a message
;;;  "unknown" will be shown.
;;;
;;;  contrib/amixer.lisp shows how to use volume mode with alsa.
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading Volume mode code... ")

(defparameter *volume-keys* nil)
(defconfig *volume-mode-placement* 'bottom-middle-placement
  'Placement "Volume mode window placement")


(defvar *volume-window* nil)
(defvar *volume-font* nil)
(defvar *volume-gc* nil)
(defvar *in-volume-mode* nil)
(defvar *leave-volume-mode* nil)

(defvar *volume-ratio* nil)
(defvar *volume-mute* nil)

(defvar *volume-mode-timeout* 3
  "Volume mode timeout in seconds:
> 0 means volume mode will exit when timeout occurs;
= 0 means exit right now;
< 0 means exit manually.")


;;; CONFIG - Volume mode
(defconfig *volume-font-string* *default-font-string*
  'Volume-mode "Volume string window font string")
(defconfig *volume-background* "black"
  'Volume-mode "Volume string window background color")
(defconfig *volume-foreground* "green"
  'Volume-mode "Volume string window foreground color")
(defconfig *volume-border* "red"
  'Volume-mode "Volume string window border color")
(defconfig *volume-width* 400
  'Volume-mode "Volume mode window width")
(defconfig *volume-height* 15
  'Volume-mode "Volume mode window height")
(defconfig *volume-text-limit* 30
  'Volume-mode "Maximum text limit in the volume window")
(defconfig *volume-external-mixer-cmd* "/usr/bin/gnome-alsamixer"
  'Volume-mode "Command to start an external mixer program")

(define-init-hash-table-key *volume-keys* "Volume mode keys")
(define-define-key "volume" *volume-keys*)

(add-hook *binding-hook* 'init-*volume-keys*)

(defun set-default-volume-keys ()
  (define-volume-key ("XF86AudioMute") 'volume-mute)
  (define-volume-key ("XF86AudioLowerVolume") 'volume-lower)
  (define-volume-key ("XF86AudioRaiseVolume") 'volume-raise)
  (define-volume-key (#\/) 'volume-mute)
  (define-volume-key (#\,) 'volume-lower)
  (define-volume-key (#\.) 'volume-raise)
  (define-volume-key ("m") 'volume-mute)
  (define-volume-key ("l") 'volume-lower)
  (define-volume-key ("r") 'volume-raise)
  (define-volume-key ("Down") 'volume-lower)
  (define-volume-key ("Up") 'volume-raise)
  (define-volume-key ("Left") 'volume-lower)
  (define-volume-key ("Right") 'volume-raise)
  (define-volume-key ("PageUp") 'volume-lower)
  (define-volume-key ("PageDown") 'volume-raise)
  (define-volume-key ("Return") 'leave-volume-mode)
  (define-volume-key ("Escape") 'leave-volume-mode)
  (define-volume-key ("g" :control) 'leave-volume-mode)
  (define-volume-key ("e") 'run-external-volume-mixer)
  ;;; Main mode
  (define-main-key ("XF86AudioMute") 'volume-mute)
  (define-main-key ("XF86AudioLowerVolume") 'volume-lower)
  (define-main-key ("XF86AudioRaiseVolume") 'volume-raise)
  ;;; Second mode
  (define-second-key ("XF86AudioMute") 'volume-mute)
  (define-second-key ("XF86AudioLowerVolume") 'volume-lower)
  (define-second-key ("XF86AudioRaiseVolume") 'volume-raise))

(add-hook *binding-hook* 'set-default-volume-keys)

(defun volume-mode-window-message (width)
  (if *volume-ratio*
      (let* ((mute (if *volume-mute* #\- #\+))
             (percentage (round (* 100 *volume-ratio*)))
             (n (round (* width *volume-ratio*))))
        (format nil "[~A] ~3@A% ~A~A" mute percentage
                (repeat-chars n #\#) (repeat-chars (- width n) #\.)))
      "unknown"))

(defun draw-volume-mode-window ()
  (raise-window *volume-window*)
  (clear-pixmap-buffer *volume-window* *volume-gc*)
  (let* ((text (limit-length (volume-mode-window-message 20) *volume-text-limit*))
         (len (length text)))
    (xlib:draw-glyphs *pixmap-buffer* *volume-gc*
                      (truncate (/ (- *volume-width* (* (xlib:max-char-width *volume-font*) len)) 2))
                      (truncate (/ (+ *volume-height* (- (xlib:font-ascent *volume-font*) (xlib:font-descent *volume-font*))) 2))
                      text))
  (copy-pixmap-buffer *volume-window* *volume-gc*))

(defun leave-volume-mode ()
  "Leave the volume mode"
  (throw 'exit-volume-loop nil))

(defun update-volume-mode ()
  (draw-volume-mode-window)
  (cond ((plusp *volume-mode-timeout*)
         (erase-timer :volume-mode-timer)
         (with-timer (*volume-mode-timeout* :volume-mode-timer)
           (setf *leave-volume-mode* t)))
        ((zerop *volume-mode-timeout*)
         (erase-timer :volume-mode-timer)
         (setf *leave-volume-mode* t))
        ((minusp *volume-mode-timeout*)
         (erase-timer :volume-mode-timer))))

(defun volume-enter-function ()
  (with-placement (*volume-mode-placement* x y *volume-width* *volume-height*)
    (setf *volume-font* (xlib:open-font *display* *volume-font-string*)
          *volume-window* (xlib:create-window :parent *root*
                                              :x x
                                              :y y
                                              :width *volume-width*
                                              :height *volume-height*
                                              :background (get-color *volume-background*)
                                              :border-width 1
                                              :border (get-color *volume-border*)
                                              :colormap (xlib:screen-default-colormap *screen*)
                                              :event-mask '(:exposure :key-press))
          *volume-gc* (xlib:create-gcontext :drawable *volume-window*
                                            :foreground (get-color *volume-foreground*)
                                            :background (get-color *volume-background*)
                                            :font *volume-font*
                                            :line-style :solid))
    (map-window *volume-window*))
  (setf *in-volume-mode* t
        *leave-volume-mode* nil)
  (update-volume-mode))

(defun volume-loop-function ()
  (when *leave-volume-mode*
    (leave-volume-mode)))

(defun volume-leave-function ()
  (when *volume-gc*
    (xlib:free-gcontext *volume-gc*))
  (when *volume-window*
    (xlib:destroy-window *volume-window*))
  (when *volume-font*
    (xlib:close-font *volume-font*))
  (xlib:display-finish-output *display*)
  (erase-timer :volume-mode-timer)
  (setf *volume-window* nil
        *volume-gc* nil
        *volume-font* nil
        *in-volume-mode* nil
        *leave-volume-mode* nil))

(define-handler volume-mode :key-press (code state)
  (funcall-key-from-code *volume-keys* code state))

(defun volume-mode ()
  (let ((grab-keyboard-p (xgrab-keyboard-p))
        (grab-pointer-p (xgrab-pointer-p)))
    (xgrab-pointer *root* 92 93)
    (unless grab-keyboard-p
      (ungrab-main-keys)
      (xgrab-keyboard *root*))
    (generic-mode 'volume-mode 'exit-volume-loop
                  :enter-function 'volume-enter-function
                  :loop-function 'volume-loop-function
                  :leave-function 'volume-leave-function
                  :original-mode '(main-mode))
    (unless grab-keyboard-p
      (xungrab-keyboard)
      (grab-main-keys))
    (if grab-pointer-p
        (xgrab-pointer *root* 66 67)
        (xungrab-pointer))))

(defun volume-set (fn)
  (when fn
    (setf (values *volume-ratio* *volume-mute*) (funcall fn))
    (if *in-volume-mode*
        (update-volume-mode)
        (volume-mode))))

(defvar *volume-mute-function* nil)
(defvar *volume-lower-function* nil)
(defvar *volume-raise-function* nil)

(defun volume-mute ()
  "Toggle mute."
  (volume-set *volume-mute-function*))

(defun volume-lower ()
  "Lower volume."
  (volume-set *volume-lower-function*))

(defun volume-raise ()
  "Raise volume."
  (volume-set *volume-raise-function*))

(defun run-external-volume-mixer ()
  "Start an external volume mixer"
  (do-shell *volume-external-mixer-cmd*))

(format t "done~%")
