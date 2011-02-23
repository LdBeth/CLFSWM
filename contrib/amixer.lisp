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

(format t "Loading amixer code... ")

(defvar *amixer-scontrol* "Master"
  "Default control for amixer commands.")

(defun amixer-cmd (cmd scontrol &rest parameters)
  (let* ((sed "sed 's/^.*\\[\\([[:digit:]]\\+\\)%\\].*\\[\\(on\\|off\\)\\].*$/\\1%\\2/'")
         (fmt "amixer ~A ~A~{ ~A~} 2>/dev/null | tail -1 | ~A")
         (shell (format nil fmt cmd scontrol parameters sed))
         (line (read-line (do-shell shell) nil nil)))
    (when line
      (let* ((ratio (parse-integer line :junk-allowed t))
             (%-pos (position #\% line)))
        (values (and ratio (/ ratio 100))
                (equal "off" (and %-pos (subseq line (1+ %-pos)))))))))

(defun amixer-sset (&rest parameters)
  (apply 'amixer-cmd "sset" *amixer-scontrol* parameters))

(defparameter *volume-mute-function*
  (lambda () (amixer-sset "toggle")))

(defparameter *volume-lower-function*
  (lambda () (amixer-sset "5%-")))

(defparameter *volume-raise-function*
  (lambda () (amixer-sset "5%+")))

(defun amixer-lower-1% ()
  "Lower 1% volume."
  (volume-set (lambda () (amixer-sset "1%-"))))

(defun amixer-raise-1% ()
  "Raise 1% volume."
  (volume-set (lambda () (amixer-sset "1%+"))))

(defun amixer-volume-bind ()
  (define-volume-key ("less") 'amixer-lower-1%)
  (define-volume-key ("greater") 'amixer-raise-1%)
  (define-second-key ("less") 'amixer-lower-1%)
  (define-second-key ("greater") 'amixer-raise-1%))

(add-hook *binding-hook* 'amixer-volume-bind)

(format t "done~%")
