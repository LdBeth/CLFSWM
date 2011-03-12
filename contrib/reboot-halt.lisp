;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Reboot and halt menu
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
;;;   (load-contrib "mpd.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading Reboot/Halt code... ")


(defun reboot-halt-menu ()
  "Open the Reboot/Halt menu"
  (open-menu (find-menu 'reboot-halt-menu)))


(defun do-with-terminal (command)
  (do-shell (format nil "xterm -e '~A'" command)))
;;(do-shell (format nil "xterm -e 'echo ~A; sleep 3'" command)))  ;; test

(defun do-nothing ()
  "Do nothing"
  ())

(defun do-suspend ()
  "Suspend the computer to RAM"
  (do-with-terminal "sudo pm-suspend"))

(defun do-hibernate ()
  "Suspend the computer to DISK"
  (do-with-terminal "sudo pm-hibernate"))

(defun do-reboot ()
  "Reboot the computer"
  (do-with-terminal "sudo reboot"))

(defun do-halt ()
  "Halt the computer"
  (do-with-terminal "sudo halt"))

(unless (find-menu 'reboot-halt-menu)
  (add-sub-menu 'clfswm-menu "Pause" 'reboot-halt-menu "Suspend/Reboot/Halt menu")
  (add-menu-key 'reboot-halt-menu "-" 'do-nothing)
  (add-menu-key 'reboot-halt-menu "s" 'do-suspend)
  (add-menu-key 'reboot-halt-menu "d" 'do-hibernate)
  (add-menu-key 'reboot-halt-menu "r" 'do-reboot)
  (add-menu-key 'reboot-halt-menu "h" 'do-halt))


(defun reboot-halt-binding ()
  (define-main-key ("Pause") 'reboot-halt-menu))

(add-hook *binding-hook* 'reboot-halt-binding)

(format t "done~%")
