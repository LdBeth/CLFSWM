;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Main functions
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


(defun generic-mode (exit-tag &key enter-function loop-function leave-function
		     (loop-hook *loop-hook*)
		     (button-press-hook *button-press-hook*)
		     (button-release-hook *button-release-hook*)
		     (motion-notify-hook *motion-notify-hook*)
		     (key-press-hook *key-press-hook*)
		     (key-release-hook *key-release-hook*)
		     (configure-request-hook *configure-request-hook*)
		     (configure-notify-hook *configure-notify-hook*)
		     (map-request-hook *map-request-hook*)
		     (unmap-notify-hook *unmap-notify-hook*)
		     (destroy-notify-hook *destroy-notify-hook*)
		     (mapping-notify-hook *mapping-notify-hook*)
		     (property-notify-hook *property-notify-hook*)
		     (create-notify-hook *create-notify-hook*)
		     (enter-notify-hook *enter-notify-hook*)
		     (exposure-hook *exposure-hook*))
  "Enter in a generic mode"
  (labels ((handler-function (&rest event-slots &key display event-key &allow-other-keys)
	     (declare (ignore display))
	     ;; (dbg event-key)
	     (with-xlib-protect
	       (case event-key
		 (:button-press (call-hook button-press-hook event-slots))
		 (:button-release (call-hook button-release-hook event-slots))
		 (:motion-notify (call-hook motion-notify-hook event-slots))
		 (:key-press (call-hook key-press-hook event-slots))
		 (:key-release (call-hook key-release-hook event-slots))
		 (:configure-request (call-hook configure-request-hook event-slots))
		 (:configure-notify (call-hook configure-notify-hook event-slots))
		 (:map-request (call-hook map-request-hook event-slots))
		 (:unmap-notify (call-hook unmap-notify-hook event-slots))
		 (:destroy-notify (call-hook destroy-notify-hook event-slots))
		 (:mapping-notify (call-hook mapping-notify-hook event-slots))
		 (:property-notify (call-hook property-notify-hook event-slots))
		 (:create-notify (call-hook create-notify-hook event-slots))
		 (:enter-notify (call-hook enter-notify-hook event-slots))
		 (:exposure (call-hook exposure-hook event-slots))))
	     ;;(dbg "Ignore handle event" c event-slots)))
	     t))
    (nfuncall enter-function)
    (unwind-protect
	 (catch exit-tag
	   (loop
	      (call-hook loop-hook)
	      (nfuncall loop-function)
	      (xlib:display-finish-output *display*)
	      (xlib:process-event *display* :handler #'handler-function :timeout *loop-timeout*)
	      (xlib:display-finish-output *display*)))
      (nfuncall leave-function))))
