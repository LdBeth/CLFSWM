;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Main functions
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
;;; --------------------------------------------------------------------------

(in-package :clfswm)


(defun generic-mode (mode exit-tag &key enter-function loop-function leave-function
		     (loop-hook *loop-hook*) original-mode)
  "Enter in a generic mode"
  (let ((last-mode *current-event-mode*))
    (unassoc-keyword-handle-event)
    (when original-mode
      (dolist (add-mode (ensure-list original-mode))
	(assoc-keyword-handle-event add-mode)))
    (assoc-keyword-handle-event mode)
    (nfuncall enter-function)
    (catch exit-tag
      (unwind-protect
	   (loop
	      (call-hook loop-hook)
	      (process-timers)
	      (nfuncall loop-function)
	      (when (xlib:event-listen *display* *loop-timeout*)
		(xlib:process-event *display* :handler #'handle-event))
	      (xlib:display-finish-output *display*))
	(nfuncall leave-function)
	(unassoc-keyword-handle-event)
	(assoc-keyword-handle-event last-mode)))))

