;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: NetWM functions
;;;   http://freedesktop.org/wiki/Specifications_2fwm_2dspec
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


;;; Client List functions
(defun netwm-set-client-list (id-list)
  (xlib:change-property *root* :_NET_CLIENT_LIST id-list :window 32))

(defun netwm-get-client-list ()
  (xlib:get-property *root* :_NET_CLIENT_LIST))

(defun netwm-add-in-client-list (window)
  (let ((last-list (netwm-get-client-list)))
    (pushnew (xlib:window-id window) last-list)
    (netwm-set-client-list last-list)))

(defun netwm-remove-in-client-list (window)
  (netwm-set-client-list (remove (xlib:window-id window) (netwm-get-client-list))))



;;; Desktop functions ;; +PHIL
(defun netwm-update-desktop-property ()
  ;;  (xlib:change-property *root* :_NET_NUMBER_OF_DESKTOPS
  ;;		   (list (length *workspace-list*)) :cardinal 32)
  ;;  (xlib:change-property *root* :_NET_DESKTOP_GEOMETRY
  ;;		   (list (xlib:screen-width *screen*)
  ;;			 (xlib:screen-height *screen*))
  ;;		   :cardinal 32)
  ;;  (xlib:change-property *root* :_NET_DESKTOP_VIEWPORT
  ;;		   (list 0 0) :cardinal 32)
  ;;  (xlib:change-property *root* :_NET_CURRENT_DESKTOP
  ;;		   (list 1) :cardinal 32)
;;; TODO
  ;;(xlib:change-property *root* :_NET_DESKTOP_NAMES
  ;;		   (list "toto" "klm" "poi") :string 8 :transform #'xlib:char->card8))
  )




;;; Taken from stumpwm (thanks)
(defun netwm-set-properties ()
  "Set NETWM properties on the root window of the specified screen.
FOCUS-WINDOW is an extra window used for _NET_SUPPORTING_WM_CHECK."
  ;; _NET_SUPPORTED
  (xlib:change-property *root* :_NET_SUPPORTED
			(mapcar (lambda (a)
				  (xlib:intern-atom *display* a))
				(append +netwm-supported+
					(mapcar 'car +netwm-window-types+)))
			:atom 32)
  ;; _NET_SUPPORTING_WM_CHECK
  (xlib:change-property *root* :_NET_SUPPORTING_WM_CHECK
			(list *no-focus-window*) :window 32
			:transform #'xlib:drawable-id)
  (xlib:change-property *no-focus-window* :_NET_SUPPORTING_WM_CHECK
			(list *no-focus-window*) :window 32
			:transform #'xlib:drawable-id)
  (xlib:change-property *no-focus-window* :_NET_WM_NAME
			"clfswm"
			:string 8 :transform #'xlib:char->card8)
  (netwm-update-desktop-property))





