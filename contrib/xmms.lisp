;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Music Player Daemon (MPD) interface
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
;;; Documentation: Handle the XMMS player
;;;   This code needs xmmsctrl.
;;    If you want to use this file, just add this line in
;;;   your configuration file:
;;;
;;;     (load-contrib "xmms.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading XMMS code... ")

(defun xmms-menu ()
  "Open the XMMS menu"
  (open-menu (find-menu 'xmms-menu)))

(defun launch-xmms ()
  "Lanch XMMS"
  (do-shell "xmmsctrl launch"))

(defun show-xmms-status ()
  "Show the current xmms status"
  (info-on-shell "XMMS status:" "xmmsctrl cur"))

(defun show-xmms-playlist ()
  "Show the current xmms playlist"
  (info-on-shell "XMMS Playlist:" "xmmsctrl playlist"))

(defun xmms-next-track ()
  "Play the next XMMS track"
  (do-shell "xmmsctrl next")
  (show-xmms-status)
  (xmms-menu))

(defun xmms-previous-track ()
  "Play the previous XMMS track"
  (do-shell "xmmsctrl previous")
  (show-xmms-status)
  (xmms-menu))

(defun xmms-load-file ()
  "open xmms \"Load file(s)\" dialog window."
  (do-shell "xmmsctrl eject"))

(unless (find-menu 'xmms-menu)
  (add-sub-menu 'help-menu "x" 'xmms-menu "XMMS menu")

  (add-menu-key 'xmms-menu "r" 'launch-xmms)
  (add-menu-key 'xmms-menu "s" 'show-xmms-status)
  (add-menu-key 'xmms-menu "l" 'show-xmms-playlist)
  (add-menu-key 'xmms-menu "n" 'xmms-next-track)
  (add-menu-key 'xmms-menu "p" 'xmms-previous-track)
  (add-menu-key 'xmms-menu "e" 'xmms-load-file))

(format t "done~%")
