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
;;; Documentation: Handle the CD player
;;;   This code needs pcd (http://hocwp.free.fr/pcd.html).
;;    If you want to use this file, just add this line in
;;;   your configuration file:
;;;
;;;     (load-contrib "cd-player.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading CDPLAYER code... ")

(defun cdplayer-menu ()
  "Open the CDPLAYER menu"
  (open-menu (find-menu 'cdplayer-menu)))

(defun cdplayer-play ()
  "Start playing CD"
  (do-shell "pcd play"))

(defun cdplayer-stop ()
  "Stop playing CD"
  (do-shell "pcd stop"))

(defun cdplayer-pause ()
  "Toggle pause"
  (do-shell "pcd toggle"))

(defun show-cdplayer-status ()
  "Show the current CD status"
  (info-on-shell "CDPLAYER status:" "pcd info")
  (cdplayer-menu))

(defun show-cdplayer-playlist ()
  "Show the current CD playlist"
  (info-on-shell "CDPLAYER:" "pcd more_info")
  (cdplayer-menu))

(defun cdplayer-next-track ()
  "Play the next CD track"
  (do-shell "pcd next")
  (cdplayer-menu))

(defun cdplayer-previous-track ()
  "Play the previous CD track"
  (do-shell "pcd previous")
  (cdplayer-menu))

(defun cdplayer-eject ()
  "Eject CD"
  (do-shell "pcd eject"))

(defun cdplayer-close ()
  "Close CD"
  (do-shell "pcd close"))

(unless (find-menu 'cdplayer-menu)
  (add-sub-menu 'help-menu "i" 'cdplayer-menu "CDPLAYER menu")

  (add-menu-key 'cdplayer-menu "y" 'cdplayer-play)
  (add-menu-key 'cdplayer-menu "k" 'cdplayer-stop)
  (add-menu-key 'cdplayer-menu "t" 'cdplayer-pause)
  (add-menu-key 'cdplayer-menu "s" 'show-cdplayer-status)
  (add-menu-key 'cdplayer-menu "l" 'show-cdplayer-playlist)
  (add-menu-key 'cdplayer-menu "n" 'cdplayer-next-track)
  (add-menu-key 'cdplayer-menu "p" 'cdplayer-previous-track)
  (add-menu-key 'cdplayer-menu "e" 'cdplayer-eject)
  (add-menu-key 'cdplayer-menu "c" 'cdplayer-close))

(format t "done~%")
