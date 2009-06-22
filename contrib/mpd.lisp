;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Music Player Daemon (MPD) interface
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2009 Philippe Brochard <hocwp@free.fr>
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

(format t "Loading MPD code... ")


(defun start-sonata ()
  "Start sonata"
  (do-shell "exec sonata"))


(defun show-mpd-info ()
  "Show MPD informations"
  (info-on-shell "MPD informations:" "mpc"))

(defun mpd-previous ()
  "Play the previous song in the current playlist"
  (do-shell "mpc prev"))

(defun mpd-next ()
  "Play the next song in the current playlist"
  (do-shell "mpc next"))

(defun mpd-toggle ()
  "Toggles Play/Pause, plays if stopped"
  (do-shell "mpc toggle"))

(defun mpd-play ()
  "Start playing"
  (do-shell "mpc play"))

(defun mpd-stop ()
  "Stop the currently playing playlists"
  (do-shell "mpc stop"))


(defun mpd-seek-+5% ()
  "Seeks to +5%"
  (do-shell "mpc seek +5%")
  (mpd-menu))

(defun mpd-seek--5% ()
  "Seeks to -5%"
  (do-shell "mpc seek -5%")
  (mpd-menu))

(defun show-mpd-playlist ()
  "Show the current MPD playlist"
  (info-on-shell "Current MPD playlist:" "mpc playlist"))

(defun mpd-menu ()
  "< Open the MPD menu >"
  (info-mode-menu '((#\i show-mpd-info)
		    (#\p mpd-previous)
		    (#\n mpd-next)
		    (#\t mpd-toggle)
		    (#\y mpd-play)
		    (#\k mpd-stop)
		    (#\x mpd-seek-+5%)
		    (#\w mpd-seek--5%)
		    (#\l show-mpd-playlist)
		    (#\s start-sonata))))


(defun add-mpd-menu-to-help-menu ()
  (setf *help-menu-list* (append *help-menu-list*
				 `((#\s (mpd-menu ,*menu-color-submenu*))))))

(add-hook *init-hook* 'add-mpd-menu-to-help-menu)

(defun mpd-binding ()
  (define-main-key ("F2" :alt) 'mpd-menu))

(add-hook *binding-hook* 'mpd-binding)



(format t "done~%")
