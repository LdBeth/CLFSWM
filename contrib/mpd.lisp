;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Music Player Daemon (MPD) interface
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
;;; Documentation: If you want to use this file, just add this line in
;;; your configuration file:
;;;
;;;   (load-contrib "mpd.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading MPD code... ")


(defun mpd-menu ()
  "Open the Music Player Daemon (MPD) menu"
  (open-menu (find-menu 'mpd-menu)))


(defun start-sonata ()
  "Start sonata"
  (do-shell "exec sonata"))

(defun start-gmpc ()
  "Start gmpc"
  (do-shell "exec gmpc"))


(defun show-mpd-info ()
  "Show MPD informations"
  (info-on-shell "MPD informations:" "mpc")
  (mpd-menu))

(defun mpd-previous (&optional (in-menu t))
  "Play the previous song in the current playlist"
  (if in-menu
      (progn
        (info-on-shell "MPD:" "mpc prev")
        (mpd-menu))
      (do-shell "mpc prev" nil t)))

(defun mpd-next (&optional (in-menu t))
  "Play the next song in the current playlist"
  (if in-menu
      (progn
        (info-on-shell "MPD:" "mpc next")
        (mpd-menu))
      (do-shell "mpc next" nil t)))

(defun mpd-toggle ()
  "Toggles Play/Pause, plays if stopped"
  (do-shell "mpc toggle"))

(defun mpd-play ()
  "Start playing"
  (do-shell "mpc play"))

(defun mpd-stop ()
  "Stop the currently playing playlists"
  (do-shell "mpc stop"))


(defun mpd-seek-+5% (&optional (in-menu t))
  "Seeks to +5%"
  (if in-menu
      (progn
        (do-shell "mpc seek +5%")
        (mpd-menu))
      (do-shell "mpc seek +5%" nil t)))

(defun mpd-seek--5% (&optional (in-menu t))
  "Seeks to -5%"
  (if in-menu
      (progn
        (do-shell "mpc seek -5%")
        (mpd-menu))
      (do-shell "mpc seek -5%" nil t)))

(defun show-mpd-playlist ()
  "Show the current MPD playlist"
  (info-on-shell "Current MPD playlist:" "mpc playlist")
  (mpd-menu))

(unless (find-menu 'mpd-menu)
  (add-sub-menu 'help-menu "F2" 'mpd-menu "Music Player Daemon (MPD) menu")

  (add-menu-key 'mpd-menu "i" 'show-mpd-info)
  (add-menu-key 'mpd-menu "p" 'mpd-previous)
  (add-menu-key 'mpd-menu "n" 'mpd-next)
  (add-menu-key 'mpd-menu "t" 'mpd-toggle)
  (add-menu-key 'mpd-menu "y" 'mpd-play)
  (add-menu-key 'mpd-menu "k" 'mpd-stop)
  (add-menu-key 'mpd-menu "x" 'mpd-seek-+5%)
  (add-menu-key 'mpd-menu "w" 'mpd-seek--5%)
  (add-menu-key 'mpd-menu "l" 'show-mpd-playlist)
  (add-menu-key 'mpd-menu "s" 'start-sonata)
  (add-menu-key 'mpd-menu "g" 'start-gmpc))


(defun mpd-binding ()
  (define-main-key ("F2" :alt) 'mpd-menu))

(add-hook *binding-hook* 'mpd-binding)



#+:clfswm-toolbar
(progn
  (defconfig *mpd-toolbar* '((mpd-buttons 1)
                             (mpd-info 60))
    'Toolbar "MPD toolbar modules")

  (defconfig *mpd-toolbar-client* "gmpc"
    'Toolbar "MPD client")

  (define-toolbar-color mpd-info "MPD - Music Player Daemon information color")
  (define-toolbar-color mpd-buttons "MPD - Music Player Daemon buttons color")

  (define-toolbar-module (mpd-info small)
    "(small) - MPD (Music Player Daemon) informations"
    (let* ((lines (do-shell "mpc" nil t))
           (mpd-line (loop for line = (read-line lines nil nil)
                        while line
                        collect line)))
      (if (>= (length mpd-line) 3)
          (if small
              (toolbar-module-text toolbar module (tb-color mpd-info)
                               "~A"
                               (ensure-printable (first mpd-line)))
              (toolbar-module-text toolbar module (tb-color mpd-info)
                                   "~A - ~A"
                                   (ensure-printable (first mpd-line))
                                   (ensure-printable (second mpd-line))))
          (toolbar-module-text toolbar module (tb-color mpd-info)
                               "MPD - Not playing"))))

  (define-toolbar-module (mpd-buttons small)
    "(small) - MPD (Music Player Daemon) buttons"
    (with-set-toolbar-module-rectangle (module)
      (toolbar-module-text toolbar module (tb-color mpd-buttons)
                           (if small
                               "PNT<>C"
                               "P N T < > C"))))

  (define-toolbar-module-click (mpd-buttons small)
    "P=Previous, N=Next, T=Toogle, <=seek-5% >=seek+5% C=start MPD client"
    (declare (ignore state small))
    (when (= code 1)
      (let ((pos (toolbar-module-subdiv toolbar module root-x root-y 6)))
        (case pos
          (0 (mpd-previous nil))
          (1 (mpd-next nil))
          (2 (mpd-toggle))
          (3 (mpd-seek--5% nil))
          (4 (mpd-seek-+5% nil))
          (5 (do-shell *mpd-toolbar-client*))))
      (refresh-toolbar toolbar))))


(format t "done~%")
