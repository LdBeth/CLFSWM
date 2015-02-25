;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: MOC - Console audio player - interface
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
;;;   (load-contrib "moc.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading MOC code... ")


(defun moc-menu ()
  "Open the MOC menu"
  (open-menu (find-menu 'moc-menu)))


(defun start-mocp ()
  "Start mocp"
  (do-shell "xterm -e 'mocp 2> /dev/null'"))


(defun show-moc-info ()
  "Show MOC informations"
  (info-on-shell "MOC informations:" "mocp --info")
  (moc-menu))

(defun moc-previous (&optional (in-menu t))
  "Play the previous song in the current playlist"
  (do-shell "mocp --previous" nil t)
  (when in-menu
    (moc-menu)))

(defun moc-next (&optional (in-menu t))
  "Play the next song in the current playlist"
  (do-shell "mocp --next" nil t)
  (when in-menu
    (moc-menu)))

(defun moc-toggle ()
  "Toggles Play/Pause, plays if stopped"
  (do-shell "mocp --toggle-pause"))

(defun moc-play ()
  "Start playing"
  (do-shell "mocp --play"))

(defun moc-stop ()
  "Stop the currently playing playlists"
  (do-shell "mocp --stop"))


(defun moc-seek-+5s (&optional (in-menu t))
  "Seeks to +5s"
  (if in-menu
      (progn
        (do-shell "mocp --seek +5")
        (moc-menu))
      (do-shell "mocp --seek +5" nil t)))

(defun moc-seek--5s (&optional (in-menu t))
  "Seeks to -5s"
  (if in-menu
      (progn
        (do-shell "mocp --seek -5")
        (moc-menu))
      (do-shell "mocp --seek -5" nil t)))

(unless (find-menu 'moc-menu)
  (add-sub-menu 'help-menu "F3" 'moc-menu "MOC - Console audio player - menu")

  (add-menu-key 'moc-menu "i" 'show-moc-info)
  (add-menu-key 'moc-menu "p" 'moc-previous)
  (add-menu-key 'moc-menu "n" 'moc-next)
  (add-menu-key 'moc-menu "t" 'moc-toggle)
  (add-menu-key 'moc-menu "y" 'moc-play)
  (add-menu-key 'moc-menu "k" 'moc-stop)
  (add-menu-key 'moc-menu "x" 'moc-seek-+5s)
  (add-menu-key 'moc-menu "w" 'moc-seek--5s)
  (add-menu-key 'moc-menu "m" 'start-mocp))


(defun moc-binding ()
  (define-main-key ("F3" :alt) 'moc-menu))

(add-hook *binding-hook* 'moc-binding)

(format t "done~%")
