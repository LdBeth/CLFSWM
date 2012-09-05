;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Wallpaper utilities
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2012 Philippe Brochard <pbrochard@common-lisp.net>
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
;;;   (load-contrib "wallpaper.lisp")
;;;
;;; Note: You need the 'convert' program from the ImageMagick package and the
;;;   'Esetroot' program. But you can change this last one.
;;;
;;; Usage example:
;;;
;;;  (defun create-my-wallpaper ()
;;;    (create-wallpaper "/home/you/.background-full.png"
;;;                      "/home/you/Lisp/test/test-create-background/Tux_Wallpaper_by_Narcoblix.png"
;;;                      "/home/you/Lisp/test/test-create-background/background.png"))
;;;
;;;  (defun use-my-wallpaper ()
;;;    (use-wallpaper "/home/you/.background-full.png"))
;;;
;;;
;;;  (add-hook *init-hook* 'create-my-wallpaper 'use-my-wallpaper)
;;;
;;; When you're happy with your background, you can remove the creation part.
;;;
;;;  (add-hook *init-hook* 'use-my-wallpaper)
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading Wallpaper code... ")


(defconfig *wallpaper-command* "Esetroot -scale"
  'Wallpaper "Command to install the wallpaper")

;;; Example of generated line
;;; convert -size 1000x1000 xc:skyblue background.png -geometry 700x600+150+10! -composite  Tux_Wallpaper_by_Narcoblix.png -geometry 500x300+100+620!  -composite composite.png

(defun generate-wallpaper (filename width height root-list image-filename-list &optional (background "black"))
  (let ((command (with-output-to-string (str)
                   (format str "convert -size ~Ax~A xc:~A " width height background)
                   (let ((ind 0)
                         (len (1- (length image-filename-list))))
                     (dolist (root root-list)
                       (format str "~A -geometry ~Ax~A+~A+~A! -composite " (nth ind image-filename-list)
                               (third root) (fourth root) (first root) (second root))
                       (setf ind (if (< ind len) (1+ ind) 0))))
                   (format str "~A" filename))))
    (format t "~A~%" command)
    (do-shell command nil t)))

(defun create-wallpaper (filename &rest images)
  (format t "Creating wallpaper ~A from ~{~A ~}~%" filename images)
  (generate-wallpaper filename (xlib:screen-width *screen*) (xlib:screen-height *screen*)
                      (get-connected-heads-size) images)
  (format t "Done.~%"))


(defun use-wallpaper (filename)
  (when (probe-file filename)
    (format t "Using wallpaper ~A~%" filename)
    (do-shell (format nil "~A ~A" *wallpaper-command* filename) nil t)
    (format t "Done.~%")))

;;;
;;; End of code
;;;
(format t "done~%")


