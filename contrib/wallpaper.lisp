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
;;;  (defun my-wallpaper ()
;;;    (wallpaper "/home/you/.background-full" nil
;;;               "background-1.png"
;;;               "background-2.png"))
;;;
;;;  (add-hook *init-hook* 'my-wallpaper)
;;;
;;; You can have more screen heads than wallpaper images listed in the
;;; wallpaper function.
;;;
;;; You can force the wallpaper creation by replacing the nil value after the
;;; wallpaper basename with a true (t) value.
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
    (do-shell-output "~A" command)))


(defun create-wallpaper (filename &rest images)
  (format t "Creating wallpaper ~A from ~{~A ~}~%" filename images)
  (generate-wallpaper filename (x-drawable-width *root*) (x-drawable-height *root*)
                      (or (get-connected-heads-size)
                          `((0 0 ,(x-drawable-width *root*) ,(x-drawable-height *root*))))
                      images)
  (format t "Done.~%"))


(defun use-wallpaper (filename)
  (when (probe-file filename)
    (format t "Using wallpaper ~A~%" filename)
    (do-shell (format nil "~A ~A" *wallpaper-command* filename) nil t)
    (format t "Done.~%")))



(defun wallpaper-name (basename)
  (let ((sizes (or (get-connected-heads-size)
                   `((0 0 ,(x-drawable-width *root*) ,(x-drawable-height *root*)))))
        (count 0))
    (dolist (s sizes)
      (dolist (v s)
        (incf count (+ v count))))
    (format nil "~A-~A.png" basename count)))

(defun wallpaper (basename force-create &rest images)
  (let* ((filename (wallpaper-name basename)))
    (when (or force-create (not (probe-file filename)))
      (open-notify-window '(" " " " "          Please wait. Updating wallpaper...        " " " " "))
      (apply #'create-wallpaper filename images)
      (close-notify-window))
    (use-wallpaper filename)))

;;;
;;; End of code
;;;
(format t "done~%")


