;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Define some keybindings for an azerty french keyboard
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
;;; Documentation: French keyboard layout.
;;;   If you want to use this file, just add this line in
;;; your configuration file:
;;;
;;;   (load-contrib "keyb_fr.lisp")
;;;
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading French Keyboard code... ")

(defun fr-binding ()
  ;; For an azery keyboard:
  ;; Main mode
  (undefine-main-multi-keys ("1" :mod-1) ("2" :mod-1) ("3" :mod-1)
			    ("4" :mod-1) ("5" :mod-1) ("6" :mod-1)
			    ("7" :mod-1) ("8" :mod-1) ("9" :mod-1) ("0" :mod-1))
  (define-main-key ("twosuperior") 'banish-pointer)
  (define-main-key ("ampersand" :mod-1) 'bind-or-jump 1)
  (define-main-key ("eacute" :mod-1) 'bind-or-jump 2)
  (define-main-key ("quotedbl" :mod-1) 'bind-or-jump 3)
  (define-main-key ("quoteright" :mod-1) 'bind-or-jump 4)
  (define-main-key ("parenleft" :mod-1) 'bind-or-jump 5)
  (define-main-key ("minus" :mod-1) 'bind-or-jump 6)
  (define-main-key ("egrave" :mod-1) 'bind-or-jump 7)
  (define-main-key ("underscore" :mod-1) 'bind-or-jump 8)
  (define-main-key ("ccedilla" :mod-1) 'bind-or-jump 9)
  (define-main-key ("agrave" :mod-1) 'bind-or-jump 10)
  ;; Second mode
  (undefine-second-multi-keys ("1" :mod-1) ("2" :mod-1) ("3" :mod-1)
			      ("4" :mod-1) ("5" :mod-1) ("6" :mod-1)
			      ("7" :mod-1) ("8" :mod-1) ("9" :mod-1) ("0" :mod-1))
  (define-second-key ("twosuperior") 'banish-pointer)
  (define-second-key ("ampersand" :mod-1) 'bind-or-jump 1)
  (define-second-key ("eacute" :mod-1) 'bind-or-jump 2)
  (define-second-key ("quotedbl" :mod-1) 'bind-or-jump 3)
  (define-second-key ("quoteright" :mod-1) 'bind-or-jump 4)
  (define-second-key ("parenleft" :mod-1) 'bind-or-jump 5)
  (define-second-key ("minus" :mod-1) 'bind-or-jump 6)
  (define-second-key ("egrave" :mod-1) 'bind-or-jump 7)
  (define-second-key ("underscore" :mod-1) 'bind-or-jump 8)
  (define-second-key ("ccedilla" :mod-1) 'bind-or-jump 9)
  (define-second-key ("agrave" :mod-1) 'bind-or-jump 10))

(unless (member 'fr-binding *binding-hook*)
  (add-hook *binding-hook* 'fr-binding))

(format t "done~%")
