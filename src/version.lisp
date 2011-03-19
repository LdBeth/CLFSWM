;; Copyright (C) 2011 Xavier Maillard <xma@gnu.org>
;; Copyright (C) 2011 Martin Bishop
;;
;;  Borrowed from Stumpwm
;;  This file is part of clfswm.
;;
;; clfswm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; clfswm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; This file contains version information.
;;
;; Code:

(in-package :common-lisp-user)

(defpackage version
  (:use :common-lisp :tools)
   (:export *version*))

(in-package :version)

(defparameter *version* #.(concatenate 'string "Version: 1103   built " (date-string)))
