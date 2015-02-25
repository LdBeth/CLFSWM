;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Toolbar
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
;;;   (load-contrib "toolbar.lisp")
;;;
;;; You can add a toolbar with the function add-toolbar in your configuration
;;; file. You can obtain modules list with the list-toolbar-modules function.
;;;
;;; For convenience, here is the add-toolbar documentation:
;;;
;;;  add-toolbar (root-x root-y direction size placement modules
;;;                      &key (autohide *toolbar-default-autohide*)
;;;                      (thickness *toolbar-default-thickness*)
;;;                      (refresh-delay *toolbar-default-refresh-delay*)
;;;                      (border-size *toolbar-default-border-size*))
;;;    "Add a new toolbar.
;;;       root-x, root-y: root coordinates or if root-y is nil, root-x is the nth root in root-list.
;;;       direction: one of :horiz or :vert
;;;       placement: same argument as with-placement macro
;;;       modules: list of modules: a list of module name, position in percent and arguments.
;;;                0%=left/up   <->   100%=right/down.
;;;                Example: '((clock 1) (label 50 \"My label\") (clickable-clock 90))
;;;       size: toolbar size in percent of root size
;;;       thickness: toolbar height for horizontal toolbar or width for vertical one
;;;       autohide: one of nil, :click, or :motion
;;;       refresh-delay: refresh delay for toolbar in seconds
;;;       border-size: toolbar window border size"
;;;
;;; Here are some examples:
;;;    (load-contrib "toolbar.lisp")
;;;
;;;  ;; Add an horizontal toolbar on root at coordinates 0,0 pixels
;;;  ;; with default modules
;;;
;;;    (add-toolbar 0 0 :horiz 80 'top-middle-root-placement *default-toolbar*)
;;;
;;;
;;;  ;; Add an horizontal toolbar on root at coordinates 0,0 pixels
;;;
;;;    (add-toolbar 0 0 :horiz 90 'top-middle-root-placement
;;;                 '((clock 1) (label 50 "Plop") (clock-second 25) (clickable-clock 99))
;;;                 :autohide :click
;;;                 :refresh-delay 1)
;;;
;;;
;;;  ;; Add an horizontal toolbar on root at coordinates 0,0 pixels
;;;
;;;    (add-toolbar 0 0 :horiz 70 'bottom-middle-root-placement '((clock 1) (label 50 "Paf) (clock 99))
;;;                 :autohide :motion)
;;;
;;;
;;;  ;; Add a vertical toolbar on root 0
;;;
;;;    (add-toolbar 0 nil :vert 60 'middle-left-root-placement '((clock 1) (label 50 "My label") (clock 90)))
;;;
;;;
;;;  ;; Add a vertical toolbar on root 1
;;;
;;;    (add-toolbar 1 nil :vert 70 'bottom-right-root-placement '((clock 1) (label 50) (clickable-clock 99)))
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(format t "Loading Toolbar code... ")

(pushnew :clfswm-toolbar *features*)

(defstruct toolbar root-x root-y root direction size thickness placement refresh-delay
           autohide modules clickable hide-state font window gc border-size
           exposure-hook button-press-hook motion-notify-hook leave-notify-hook)

(defstruct toolbar-module name pos display-fun click-fun args rect)

(defparameter *toolbar-list* nil)
(defparameter *toolbar-module-list* nil)

(defparameter *toolbar-root-usage* nil)

(defconfig *default-toolbar* '((clfswm-menu 1)
                               (expose-mode-button 10)
                               (system-usage 90)
                               (clickable-clock 99))
  'Toolbar "Default toolbar modules")


;;; CONFIG - Toolbar window string colors
(defconfig *toolbar-window-font-string* *default-font-string*
  'Toolbar "Toolbar window font string")
(defconfig *toolbar-window-background* "black"
  'Toolbar "Toolbar Window background color")
(defconfig *toolbar-window-foreground* "green"
  'Toolbar "Toolbar Window foreground color")
(defconfig *toolbar-window-border* "red"
  'Toolbar "Toolbar Window border color")
(defconfig *toolbar-default-border-size* 0
  'Toolbar "Toolbar Window border size")
(defconfig *toolbar-window-transparency* *default-transparency*
  'Toolbar "Toolbar window background transparency")
(defconfig *toolbar-default-thickness* 20
  'Toolbar "Toolbar default thickness")
(defconfig *toolbar-default-refresh-delay* 30
  'Toolbar "Toolbar default refresh delay")
(defconfig *toolbar-default-autohide* nil
  'Toolbar "Toolbar default autohide value")
(defconfig *toolbar-sensibility* 3
  'Toolbar "Toolbar sensibility in pixels")

(defconfig *toolbar-window-placement* 'top-left-placement
  'Placement "Toolbar window placement")

(use-event-hook :exposure)
(use-event-hook :button-press)
(use-event-hook :motion-notify)
(use-event-hook :leave-notify)

(defun toolbar-symbol-fun (name &optional (type 'display))
  (create-symbol-in-package :clfswm 'toolbar- name '-module- type))

(defmacro with-toolbar-root-usage ((root place) &body body)
  "Apply body only if root place is not already used"
  `(unless (member ,place (gethash ,root *toolbar-root-usage*))
     ,@body
     (pushnew ,place (gethash ,root *toolbar-root-usage*))))

(defun toolbar-adjust-root-size (toolbar &optional (dir +1))
  (unless (toolbar-autohide toolbar)
    (let ((root (toolbar-root toolbar))
          (placement-name (symbol-name (toolbar-placement toolbar)))
          (thickness (+ (toolbar-thickness toolbar) (* 2 (toolbar-border-size toolbar)))))
      (when (root-p root)
        (case (toolbar-direction toolbar)
          (:horiz (cond ((search "TOP" placement-name)
                         (with-toolbar-root-usage (root :top)
                           (incf (root-y root) (* thickness dir))
                           (decf (root-h root) (* thickness dir))))
                        ((search "BOTTOM" placement-name)
                         (with-toolbar-root-usage (root :bottom)
                           (decf (root-h root) (* thickness dir))))))
          (t (cond ((search "LEFT" placement-name)
                    (with-toolbar-root-usage (root :left)
                      (incf (root-x root) (* thickness dir))
                      (decf (root-w root) (* thickness dir))))
                   ((search "RIGHT" placement-name)
                    (with-toolbar-root-usage (root :right)
                      (decf (root-w root) (* thickness dir)))))))))))


(defun toolbar-draw-text (toolbar pos1 pos2 text color)
  "pos1: percent of toolbar, pos2: pixels in toolbar"
  (labels ((horiz-text ()
             (let* ((height (- (xlib:font-ascent (toolbar-font toolbar)) (xlib:font-descent (toolbar-font toolbar))))
                    (dy (truncate (+ pos2 (/ height 2))))
                    (width (xlib:text-width (toolbar-font toolbar) text))
                    (pos (truncate (/ (* (- (xlib:drawable-width (toolbar-window toolbar)) width) pos1) 100))))
               (xlib:draw-glyphs *pixmap-buffer* (toolbar-gc toolbar) pos dy text)
               (values (+ pos (xlib:drawable-x (toolbar-window toolbar)))
                       (xlib:drawable-y (toolbar-window toolbar))
                       width
                       (xlib:drawable-height (toolbar-window toolbar)))))
           (vert-text ()
             (let* ((width (xlib:max-char-width (toolbar-font toolbar)))
                    (dx (truncate (- pos2 (/ width 2))))
                    (dpos (xlib:max-char-ascent (toolbar-font toolbar)))
                    (height (* dpos (length text)))
                    (pos (+ (truncate (/ (* (- (xlib:drawable-height (toolbar-window toolbar)) height
                                               (xlib:max-char-descent (toolbar-font toolbar)))
                                            pos1) 100))
                            (xlib:font-ascent (toolbar-font toolbar)))))
               (loop for c across text
                    for i from 0
                  do (xlib:draw-glyphs *pixmap-buffer* (toolbar-gc toolbar) dx (+ pos (* i dpos)) (string c)))
               (values (xlib:drawable-x (toolbar-window toolbar))
                       (+ (- pos dpos) (xlib:drawable-y (toolbar-window toolbar)))
                       (xlib:drawable-width (toolbar-window toolbar))
                       height))))
    (xlib:with-gcontext ((toolbar-gc toolbar) :foreground (get-color color))
      (case (toolbar-direction toolbar)
        (:horiz (horiz-text))
        (:vert (vert-text))))))


(defun toolbar-module-text (toolbar module color formatter &rest text)
  "Print a formatted text at module position centered in toolbar"
  (toolbar-draw-text toolbar (toolbar-module-pos module) (/ *toolbar-default-thickness* 2)
                     (apply #'format nil formatter text)
                     color))

(defun is-valid-toolbar (toolbar)
  (member toolbar *toolbar-list*))


(defun refresh-toolbar (toolbar)
  (when (is-valid-toolbar toolbar)
    (unless (toolbar-hide-state toolbar)
      (add-timer (toolbar-refresh-delay toolbar)
                 (lambda ()
                   (refresh-toolbar toolbar))
                 :refresh-toolbar)
      (clear-pixmap-buffer (toolbar-window toolbar) (toolbar-gc toolbar))
      (dolist (module (toolbar-modules toolbar))
        (when (fboundp (toolbar-module-display-fun module))
          (apply (toolbar-module-display-fun module) toolbar module (toolbar-module-args module))))
      (copy-pixmap-buffer (toolbar-window toolbar) (toolbar-gc toolbar)))))

(defun toolbar-in-sensibility-zone-p (toolbar root-x root-y)
  (let* ((tb-win (toolbar-window toolbar))
         (win-x (xlib:drawable-x tb-win))
         (win-y (xlib:drawable-y tb-win))
         (width (xlib:drawable-width tb-win))
         (height (xlib:drawable-height tb-win))
         (tb-dir (toolbar-direction toolbar) )
         (placement-name (symbol-name (toolbar-placement toolbar))))
    (or (and (equal tb-dir :horiz) (search "TOP" placement-name)
             (<= root-y win-y (+ root-y *toolbar-sensibility*))
             (<= win-x root-x (+ win-x width)) (toolbar-autohide toolbar))
        (and (equal tb-dir :horiz) (search "BOTTOM" placement-name)
             (<= (+ win-y height (- *toolbar-sensibility*)) root-y (+ win-y height))
             (<= win-x root-x (+ win-x width)) (toolbar-autohide toolbar))
        (and (equal tb-dir :vert) (search "LEFT" placement-name)
             (<= root-x win-x (+ root-x *toolbar-sensibility*))
             (<= win-y root-y (+ win-y height)) (toolbar-autohide toolbar))
        (and (equal tb-dir :vert) (search "RIGHT" placement-name)
             (<= (+ win-x width (- *toolbar-sensibility*)) root-x (+ win-x width))
             (<= win-y root-y (+ win-y height)) (toolbar-autohide toolbar)))))


(defun toolbar-add-exposure-hook (toolbar)
  (push (define-event-hook :exposure (window)
          (when (and (is-valid-toolbar toolbar)
                     (xlib:window-p window)
                     (xlib:window-equal (toolbar-window toolbar) window))
            (refresh-toolbar toolbar)))
        (toolbar-exposure-hook toolbar)))


(defun toggle-toolbar-hide-state (toolbar)
  (let* ((tb-win (toolbar-window toolbar)))
    (if (toolbar-hide-state toolbar)
        (progn
          (setf (toolbar-hide-state toolbar) nil)
          (map-window tb-win)
          (raise-window tb-win)
          (refresh-toolbar toolbar))
        (progn
          (hide-window tb-win)
          (setf (toolbar-hide-state toolbar) t)))))

(defun toolbar-add-hide-button-press-hook (toolbar)
  (push (define-event-hook :button-press (code root-x root-y)
          (when (and (is-valid-toolbar toolbar) (= code 1)
                     (toolbar-in-sensibility-zone-p toolbar root-x root-y))
            (toggle-toolbar-hide-state toolbar)
            (wait-mouse-button-release)
            (stop-button-event)
            (exit-handle-event)))
        (toolbar-button-press-hook toolbar)))

(defun toolbar-add-hide-motion-hook (toolbar)
  (push (define-event-hook :motion-notify (root-x root-y)
          (unless (compress-motion-notify)
            (when (and (is-valid-toolbar toolbar)
                       (toolbar-hide-state toolbar)
                       (toolbar-in-sensibility-zone-p toolbar root-x root-y))
              (map-window (toolbar-window toolbar))
              (raise-window (toolbar-window toolbar))
              (refresh-toolbar toolbar)
              (setf (toolbar-hide-state toolbar) nil)
              (exit-handle-event))))
         (toolbar-motion-notify-hook toolbar)))

(defun toolbar-add-hide-leave-hook (toolbar)
  (push (define-event-hook :leave-notify (root-x root-y)
          (when (and (is-valid-toolbar toolbar)
                     (not (toolbar-hide-state toolbar))
                     (not (in-window (toolbar-window toolbar) root-x root-y)))
            (hide-window (toolbar-window toolbar))
            (setf (toolbar-hide-state toolbar) t)
            (exit-handle-event)))
         (toolbar-leave-notify-hook toolbar)))


(defun toolbar-add-clickable-module-hook (toolbar)
  (push (define-event-hook :button-press (code state root-x root-y)
          (when (and (is-valid-toolbar toolbar)
                     (in-window (toolbar-window toolbar) root-x root-y)
                     (not (toolbar-hide-state toolbar)))
            (dolist (module (toolbar-modules toolbar))
              (when (and (in-rectangle root-x root-y (toolbar-module-rect module))
                         (fboundp (toolbar-module-click-fun module)))
                (apply (toolbar-module-click-fun module) toolbar module code state root-x root-y
                       (toolbar-module-args module))
                (stop-button-event)
                (exit-handle-event)))))
          (toolbar-button-press-hook toolbar)))


(defun define-toolbar-hooks (toolbar)
  (toolbar-add-exposure-hook toolbar)
  (when (toolbar-clickable toolbar)
    (toolbar-add-clickable-module-hook toolbar))
  (case (toolbar-autohide toolbar)
    (:click (toolbar-add-hide-button-press-hook toolbar))
    (:motion (toolbar-add-hide-motion-hook toolbar)
             (toolbar-add-hide-leave-hook toolbar))))

(defun set-clickable-toolbar (toolbar)
  (dolist (module (toolbar-modules toolbar))
    (when (fboundp (toolbar-module-click-fun module))
      (setf (toolbar-clickable toolbar) t))))


(defmacro remove-toolbar-hook (toolbar keyword)
  (let ((fun (create-symbol 'toolbar- keyword '-hook)))
    `(dolist (hook (,fun ,toolbar))
       (remove-event-hook ,keyword hook))))



(let ((windows-list nil))
  (defun is-toolbar-window-p (win)
    (and (xlib:window-p win) (member win windows-list :test 'xlib:window-equal)))

  (defun close-toolbar (toolbar)
    (when (toolbar-p toolbar)
      (erase-timer :refresh-toolbar-window)
      (remove-toolbar-hook toolbar :exposure)
      (remove-toolbar-hook toolbar :button-press)
      (remove-toolbar-hook toolbar :leave-notify)
      (remove-toolbar-hook toolbar :motion-notify)
      (setf *never-managed-window-list*
            (remove (list #'is-toolbar-window-p nil)
                    *never-managed-window-list* :test #'equal))
      (awhen (toolbar-gc toolbar)
        (xlib:free-gcontext it))
      (awhen (toolbar-window toolbar)
        (xlib:destroy-window it))
      (awhen (toolbar-font toolbar)
        (xlib:close-font it))
      (xlib:display-finish-output *display*)
      (setf (toolbar-window toolbar) nil
            (toolbar-gc toolbar) nil
            (toolbar-font toolbar) nil)))

  (defun open-toolbar (toolbar)
    (let ((root (root (toolbar-root-x toolbar) (toolbar-root-y toolbar))))
      (when (root-p root)
        (setf (toolbar-root toolbar) root)
        (let ((*get-current-root-fun* (lambda () root)))
          (setf (toolbar-font toolbar) (xlib:open-font *display* *toolbar-window-font-string*))
          (let* ((width (if (equal (toolbar-direction toolbar) :horiz)
                            (round (/ (* (root-w root) (toolbar-size toolbar)) 100))
                            (toolbar-thickness toolbar)))
                 (height (if (equal (toolbar-direction toolbar) :horiz)
                             (toolbar-thickness toolbar)
                             (round (/ (* (root-h root) (toolbar-size toolbar)) 100)))))
            (with-placement ((toolbar-placement toolbar) x y width height (toolbar-border-size toolbar))
              (setf (toolbar-window toolbar) (xlib:create-window :parent *root*
                                                                 :x x
                                                                 :y y
                                                                 :width width
                                                                 :height height
                                                                 :background (get-color *toolbar-window-background*)
                                                                 :border-width (toolbar-border-size toolbar)
                                                                 :border (when (plusp (toolbar-border-size toolbar))
                                                                           (get-color *toolbar-window-border*))
                                                                 :colormap (xlib:screen-default-colormap *screen*)
                                                                 :event-mask '(:exposure :key-press :leave-window
                                                                               :pointer-motion))
                    (toolbar-gc toolbar) (xlib:create-gcontext :drawable (toolbar-window toolbar)
                                                               :foreground (get-color *toolbar-window-foreground*)
                                                               :background (get-color *toolbar-window-background*)
                                                               :font (toolbar-font toolbar)
                                                               :line-style :solid))
              (push (toolbar-window toolbar) windows-list)
              (setf (window-transparency (toolbar-window toolbar)) *toolbar-window-transparency*)
              (add-in-never-managed-window-list (list 'is-toolbar-window-p nil))
              (map-window (toolbar-window toolbar))
              (raise-window (toolbar-window toolbar))
              (refresh-toolbar toolbar)
              (when (toolbar-autohide toolbar)
                (hide-window (toolbar-window toolbar))
                (setf (toolbar-hide-state toolbar) t))
              (xlib:display-finish-output *display*)
              (set-clickable-toolbar toolbar)
              (define-toolbar-hooks toolbar))))))))



(defun remove-toolbar (toolbar)
  (close-toolbar toolbar)
  (setf *toolbar-list* (remove toolbar *toolbar-list* :test #'equal)))


(defun open-all-toolbars ()
  "Open all toolbars"
  (setf *toolbar-root-usage* (make-hash-table :test #'equal))
  (dolist (toolbar *toolbar-list*)
    (open-toolbar toolbar))
  (dolist (toolbar *toolbar-list*)
    (toolbar-adjust-root-size toolbar)))

(defun close-all-toolbars ()
  (setf *toolbar-root-usage* (make-hash-table :test #'equal))
  (dolist (toolbar *toolbar-list*)
    (toolbar-adjust-root-size toolbar -1))
  (dolist (toolbar *toolbar-list*)
    (remove-toolbar toolbar))
  (stop-system-poll))

(defun create-toolbar-modules (modules)
  (loop for mod in modules
     collect (make-toolbar-module :name (first mod)
                                  :pos (second mod)
                                  :display-fun (toolbar-symbol-fun (first mod))
                                  :click-fun (toolbar-symbol-fun (first mod) 'click)
                                  :args (cddr mod)
                                  :rect nil)))


(defun add-toolbar (root-x root-y direction size placement modules
                    &key (autohide *toolbar-default-autohide*)
                    (thickness *toolbar-default-thickness*)
                    (refresh-delay *toolbar-default-refresh-delay*)
                    (border-size *toolbar-default-border-size*))
  "Add a new toolbar.
     root-x, root-y: root coordinates or if root-y is nil, root-x is the nth root in root-list.
     direction: one of :horiz or :vert
     placement: same argument as with-placement macro
     modules: list of modules: a list of module name, position in percent and arguments.
              0%=left/up   <->   100%=right/down.
              Example: '((clock 1) (label 50 \"My label\") (clickable-clock 90))
     size: toolbar size in percent of root size
     thickness: toolbar height for horizontal toolbar or width for vertical one
     autohide: one of nil, :click, or :motion
     refresh-delay: refresh delay for toolbar in seconds
     border-size: toolbar window border size"
  (let ((toolbar (make-toolbar :root-x root-x :root-y root-y
                               :direction direction :size size
                               :thickness thickness
                               :placement placement
                               :autohide autohide
                               :refresh-delay refresh-delay
                               :border-size border-size
                               :modules (create-toolbar-modules modules))))
    (push toolbar *toolbar-list*)
    toolbar))


(add-hook *init-hook* 'open-all-toolbars)
(add-hook *close-hook* 'close-all-toolbars)


(defun set-toolbar-module-rectangle (module x y width height)
  (unless (toolbar-module-rect module)
    (setf (toolbar-module-rect module) (make-rectangle)))
  (setf (rectangle-x (toolbar-module-rect module)) x
        (rectangle-y (toolbar-module-rect module)) y
        (rectangle-width (toolbar-module-rect module)) width
        (rectangle-height (toolbar-module-rect module)) height))

(defmacro with-set-toolbar-module-rectangle ((module) &body body)
  (let ((x (gensym)) (y (gensym)) (width (gensym)) (height (gensym)))
    `(multiple-value-bind (,x ,y ,width ,height)
         ,@body
       (set-toolbar-module-rectangle ,module ,x ,y ,width ,height))))



(defmacro define-toolbar-module ((name &rest args) &body body)
  (let ((symbol-fun (toolbar-symbol-fun name)))
    `(progn
       (pushnew ',name *toolbar-module-list*)
       (defun ,symbol-fun (toolbar module ,@(when args `(&optional ,@args)))
           ,@body))))

(defmacro define-toolbar-module-click ((name &rest args) &body body)
  (let ((symbol-fun (toolbar-symbol-fun name 'click)))
    `(progn
       (pushnew ',name *toolbar-module-list*)
       (defun ,symbol-fun (toolbar module code state root-x root-y ,@(when args `(&optional ,@args)))
           ,@body))))


(defun list-toolbar-modules (&optional (stream t))
  "List all toolbar modules"
  (format stream "Toolbar modules availables:~%")
  (dolist (module (reverse *toolbar-module-list*))
    (format stream "  Module: ~A~%" module)
    (when (fboundp (toolbar-symbol-fun module))
      (format stream "    ~A~%" (documentation (toolbar-symbol-fun module) 'function)))
    (when (fboundp (toolbar-symbol-fun module 'click))
      (format stream "    On click: ~A~%" (documentation (toolbar-symbol-fun module 'click) 'function)))))


(defmacro define-toolbar-color (name doc-string &optional (value *toolbar-window-foreground*))
  (let ((symbol-name (create-symbol '*toolbar- name '-color*)))
    `(defconfig ,symbol-name ,value 'Toolbar ,doc-string)))

(defmacro tb-color (name)
  (let ((symbol-name (create-symbol '*toolbar- name '-color*)))
    symbol-name))


;;;
;;; Module subdivisions functions
;;;
(defun toolbar-module-subdiv-horiz (module root-x N)
  (truncate (* N  (/ (- root-x (rectangle-x (toolbar-module-rect module)))
                     (rectangle-width (toolbar-module-rect module))))))

(defun toolbar-module-subdiv-vert (module root-y N)
  (truncate (* N  (/ (- root-y (rectangle-y (toolbar-module-rect module)))
                     (rectangle-height (toolbar-module-rect module))))))

(defun toolbar-module-subdiv (toolbar module root-x root-y N)
  (case (toolbar-direction toolbar)
    (:horiz (toolbar-module-subdiv-horiz module root-x N))
    (:vert (toolbar-module-subdiv-vert module root-y N))))


;;;
;;; Modules definitions
;;;

;;;
;;; Clock module
;;;
(define-toolbar-color clock "Clock color")

(define-toolbar-module (clock)
  "A clock module"
  (multiple-value-bind (s m h)
      (get-decoded-time)
    (declare (ignore s))
    (toolbar-module-text toolbar module (tb-color clock) "~2,'0D:~2,'0D" h m)))

;;;
;;; Clock module with seconds
;;;
(define-toolbar-module (clock-second)
  "A clock module with seconds"
  (multiple-value-bind (s m h)
      (get-decoded-time)
    (toolbar-module-text toolbar module (tb-color clock) "~2,'0D:~2,'0D:~2,'0D" h m s)))


;;;
;;; Label module
;;;
(define-toolbar-color label "Label color")

(define-toolbar-module (label text)
  "(text) - Display a text in toolbar"
  (toolbar-module-text toolbar module (tb-color label) (or text "Empty")))

;;;
;;; Clickable label module
;;;
(define-toolbar-color clickable-label "Clickable label color")

(define-toolbar-module (clickable-label text action)
  "(text action) - Display a clickable text in toolbar"
  (declare (ignore action))
  (with-set-toolbar-module-rectangle (module)
    (toolbar-module-text toolbar module (tb-color clickable-label) (or text "Empty"))))

(define-toolbar-module-click (clickable-label text action)
  "Call the function 'action'"
  (declare (ignore text root-x root-y))
  (when action
    (funcall action toolbar module code state )))

;;;
;;;  Clickable clock module
;;;
(define-toolbar-color clickable-clock "Clickable clock color")

(define-toolbar-module (clickable-clock)
  "A clickable clock module"
  (multiple-value-bind (s m h)
      (get-decoded-time)
    (declare (ignore s))
    (with-set-toolbar-module-rectangle (module)
      (toolbar-module-text toolbar module (tb-color clickable-clock) "~2,'0D:~2,'0D" h m))))


(defconfig *toolbar-clock-action* "xclock -analog"
  'toolbar "Toolbar clickable clock module action on click")

(define-toolbar-module-click (clickable-clock)
  "Start an external clock"
  (declare (ignore toolbar module state root-x root-y))
  (when (= code 1)
    (do-shell *toolbar-clock-action*)))


;;;
;;; CLFSWM menu module
;;;
(define-toolbar-color clfswm-menu "CLFSWM menu color")

(define-toolbar-module (clfswm-menu text placement)
  "(text placement) - Display an entry for the CLFSWM menu"
  (declare (ignore placement))
  (with-set-toolbar-module-rectangle (module)
    (toolbar-module-text toolbar module (tb-color clfswm-menu) (or text "CLFSWM"))))

(define-toolbar-module-click (clfswm-menu text placement)
  "Open the CLFSWM main menu"
  (declare (ignore text code state toolbar module root-x root-y))
  (let ((*info-mode-placement* (or placement *info-mode-placement*)))
    (open-menu)))

;;;
;;; CPU usage
;;;
(define-toolbar-color cpu "CPU color")

(define-toolbar-module (cpu)
  "Display the CPU usage (slow methode)"
  (toolbar-module-text toolbar module (tb-color cpu) "CPU:~A%" (cpu-usage)))


;;;
;;; Memory usage
;;;
(define-toolbar-color mem "Memory color")

(define-toolbar-module (mem)
  "Display the memory usage (slow methode)"
  (multiple-value-bind (used total)
      (memory-usage)
    (toolbar-module-text toolbar module (tb-color mem) "Mem:~A%" (round (* (/ used total) 100.0)))))



;;;
;;; Battery usage
;;;
(define-toolbar-color system-info "System information colors (CPU+Mem+Battery)")
(define-toolbar-color system-info-low "System information colors (CPU+Mem+Battery)" "Yellow")
(define-toolbar-color system-info-alert "System information colors (CPU+Mem+Battery)" "Magenta")
(define-toolbar-color system-info-urgent "System information colors (CPU+Mem+Battery)" "Red")

(defun toolbar-battery-color (bat)
  (if (numberp bat)
      (cond ((<= bat 5) (tb-color system-info-urgent))
            ((<= bat 10) (tb-color system-info-alert))
            ((<= bat 25) (tb-color system-info-low))
            (t (tb-color system-info)))
      (tb-color system-info)))

(define-toolbar-module (bat)
  "Display the battery usage (slow methode)"
  (let* ((bat (battery-usage)))
    (toolbar-module-text toolbar module
                         (toolbar-battery-color bat)
                         "Bat:~A%" bat)))



;;;
;;; System usage - Battery, CPU and Memory usage all in one
;;;
(define-toolbar-module (system-usage (poll-delay 10))
  "Display system usage: CPU, Memory and Battery (poll methode)"
  (multiple-value-bind (cpu used total bat)
      (system-usage-poll poll-delay)
    (toolbar-module-text toolbar module (toolbar-battery-color bat)
                         "Bat:~A% CPU:~A% Mem:~A%"
                         bat cpu
                         (round (* (/ used total) 100)))))

;;;
;;; CPU and Memory usage - CPU and Memory usage
;;;
(define-toolbar-module (system-cpu-mem (poll-delay 10))
  "Display system usage: CPU and Memory (poll methode)"
  (multiple-value-bind (cpu used total)
      (system-usage-poll poll-delay)
    (toolbar-module-text toolbar module (tb-color cpu)
                         "CPU:~A% Mem:~A%"
                         cpu
                         (round (* (/ used total) 100)))))

;;;
;;; Expose-mode-button
;;;
(define-toolbar-color expose-mode-button "Expose-mode button")

(define-toolbar-module (expose-mode-button text)
  "On click, switch to expose-mode"
  (with-set-toolbar-module-rectangle (module)
    (toolbar-module-text toolbar module (tb-color expose-mode-button) (or text "Xpo"))))

(define-toolbar-module-click (expose-mode-button)
  "left click=Show only current frames ; Right click=show all roots frames"
  (declare (ignore state toolbar module root-x root-y))
  (if (= code 1)
      (expose-windows-mode)
      (expose-all-windows-mode)))


;;;
;;; End of code
;;;
(format t "done~%")
