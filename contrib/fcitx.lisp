(in-package :clfswm)

(format t "Loading fcitx code...")

(defconfig *fcitx-remote-command* "fcitx-remote"
  'fcitx "Command for fcitx remote program")

(defun fcitx-status ()
  (let ((code (car (do-shell-output *fcitx-remote-command*))))
    (if (or (string-equal "0" code) nil t))))

(defun fcitx-toggle ()
  "Toogle fxitx"
  (do-shell (concatenate 'string *fcitx-remote-command*
                         (if (fcitx-status) "-c" "-o"))))

(defun set-fcitx-keys ()
  (define-main-key ("Shift" :mod-4) 'fcitx-toggle))

(add-hook *binding-hook* 'set-fcitx-keys)

(format t "done~%")
