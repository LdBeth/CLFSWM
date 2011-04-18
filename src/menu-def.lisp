;;; --------------------------------------------------------------------------
;;; CLFSWM - FullScreen Window Manager
;;;
;;; --------------------------------------------------------------------------
;;; Documentation: Menu definitions
;;;
;;; Note: Mod-1 is the Alt or Meta key, Mod-2 is the Numlock key.
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
;;; --------------------------------------------------------------------------

(in-package :clfswm)

(init-menu)

;;; Here is a small example of menu manipulation:

;;(add-menu-key 'main "a" 'help-on-second-mode)
;;(add-menu-key 'main "c" 'help-on-clfswm)
;;
;;(add-sub-menu 'main "p" 'plop "A sub menu")
;;
;;(add-menu-key 'plop "a" 'help-on-clfswm)
;;(add-menu-key 'plop "b" 'help-on-second-mode)
;;(add-menu-key 'plop "d" 'help-on-second-mode)


;;(del-menu-key 'main "p")
;;(del-menu-value 'plop 'help-on-main-mode)
;;(del-sub-menu 'main 'plop)

;;(define-second-key ("a") 'open-menu)

(add-sub-menu 'main "F1" 'help-menu "Help menu")
(add-sub-menu 'main "d" 'standard-menu "Standard menu")
(add-sub-menu 'main "c" 'child-menu "Child menu")
(add-sub-menu 'main "f" 'frame-menu "Frame menu")
(add-sub-menu 'main "w" 'window-menu "Window menu")
(add-sub-menu 'main "s" 'selection-menu "Selection menu")
(add-sub-menu 'main "n" 'action-by-name-menu "Action by name menu")
(add-sub-menu 'main "u" 'action-by-number-menu "Action by number menu")
(add-sub-menu 'main "y" 'utility-menu "Utility menu")
(add-sub-menu 'main "o" 'configuration-menu "Configuration menu")
(add-sub-menu 'main "m" 'clfswm-menu "CLFSWM menu")


(update-menus (find-menu 'standard-menu))

(add-menu-key 'help-menu "a" 'show-first-aid-kit)
(add-menu-key 'help-menu "h" 'show-global-key-binding)
(add-menu-key 'help-menu "b" 'show-main-mode-key-binding)
(add-menu-key 'help-menu "s" 'show-second-mode-key-binding)
(add-menu-key 'help-menu "r" 'show-circulate-mode-key-binding)
(add-menu-key 'help-menu "e" 'show-expose-window-mode-key-binding)
(add-menu-key 'help-menu "c" 'show-corner-help)
(add-menu-key 'help-menu "g" 'show-config-variable)
(add-menu-key 'help-menu "d" 'show-date)
(add-menu-key 'help-menu "p" 'show-cpu-proc)
(add-menu-key 'help-menu "m" 'show-mem-proc)
(add-menu-key 'help-menu "v" 'show-version)


(add-menu-key 'child-menu "r" 'rename-current-child)
(add-menu-key 'child-menu "e" 'ensure-unique-name)
(add-menu-key 'child-menu "n" 'ensure-unique-number)
(add-menu-key 'child-menu "Delete" 'delete-current-child)
(add-menu-key 'child-menu "X" 'remove-current-child)
(add-menu-key 'child-menu "h" 'hide-current-child)
(add-menu-key 'child-menu "u" 'unhide-a-child)
(add-menu-key 'child-menu "f" 'unhide-a-child-from-all-frames)
(add-menu-key 'child-menu "a" 'unhide-all-children)
(add-menu-key 'child-menu "Page_Up" 'frame-lower-child)
(add-menu-key 'child-menu "Page_Down" 'frame-raise-child)



(add-sub-menu 'frame-menu "a" 'frame-adding-menu "Adding frame menu")
(add-sub-menu 'frame-menu "l" 'frame-layout-menu "Frame layout menu")
(add-sub-menu 'frame-menu "n" 'frame-nw-hook-menu "Frame new window hook menu")
(add-sub-menu 'frame-menu "m" 'frame-movement-menu "Frame movement menu")
(add-sub-menu 'frame-menu "f" 'frame-focus-policy "Frame focus policy menu")
(add-sub-menu 'frame-menu "w" 'frame-managed-window-menu "Managed window type menu")
(add-sub-menu 'frame-menu "u" 'frame-unmanaged-window-menu "Unmanaged window behaviour")
(add-sub-menu 'frame-menu "s" 'frame-miscellaneous-menu "Frame miscallenous menu")
(add-menu-key 'frame-menu "x" 'frame-toggle-maximize)


(add-menu-key 'frame-adding-menu "a" 'add-default-frame)
(add-menu-key 'frame-adding-menu "p" 'add-placed-frame)


(add-sub-menu 'frame-movement-menu "p" 'frame-pack-menu "Frame pack menu")
(add-sub-menu 'frame-movement-menu "f" 'frame-fill-menu "Frame fill menu")
(add-sub-menu 'frame-movement-menu "r" 'frame-resize-menu "Frame resize menu")
(add-menu-key 'frame-movement-menu "c" 'center-current-frame)
(add-menu-key 'frame-movement-menu "Right" 'with-movement-select-next-brother)
(add-menu-key 'frame-movement-menu "Left" 'with-movement-select-previous-brother)
(add-menu-key 'frame-movement-menu "Up" 'with-movement-select-next-level)
(add-menu-key 'frame-movement-menu "Down" 'with-movement-select-previous-level)
(add-menu-key 'frame-movement-menu "Tab" 'with-movement-select-next-child)


(add-menu-key 'frame-pack-menu "Up" 'current-frame-pack-up)
(add-menu-key 'frame-pack-menu "Down" 'current-frame-pack-down)
(add-menu-key 'frame-pack-menu "Left" 'current-frame-pack-left)
(add-menu-key 'frame-pack-menu "Right" 'current-frame-pack-right)


(add-menu-key 'frame-fill-menu "Up" 'current-frame-fill-up)
(add-menu-key 'frame-fill-menu "Down" 'current-frame-fill-down)
(add-menu-key 'frame-fill-menu "Left" 'current-frame-fill-left)
(add-menu-key 'frame-fill-menu "Right" 'current-frame-fill-right)
(add-menu-key 'frame-fill-menu "a" 'current-frame-fill-all-dir)
(add-menu-key 'frame-fill-menu "v" 'current-frame-fill-vertical)
(add-menu-key 'frame-fill-menu "h" 'current-frame-fill-horizontal)

(add-menu-key 'frame-resize-menu "Up" 'current-frame-resize-up)
(add-menu-key 'frame-resize-menu "Down" 'current-frame-resize-down)
(add-menu-key 'frame-resize-menu "Left" 'current-frame-resize-left)
(add-menu-key 'frame-resize-menu "Right" 'current-frame-resize-right)
(add-menu-key 'frame-resize-menu "a" 'current-frame-resize-all-dir)
(add-menu-key 'frame-resize-menu "m" 'current-frame-resize-all-dir-minimal)


(add-menu-comment 'frame-focus-policy "-=- For the current frame -=-")
(add-menu-key 'frame-focus-policy "a" 'current-frame-set-click-focus-policy)
(add-menu-key 'frame-focus-policy "b" 'current-frame-set-sloppy-focus-policy)
(add-menu-key 'frame-focus-policy "c" 'current-frame-set-sloppy-strict-focus-policy)
(add-menu-key 'frame-focus-policy "d" 'current-frame-set-sloppy-select-policy)
(add-menu-comment 'frame-focus-policy "-=- For all frames -=-")
(add-menu-key 'frame-focus-policy "e" 'all-frames-set-click-focus-policy)
(add-menu-key 'frame-focus-policy "f" 'all-frames-set-sloppy-focus-policy)
(add-menu-key 'frame-focus-policy "g" 'all-frames-set-sloppy-strict-focus-policy)
(add-menu-key 'frame-focus-policy "h" 'all-frames-set-sloppy-select-policy)




(add-menu-key 'frame-managed-window-menu "m" 'current-frame-manage-window-type)
(add-menu-key 'frame-managed-window-menu "a" 'current-frame-manage-all-window-type)
(add-menu-key 'frame-managed-window-menu "n" 'current-frame-manage-only-normal-window-type)
(add-menu-key 'frame-managed-window-menu "u" 'current-frame-manage-no-window-type)

(add-menu-key 'frame-unmanaged-window-menu "s" 'set-show-unmanaged-window)
(add-menu-key 'frame-unmanaged-window-menu "h" 'set-hide-unmanaged-window)
(add-menu-key 'frame-unmanaged-window-menu "d" 'set-default-hide-unmanaged-window)
(add-menu-key 'frame-unmanaged-window-menu "w" 'set-globally-show-unmanaged-window)
(add-menu-key 'frame-unmanaged-window-menu "i" 'set-globally-hide-unmanaged-window)


(add-menu-key 'frame-miscellaneous-menu "s" 'show-all-frames-info)
(add-menu-key 'frame-miscellaneous-menu "a" 'hide-all-frames-info)
(add-menu-key 'frame-miscellaneous-menu "h" 'hide-current-frame-window)
(add-menu-key 'frame-miscellaneous-menu "w" 'show-current-frame-window)
(add-menu-key 'frame-miscellaneous-menu "u" 'renumber-current-frame)
(add-menu-key 'frame-miscellaneous-menu "x" 'explode-current-frame)
(add-menu-key 'frame-miscellaneous-menu "i" 'implode-current-frame)




(add-menu-key 'window-menu "i" 'display-current-window-info)
(add-menu-key 'window-menu "f" 'force-window-in-frame)
(add-menu-key 'window-menu "c" 'force-window-center-in-frame)
(add-menu-key 'window-menu "m" 'manage-current-window)
(add-menu-key 'window-menu "u" 'unmanage-current-window)
(add-menu-key 'window-menu "a" 'adapt-current-frame-to-window-hints)
(add-menu-key 'window-menu "w" 'adapt-current-frame-to-window-width-hint)
(add-menu-key 'window-menu "h" 'adapt-current-frame-to-window-height-hint)



(add-menu-key 'selection-menu "x" 'cut-current-child)
(add-menu-key 'selection-menu "c" 'copy-current-child)
(add-menu-key 'selection-menu "v" 'paste-selection)
(add-menu-key 'selection-menu "p" 'paste-selection-no-clear)
(add-menu-key 'selection-menu "Delete" 'remove-current-child)
(add-menu-key 'selection-menu "z" 'clear-selection)



(add-menu-key 'action-by-name-menu "f" 'focus-frame-by-name)
(add-menu-key 'action-by-name-menu "o" 'open-frame-by-name)
(add-menu-key 'action-by-name-menu "d" 'delete-frame-by-name)
(add-menu-key 'action-by-name-menu "m" 'move-current-child-by-name)
(add-menu-key 'action-by-name-menu "c" 'copy-current-child-by-name)

(add-menu-key 'action-by-number-menu "f" 'focus-frame-by-number)
(add-menu-key 'action-by-number-menu "o" 'open-frame-by-number)
(add-menu-key 'action-by-number-menu "d" 'delete-frame-by-number)
(add-menu-key 'action-by-number-menu "m" 'move-current-child-by-number)
(add-menu-key 'action-by-number-menu "c" 'copy-current-child-by-number)


(add-menu-key 'utility-menu "i" 'identify-key)
(add-menu-key 'utility-menu "colon" 'eval-from-query-string)
(add-menu-key 'utility-menu "exclam" 'run-program-from-query-string)
(add-sub-menu 'utility-menu "o" 'other-window-manager-menu "Other window manager menu")

(add-menu-key 'other-window-manager-menu "x" 'run-xterm)
(add-menu-key 'other-window-manager-menu "t" 'run-twm)
(add-menu-key 'other-window-manager-menu "i" 'run-icewm)
(add-menu-key 'other-window-manager-menu "g" 'run-gnome-session)
(add-menu-key 'other-window-manager-menu "k" 'run-startkde)
(add-menu-key 'other-window-manager-menu "c" 'run-xfce4-session)
(add-menu-key 'other-window-manager-menu "l" 'run-lxde)
(add-menu-key 'other-window-manager-menu "p" 'run-prompt-wm)


(add-menu-key 'clfswm-menu "r" 'reset-clfswm)
(add-menu-key 'clfswm-menu "l" 'reload-clfswm)
(add-menu-key 'clfswm-menu "x" 'exit-clfswm)

