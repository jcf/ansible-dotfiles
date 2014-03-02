;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 's)
(require 'f)
(require 'git)
(require 'ert)
(require 'evm)
(require 'dash)
(require 'use-package)

(defun load-x (file)
  "Load FILE relative to `user-emacs-directory'."
  (load (f-expand file user-emacs-directory)))

(setq inhibit-startup-message t)

(use-package color-theme-molokai
  :init
  (load-theme 'molokai t))

;; Better scroll settings
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Mode line settings
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; Make the gutter smaller
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

(load-x "defuns")
(load-x "misc")
(load-x "packages")

(when (eq system-type 'darwin)
  (load-x "osx"))

(server-start)


;;;; Bindings

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(bind-key "C-o" 'occur)
(bind-key "M-g" 'goto-line)
(bind-key "M-n" 'open-line-below)
(bind-key "M-p" 'open-line-above)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)
(bind-key "M-`" 'other-frame)

(bind-key "C-c g" 'google)
(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c r" 'rename-this-buffer-and-file)
(bind-key "C-c k" 'delete-this-buffer-and-file)

(bind-key "C-M-h" 'backward-kill-word)
(bind-key "C-c C-n" 'todo)

(custom-set-faces
 '(helm-selection ((t (:background "gray12" :foreground "DeepSkyBlue2" :underline nil))))
 '(helm-separator ((t (:foreground "LightGoldenrod4"))))
 '(helm-source-header ((t (:foreground "white" :weight bold)))))
