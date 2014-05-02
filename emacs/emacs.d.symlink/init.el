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

(use-package color-theme-monokai
  :init
  (load-theme 'monokai t))

(load-x "defuns")
(load-x "misc")
(load-x "packages")
(load-x "evil-custom")
(load-x "helm-custom")
(load-x "powerline-custom")

(load-x "ruby-custom")

(when (eq system-type 'darwin)
  (load-x "osx"))


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
