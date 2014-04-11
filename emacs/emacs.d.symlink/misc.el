;;; misc.el --- General Emacs config

;;; Commentary:

;; This is where all configuration that doesn't fit into a packages
;; ends up.

;;; Code:

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

;; Keep things narrow
(set-default 'fill-column 72)

(setq redisplay-dont-pause t)

;; Keep Emacs cruft in a platform-specific tmp directory
(setq
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Do not make any backup files
(setq make-backup-files nil)

;; Kill whole line
(setq kill-whole-line t)

;; Do not show startup message
(setq inhibit-startup-message t)

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Newline at EOF
(setq next-line-add-newlines nil)
(setq require-final-newline t)

;; Do not show annoying menu-bar tips
(setq suggest-key-bindings nil)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; Initial major mode is Emacs Lisp mode
(setq initial-major-mode 'emacs-lisp-mode)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Truncate lines
(set-default 'truncate-lines t)

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Remove selected region if typing
(pending-delete-mode 1)

;; Allow some commands
(dolist (command '(narrow-to-region downcase-region upcase-region))
  (put command 'disabled nil))

;; Prefer UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set font size
(set-face-attribute 'default nil :height 120)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight symbol at point
(add-hook 'find-file-hook 'idle-highlight-mode)

;; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load sh-mode when opening a file with a zsh extension
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; Load conf-mode in a Procfile
(add-to-list 'auto-mode-alist '("Procfile" . conf-mode))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 0)

(provide 'misc)
;;; misc.el ends here
