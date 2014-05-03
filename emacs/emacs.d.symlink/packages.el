;;; packages.el --- Configures all packages

;;; Commentary:

;; Uses use-packages to require, initialise, and configure packages
;; and modes.

;;; Code:
(require 'use-package)

(use-package align-cljlet)
(use-package ibuffer-git)
(use-package scratch)
(use-package volatile-highlights)
(use-package gist)
(use-package browse-kill-ring)

(use-package editorconfig
  :mode "\\.editorconfig\\'"
  :config
  (add-to-list 'auto-mode-alist '("\\.editorconfig$" . conf-unix-mode)))

(use-package linum
  :init
  (progn
    ;; Show lines numbers in the gutter
    (global-linum-mode 1)
    (setq linum-format "%4d")))

(use-package ggtags
  :init
  (ggtags-mode 1))

(use-package fill-column-indicator
  :init
  (progn
    (define-globalized-minor-mode global-fci-mode fci-mode
      (lambda () (fci-mode 1)))

    (setq-default fill-column 80)
    (global-fci-mode 1)))

(use-package dired
  :config
  (progn
    (use-package dired-x
      :config
      (progn
        (setq-default dired-omit-files-p t)
        (setq dired-omit-files
         (concat dired-omit-files "\\|.DS_Store$"))))))

(use-package winner
  :init (winner-mode))

(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode 1)

    (setq
     undo-tree-auto-save-history t
     undo-tree-history-directory-alist backup-directory-alist)))

(use-package auto-complete
  :init
  (progn
    (global-auto-complete-mode 1)
    (ac-linum-workaround)

    (use-package popup)
    (use-package fuzzy)

    (use-package ac-nrepl
      :init
      (progn
        (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
        (add-hook 'cider-mode-hook 'ac-nrepl-setup)
        (eval-after-load "auto-complete"
          '(add-to-list 'ac-modes 'cider-repl-mode))))))

(use-package guide-key
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))))

(use-package pretty-symbols
  :init (pretty-symbols-mode 1))

(use-package ido
  :init
  (progn
    (setq ido-use-faces nil)

    (use-package flx-ido
      :init (flx-ido-mode 1))

    (use-package ido-ubiquitous
      :init (setq ido-everywhere t))

    (use-package ido-vertical-mode
      :init (ido-vertical-mode 1)))

  :config
  (progn
    (setq
     ido-case-fold t
     ido-enable-prefix nil
     ido-enable-flex-matching t
     ido-create-new-buffer 'always
     ido-max-prospects 10
     ido-file-extensions-order
     '(".clj" ".rb" ".el" ".coffee" ".js"))

    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)))

(use-package popwin
  :init (popwin-mode 1))

(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq
     projectile-enable-caching t
     projectile-require-project-root nil
     projectile-completion-system 'projectile-completion-fn)

    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))

  :config
  (progn
    (setq
     ;; magit-emacsclient-executable (evm-find "emacsclient"))
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     magit-set-upstream-on-push t
     magit-completing-read-function 'magit-ido-completing-read
     magit-stage-all-confirm nil
     magit-unstage-all-confirm nil)

    (add-hook 'magit-mode-hook 'rinari-launch)

    ;; No weird little popup window that doesn't work with fullscreen apps
    (setq ediff-window-setup-function 'ediff-setup-windows-plain))

  :bind ("C-x g" . magit-status))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package cua-base
  :init (cua-mode 1)
  :config
  (progn
    (setq
     cua-enable-cua-keys nil
     cua-toggle-set-mark nil)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :init (setq-default save-place t))

(use-package diff-hl
  :config (add-hook 'vc-checkin-hook 'diff-hl-update))

(use-package page-break-lines
  :init (global-page-break-lines-mode 1)
  :config
  (progn
    (defadvice backward-page (after backward-page-mbol activate)
      (move-beginning-of-line 1))
    (defadvice forward-page (after forward-page-mbol activate)
      (move-beginning-of-line 1))))

(use-package windmove
  :config (windmove-default-keybindings 'shift))

(use-package python-mode
  :config
  (progn
    (use-package elpy
      :init (elpy-enable))))

(use-package feature-mode)

(use-package markdown-mode
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq
     smartparens-strict-mode t
     sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
    (sp-local-tag '(sgml-mode html-mode rhtml-mode)
                  "<" "<_>" "</_>"
                  :transform 'sp-match-sgml-tags))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package flycheck
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-cask
  :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package yasnippet
  :init
  (yas-global-mode 1)

  :config
  (progn
    (setq yas/snippet-dirs (f-expand "snippets" user-emacs-directory))
    (setq-default yas/prompt-functions '(yas/ido-prompt)
                  require-final-newline nil)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode))

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "bsd")))
    (setq
     tab-width 2
     c-basic-offset 2)))

(use-package css-mode
  :mode "\\.css$"
  :config (setq css-indent-offset 2))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("\\.json$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word)
         ("M-j" . join-line-or-lines-in-region))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))))

(use-package coffee-mode
  :config
  (progn
    (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
    (setq
     coffee-tab-width 2
     coffee-cleanup-whitespace nil)))

(use-package sh-script
  :config (setq sh-basic-offset 2))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config
      (progn
        (put 'ert-deftest 'lisp-indent-function 'defun)
        (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    (font-lock-add-keywords
                     nil
                     '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
                        (1 font-lock-keyword-face nil t)
                        (2 font-lock-function-name-face nil t)))))))))
  :bind (("M-&" . lisp-complete-symbol)
         ("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package html-script-src)
(use-package slim-mode)
(use-package haml-mode)

(use-package sass-mode)
(use-package scss-mode
  :config (setq scss-compile-at-save nil))

(use-package clojure-mode
  :init
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))
  :config
  (progn
    (use-package clojure-test-mode)

    (use-package cider
      :config
      (progn
        (setq
         cider-repl-pop-to-buffer-on-connect nil
         cider-popup-stacktraces nil
         cider-repl-popup-stacktraces t
         cider-auto-select-error-buffer t
         cider-repl-wrap-history t)))))

(use-package erlang)
(use-package haskell-mode)

(use-package go-mode
  :init (add-hook 'before-save-hook #'gofmt-before-save))

(use-package nrepl-eval-sexp-fu
  :init
  (setq nrepl-eval-sexp-fu-flash-duration 0.5))

(use-package eshell
  :bind ("M-e" . make-shell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq
     eshell-history-size 5000
     eshell-save-history-on-exit t)))

(use-package multi-term
  :config
  (setq
   multi-term-program "/bin/zsh"
   term-buffer-maximum-size 10000
   show-trailing-whitespace nil))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; Load this last to make sure bindings take effect
(use-package simpleclip
  :init (simpleclip-mode 1)
  :bind (("M-c" . simpleclip-copy)
         ("M-v" . simpleclip-paste)))

(provide 'packages)
;;; packages.el ends here
