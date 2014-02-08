;;; init.el --- All my config
;;; Commentary:
;;; Code:

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

(let ((default-directory user-emacs-directory))
  (load-x "defuns")
  (load-x "misc")
  (when (eq system-type 'darwin)
    (load-x "osx")))


;;;; Packages

(use-package smooth-scroll)
(use-package dired-x)
(use-package winner-mode)

(use-package helm
  :init
  (progn
    (helm-mode 1)
    (use-package helm-ag)
    (use-package helm-git-grep)
    (use-package helm-go-package))
  :config
  (progn
    (global-set-key (kbd "C-x C-f") 'helm-find-files)))

(use-package simpleclip
  :init (simpleclip-mode 1))

(use-package evil
  :init
  (progn
    ;; Do not automatically start evil-mode. Let's try to do things the
    ;; Emacs way.
    ;; (evil-mode 1)

    (define-key evil-normal-state-map "Y" (kbd "y$"))
    (define-key evil-normal-state-map (kbd "SPC") 'evil-repeat-find-char)
    (define-key
      evil-normal-state-map (kbd "S-SPC") 'evil-repeat-find-char-reverse)

    (define-key evil-normal-state-map ";" 'evil-ex)
    (define-key evil-visual-state-map ";" 'evil-ex)

    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")

        (evil-leader/set-key "b" 'helm-buffers-list
          "c" 'ido-dired
          "d" 'kill-buffer
          "f" 'helm-find-files)

        (evil-leader/set-key-for-mode 'ruby-mode
          "a" 'rspec-toggle-spec-and-target
          "v" 'rspec-verify
          "V" 'rspec-verify-all)

        (evil-leader/set-key-for-mode 'feature-mode
          "v" 'feature-verify-scenario-at-pos
          "V" 'feature-verify-all-scenarios-in-buffer)))

    (use-package evil-surround
      :init (global-surround-mode 1))

    (use-package evil-matchit
      :init (global-evil-matchit-mode 1))

    (use-package key-chord
      :init (key-chord-mode 1)
      :config
      (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)))

  :config
  (setq evil-default-cursor t))

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-file-extensions-order '(".rb" ".el" ".coffee" ".js"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; (use-package popwin
;;   :config (setq display-buffer-alist 'popwin:display-buffer))

(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'projectile-completion-fn)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    ;; (setq magit-emacsclient-executable (evm-find "emacsclient"))
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (add-hook 'magit-mode-hook 'rinari-launch))
  :bind ("C-x g" . magit-status))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package cua-base
  :init (cua-mode 1)
  :config
  (progn
    (setq cua-enable-cua-keys nil)
    (setq cua-toggle-set-mark nil)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :config (setq-default save-place t))

(use-package diff-hl
  :init (global-diff-hl-mode)
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

(use-package ruby-mode
  :init
  (progn
    (use-package rbenv
      :init
      (progn
        (setq rbenv-modeline-function 'rbenv--modeline-plain)
        (setq rbenv-show-active-ruby-in-modeline nil)
        (global-rbenv-mode)))
    (use-package ruby-tools)
    (use-package rhtml-mode
      :mode (("\\.rhtml$" . rhtml-mode)
             ("\\.html\\.erb$" . rhtml-mode)))
    (use-package rinari
      :init (global-rinari-mode 1)
      :config (setq ruby-insert-encoding-magic-comment nil))
    (use-package rspec-mode
      :config
      (progn
        (setq rspec-use-rake-flag nil)
        (defadvice rspec-compile (around rspec-compile-around activate)
          "Use BASH shell for running the specs because of ZSH issues."
          (let ((shell-file-name "/bin/bash"))
            ad-do-it)))))
  :config
  (progn
    (setenv "JRUBY_OPTS" "--2.0")
    (setenv "JAVA_OPTS" "-d32")
    (add-hook 'ruby-mode-hook 'rspec-mode)
    (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
    (setq ruby-deep-indent-paren nil))
  :bind (("C-M-h" . backward-kill-word))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Thorfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

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
    (setq smartparens-strict-mode t)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
    (sp-local-tag '(sgml-mode html-mode rhtml-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))
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
  (progn
    (let ((snippets-dir (f-expand "snippets" user-emacs-directory)))
      (yas/load-directory snippets-dir)
      (setq yas/snippet-dirs snippets-dir))
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode))

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "bsd")))
    (setq tab-width 2)
    (setq c-basic-offset 2)))

(use-package css-mode
  :config (setq css-indent-offset 2))

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word)
         ("M-j" . join-line-or-lines-in-region))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))))

(use-package coffee-mode
  :init
  (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
  :config
  (progn
    (setq coffee-tab-width 2)
    (setq coffee-cleanup-whitespace nil)))

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
(use-package scss-mode)

(use-package paredit)
(use-package clojure-mode
  :init
  (progn
    (use-package clojure-mode)
    (use-package cider
      :init (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
    (add-hook 'clojure-mode-hook 'paredit-mode)))
(use-package erlang-mode)
(use-package haskell-mode)

(use-package eshell
  :bind ("M-e" . eshell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)))

(load-theme 'ujelly t)
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

(provide 'init)
;;; init.el ends here
