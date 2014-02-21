;;; packages.el --- Configures all packages

;;; Commentary:

;; Uses use-packages to require, initialise, and configure packages
;; and modes.

;;; Code:
(use-package align-cljlet)
(use-package ibuffer-git)
(use-package scratch)
(use-package volatile-highlights)
(use-package gist)
(use-package browse-kill-ring)

(use-package auto-compile
  :init
  (progn
    (auto-compile-on-load-mode 1)
    (auto-compile-on-save-mode 1)))

(use-package elisp-slime-nav
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package smart-mode-line
  :config
  (progn
    (setq
     sml/theme 'dark
     sml/name-width 40
     sml/vc-mode-show-backend t)

    (sml/setup)))

(use-package smooth-scroll
  :init (smooth-scroll-mode))

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
  :config
  (progn
    (setq undo-tree-auto-save-history t)))

(use-package auto-complete
  :init
  (progn
    (auto-complete-mode)
    (use-package popup)
    (use-package fuzzy)))

(use-package helm
  :init
  (progn
    (use-package helm-ag)
    (use-package helm-git-grep)
    (use-package helm-go-package))
  :config
  (progn
    (global-set-key (kbd "C-x C-f") 'helm-find-files)))

(use-package m4ue
  :config
  (progn
    (setq
     mu4e-maildir       "~/Mail"
     mu4e-sent-folder   "/Sent"
     mu4e-drafts-folder "/Drafts"
     mu4e-trash-folder  "/Trash"
     mu4e-refile-folder "/Archive"
     mu4e-get-mail-command "offlineimap"
     mu4e-update-interval 300)))

(use-package evil
  :init
  (progn
    (evil-mode 1)

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

        (evil-leader/set-key
         "b" 'ido-switch-buffer
         "d" 'kill-buffer
         "f" 'ido-find-file)

        (evil-leader/set-key-for-mode 'ruby-mode
                                      "a" 'rspec-toggle-spec-and-target
                                      "v" 'rspec-verify
                                      "V" 'rspec-verify-all)

        (evil-leader/set-key-for-mode 'feature-mode
                                      "v" 'feature-verify-scenario-at-pos
                                      "V" 'feature-verify-all-scenarios-in-buffer)))

    (use-package surround
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
  :init
  (progn
    (ido-mode 1)
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
  :bind ("M-x" . smex))

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

(use-package misc
  :bind ("M-z" . zap-up-to-char))

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
    (setq
     cua-enable-cua-keys nil
     cua-toggle-set-mark nil)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :config (setq-default save-place t))

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

(use-package sh-mode
  :init
  ((add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))))

(use-package ruby-mode
  :init
  (progn
    (use-package rbenv
      :init
      (progn
        (setq
         rbenv-modeline-function 'rbenv--modeline-plain
         rbenv-show-active-ruby-in-modeline nil)
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
    (setq
     smartparens-strict-mode t
     sp-autoinsert-if-followed-by-word t)
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
    (setq-default yas/prompt-functions '(yas/ido-prompt)))
  :config
  (setq-default require-final-newline nil))

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

(use-package paredit)
(use-package clojure-mode
  :init
  (progn
    (use-package clojure-test-mode)
    (use-package cider
      :init (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
    (add-hook 'clojure-mode-hook 'paredit-mode)))
(use-package erlang)
(use-package haskell-mode)

(use-package nrepl-eval-sexp-fu
  :init
  (setq nrepl-eval-sexp-fu-flash-duration 0.5))

;; TODO Slot this into Clojure mode above
(use-package ac-nrepl
  :init
  (progn
    (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
    (add-hook 'cider-mode-hook 'ac-nrepl-setup)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes 'cider-repl-mode))))

(use-package eshell
  :bind ("M-e" . eshell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq
     eshell-history-size 5000
     eshell-save-history-on-exit t)))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; Load this last to make sure bindings take effect
(use-package simpleclip
  :init (simpleclip-mode 1)
  :bind (("M-c" . simpleclip-copy)
         ("M-v" . simpleclip-paste)))

(provide 'packages)
;;; packages.el ends here