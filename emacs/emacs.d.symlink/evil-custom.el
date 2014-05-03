;;; evil-custom.el --- Sets up Evil

;;; Commentary:

;; Loads up Evil, activates it, and configures it with custom leading
;; bindings, and more.

;;; Code:
(require 'use-package)

(use-package evil
  :init
  (progn
    (setq
     evil-insert-state-message nil
     evil-visual-state-message nil)

    (define-key evil-normal-state-map "Y" (kbd "y$"))
    (define-key evil-normal-state-map (kbd "SPC") 'evil-repeat-find-char)
    (define-key
      evil-normal-state-map (kbd "S-SPC") 'evil-repeat-find-char-reverse)

    (define-key evil-normal-state-map ";" 'evil-ex)
    (define-key evil-visual-state-map ";" 'evil-ex)

    (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    ;; Setup initial Evil mode for a number of commonly used modes.
    (loop for (mode . state)
          in '((ielm-mode . insert)
               (nrepl-mode . insert)
               (shell-mode . insert)
               (git-rebase-mode . emacs)
               (term-mode . emacs)
               (help-mode . emacs)
               (helm-grep-mode . emacs)
               (grep-mode . emacs)
               (bc-menu-mode . emacs)
               (magit-branch-manager-mode . emacs)
               (rdictcc-buffer-mode . emacs)
               (dired-mode . normal)
               (wdired-mode . normal))
          do (evil-set-initial-state mode state))

    ;; Use escape to quit, and not as a meta-key.
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")

        (evil-leader/set-key
          "a"  'ido-find-alternate-file
          "b"  'ibuffer
          "db" 'kill-buffer
          "dw" 'delete-window
          "eb" 'eval-buffer
          "ee" 'eval-expression
          "er" 'eval-region
          "fb" 'ido-switch-buffer
          "fd" 'ido-dired
          "ff" 'ido-find-file
          "fp" 'fiplr-find-file
          "gb" 'magit-blame-mode
          "gl" 'magit-log
          "gs" 'magit-status
          "pb" 'projectile-switch-to-buffer
          "pd" 'projectile-dired
          "pf" 'projectile-find-file
          "x"  'smex)

        (evil-leader/set-key-for-mode 'org-mode
          "t" 'org-todo
          "s" 'org-sort-list)

        (evil-leader/set-key-for-mode 'clojure-mode
          "a" 'clojure-jump-between-tests-and-code
          "v" 'clojure-test-run-test
          "V" 'clojure-test-run-tests
          "cc" 'cider-connect
          "cj" 'cider-jack-in
          "cq" 'cider-quit
          "rs" 'cljr-sort-ns
          "rr" 'cljr-add-require-to-ns
          "ru" 'cljr-add-use-to-ns)

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
      (progn
        (setq key-chord-two-keys-delay 0.05)
        (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)))

    ;; Activate evil-mode after global-evil-leader-mode (http://j.mp/1i0vLSP)
    (evil-mode 1))

  :config
  (setq evil-default-cursor t))

(provide 'evil-custom)
;;; evil-custom.el ends here
