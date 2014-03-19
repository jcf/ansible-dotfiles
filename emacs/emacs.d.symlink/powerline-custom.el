;;; powerline-custom.el --- Sets up Powerline

;;; Commentary:

;; Loads up Powerline, activates it, and defines my custom Powerline
;; theme, which has rudimentary support for showing the current evil
;; state/mode.

;;; Code:
(require 'use-package)

(use-package powerline
  :init
  (progn
    (defface jcf-powerline-emacs
      '((t (:background "#af74e6" :foreground "#080808" :inherit mode-line)))
      "Colour applied to Emacs mode indicator."
      :group 'powerline)

    (defface jcf-powerline-insert
      '((t (:background "#66d9ef" :foreground "#080808" :inherit mode-line)))
      "Colour applied to insert mode indicator."
      :group 'powerline)

    (defface jcf-powerline-motion
      '((t (:background "#465457" :foreground "#1b1d1e" :inherit mode-line)))
      "Colour applied to motion mode indicator."
      :group 'powerline)

    (defface jcf-powerline-normal
      '((t (:background "#e6db74" :foreground "#080808" :inherit mode-line)))
      "Colour applied to normal mode indicator."
      :group 'powerline)

    (defface jcf-powerline-visual
      '((t (:background "#fd971f" :foreground "#080808" :inherit mode-line)))
      "Colour applied to visual mode indicator."
      :group 'powerline)

    (defun jcf-propertized-evil-mode-tag ()
      (let* ((x (cond ((evil-emacs-state-p)  '(" EMACS  " jcf-powerline-emacs))
                      ((evil-insert-state-p) '(" INSERT " jcf-powerline-insert))
                      ((evil-motion-state-p) '(" MOTION " jcf-powerline-motion))
                      ((evil-normal-state-p) '(" NORMAL " jcf-powerline-normal))
                      ((evil-visual-state-p) '(" VISUAL " jcf-powerline-visual))
                      (t                     '(" WAT?!  " jcf-powerline-emacs))))
             (text (or (first x) ""))
             (face (second x)))

        (list
         (powerline-raw text face face)
         (powerline-arrow-left face nil))))

    (defun jcf-powerline-theme ()
      (interactive)
      (setq-default
       mode-line-format
       '("%e"
         (:eval
          (let* ((active (powerline-selected-window-active))
                 (mode-line (if active 'mode-line 'mode-line-inactive))
                 (face1 (if active 'powerline-active1 'powerline-inactive1))
                 (face2 (if active 'powerline-active2 'powerline-inactive2))
                 (separator-left (intern (format "powerline-%s-%s"
                                                 powerline-default-separator
                                                 (car powerline-default-separator-dir))))
                 (separator-right (intern (format "powerline-%s-%s"
                                                  powerline-default-separator
                                                  (cdr powerline-default-separator-dir))))

                 (lhs (append
                       (jcf-propertized-evil-mode-tag)
                       (list (powerline-raw "%*" nil 'l)
                             (powerline-buffer-size nil 'l)
                             (powerline-raw mode-line-mule-info nil 'l)
                             (powerline-buffer-id nil 'l)
                             (when (and (boundp 'which-func-mode) which-func-mode)
                               (powerline-raw which-func-format nil 'l))
                             (powerline-raw " ")
                             (funcall separator-left mode-line face1)
                             (when (boundp 'erc-modified-channels-object)
                               (powerline-raw erc-modified-channels-object face1 'l))
                             (powerline-major-mode face1 'l)
                             (powerline-process face1)
                             (powerline-minor-modes face1 'l)
                             (powerline-narrow face1 'l)
                             (powerline-raw " " face1)
                             (funcall separator-left face1 face2)
                             (powerline-vc face2 'r))))
                 (rhs (list (powerline-raw global-mode-string face2 'r)
                            (funcall separator-right face2 face1)
                            (powerline-raw "%4l" face1 'l)
                            (powerline-raw ":" face1 'l)
                            (powerline-raw "%3c" face1 'r)
                            (funcall separator-right face1 mode-line)
                            (powerline-raw " ")
                            (powerline-raw "%6p" nil 'r)
                            (powerline-hud face2 face1))))
            (concat (powerline-render lhs)
                    (powerline-fill face2 (powerline-width rhs))
                    (powerline-render rhs)))))))

    (jcf-powerline-theme)))

(provide 'powerline-custom)
;;; powerline-custom.el ends here
