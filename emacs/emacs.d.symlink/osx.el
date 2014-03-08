;;; osx.el --- Handles configuration specifc to OS X

;;; Commentary:

;; All configuration only relevant when running Emacs on OS X.

;;; Code:

;; Use command as meta key, leave option alone
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Menu bar is not annoying in OSX
(menu-bar-mode 1)

;; Make the browser the OS X default
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; In dired, move deletions to trash
(setq delete-by-moving-to-trash t)

;; Set font
(set-frame-font "-apple-Source_Code_Pro_for_Powerline-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")

;; Cmd-shift-f to toggle full screen
(bind-key "M-F" 'toggle-frame-fullscreen)

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file"))))

;; Use GNU ls - install with:
;;    brew install xz
;;    brew install coreutils
(setq insert-directory-program "gls")

(provide 'osx)
;;; osx.el ends here
