;;; helm-custom.el --- Sets up Helm

;;; Commentary:

;; Loads up Helm, activates it, and configures it with lots of shiny sources

;;; Code:
(require 'use-package)

(use-package helm
  :config
  (progn
    (use-package helm-ag)
    (use-package helm-ag-r)
    (use-package helm-c-yasnippet)
    (use-package helm-dired-recent-dirs)
    (use-package helm-git)
    (use-package helm-git-grep)
    (use-package helm-go-package)
    (use-package helm-helm-commands)
    (use-package helm-projectile)
    (use-package helm-rails)
    (use-package helm-rb)
    (use-package helm-rubygems-local)))

(provide 'helm-custom)
;;; helm-custom.el ends here
