;; init.el - emacs configuration

;; only runs on emacs >= 24
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; add configuration sub-directories
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; load all other configuration
;(require 'proxy)
(require 'visual-configs)
(require 'system-configs)
(require 'editor-configs)
(require 'packages)
(require 'theme-configs)
(require 'web-mode-configs)

;; are you running osx? load mac-configs
(when (eq system-type 'darwin)
  (require 'mac-configs))
;; end of init.el
