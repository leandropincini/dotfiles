;; auto-package-update-configs.el - emacs configs for auto-update-packages

(require 'auto-package-update)

;; update installed packages at startup if there is an update pending
(auto-package-update-maybe)

;; prompt before update
(setq auto-package-update-prompt-before-update t)

;; delete residual old version directory when updating
(setq auto-package-update-delete-old-versions t)

(provide 'auto-package-update-configs)
;; end of auto-package-update-configs.el
