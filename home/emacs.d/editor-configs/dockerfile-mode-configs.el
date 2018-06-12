;; dockerfile-mode-configs.el - emacs dockerfile-mode editor configs

(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))

(provide 'dockerfile-mode-configs)

;; end of dockerfile-mode-configs.el
