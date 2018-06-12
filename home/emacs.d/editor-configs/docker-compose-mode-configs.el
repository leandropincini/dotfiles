;; docker-compose-mode-configs.el - emacs docker-compose-mode editor configs

(require 'docker-compose-mode)

(add-to-list 'auto-mode-alist '("\\docker-compose.yml\\'" . docker-compose-mode))

(provide 'docker-compose-mode-configs)
;; end of docker-compose-mode-configs.el
