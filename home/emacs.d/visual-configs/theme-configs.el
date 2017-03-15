;; theme-configs.el - emacs theme configs

;; verify and load dracula theme
(when (require 'dracula-theme nil t)
  (load-theme 'dracula t))

(provide 'theme-configs)
;; end of thtme-configs.el
