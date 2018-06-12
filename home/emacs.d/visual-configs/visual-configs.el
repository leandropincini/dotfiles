;; visual-configs.el - emacs visual configs

;; hide startup message
(setq inhibit-startup-message t)

;; turn off tollbar
(tool-bar-mode -1)

;; no blinking cursor
(blink-cursor-mode -1)

;; show line,column number
(column-number-mode t)

;; enable line numbers globally
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

;; show matching pairs
(show-paren-mode 1)

;; open maximized
(toggle-frame-maximized)

(provide 'visual-configs)
;; end of visual-configs.el
