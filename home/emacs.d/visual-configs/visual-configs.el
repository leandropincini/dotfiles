;; visual-configs.el - emacs visual configs

;; hide startup message
(setq inhibit-startup-message t)

;; turn off tollbar
(tool-bar-mode -1)

;; turn off scrollbar
(scroll-bar-mode -1)

(set-fringe-mode 10)

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

;; highline cursor line
(global-hl-line-mode +1)

(provide 'visual-configs)
;; end of visual-configs.el
