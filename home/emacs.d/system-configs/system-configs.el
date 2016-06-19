;; system-configs.el - emacs system configs

;; alarmbell off
(setq visible-bell -1)
(setq ring-bell-function 'ignore)
(setq blink-cursor-mode nil)

;; UTF-8 encoding
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(setq current-language-environment "UTF-8")

;; no backup files
(setq delete-auto-save-files t)
(setq make-backup-files nil)

(provide 'system-configs)
;; end of system-configs.el
