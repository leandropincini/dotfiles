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

;; Backup files
(setq delete-auto-save-files t)
(setq make-backup-files nil)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(provide 'system-configs)
;; end of system-configs.el
