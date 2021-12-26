;;; init.el --- emacs configuration

;; only runs on emacs >= 26.1
(let ((minver 26.1))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old --this configuration requires v%s or higher" minver)))

;; add configuration sub-directories
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; local path
(add-to-list 'exec-path "/usr/local/bin")

;; utils - emacs utils
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-current-file ()
  "Open the current file as a URL using 'browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))
;; end of utils - emacs utils

;; visual-configs - emacs visual configs
;; hide startup message
(setq inhibit-startup-message t)

;; turn off toolbar
(tool-bar-mode -1)

;; turn off scrollbar
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; no blinking cursor
(blink-cursor-mode -1)

;; show line, column number
(column-number-mode t)

;; enable line numbers globally
(global-display-line-numbers-mode t)

;; show matching pairs
(show-paren-mode 1)

;; open maximized
(toggle-frame-maximized)

;; highline cursor line
(global-hl-line-mode +1)

;; default font and size
(when (memq window-system '(x))
  (set-face-attribute 'default nil
                      :family "monospace"
                      :height 100
                      :weight 'normal
                      :width 'normal))
;; end of visual-configs - emacs visual configs

;; system-configs - emcs system configs
;; alarmbell off
(setq visible-bell -1)
(setq ring-bell-function 'ignore)

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

;; warm when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;; end of system-configs - emacs system configs

;; editor-configs - emacs editor configs
;; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; classic select
(setq shift-select-mode nil)

;; indent with spaces
(setq-default indent-tabs-mode nil)

;; smart inference of indentation style
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regex): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d ocurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode (spaces)
  (let ((space-count (how-many-region (point-min) (point-max) "^ "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(infer-indentation-style)

;; auto (on save) clear trailing spaces
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; auto (on save) add new eof line
(setq require-final-newline t)
;; end of editor-configs emacs editor configs

;; packages - load emacs packages

(require 'package)

;; secure gnu repo
(setq tls-checktrust t)

(setq python (or (executable-find "py.exe")
                 (executable-find "python")))

(let ((trustfile
      (replace-regexp-in-string
       "\\\\" "/"
       (replace-regexp-in-string
        "\n" ""
        (shell-command-to-string (concat python " -m certifi"))))))
 (setq tls-program
       (list
        (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                (if (eq window-system 'w32) ".exe" "") trustfile)))
 (setq gnutls-verify-error t)
 (setq gnutls-trustfiles (list trustfile)))

;; You can test settings by using the following code snippet:

;;(let ((bad-hosts
;;       (loop for bad
;;             in `("https://wrong.host.badssl.com/"
;;                  "https://self-signed.badssl.com/")
;;             if (condition-case e
;;                    (url-retrieve
;;                     bad (lambda (retrieved) t))
;;                  (error nil))
;;             collect bad)))
;;  (if bad-hosts
;;      (error (format "tls misconfigured; retrieved %s ok"
;;                     bad-hosts))
;;    (url-retrieve "https://badssl.com"
;;                  (lambda (retrieved) t))))

(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; set package repo
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives melpa t)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa")

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package diminish)

(use-package eldoc
  :diminish eldoc-mode)

(use-package dracula-theme
  :if window-system
  :config
  (load-theme 'dracula t))
;; end of packages - load emacs packages
;; end of init.el
