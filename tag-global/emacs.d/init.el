;;; init.el --- emacs configuration

;; only runs on emacs >= 27
(let ((minver 27))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old --this configuration requires v%s or higher" minver)))

;; add configuration sub-directories
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; local path
(add-to-list 'exec-path "/usr/local/bin")

;; run the server if gnu/linux
(when (eq system-type 'gnu/linux)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;By default, the Emacs shell will show raw escape sequences used to print colors. In other words, it will display strange symbols in place of the desired colored output.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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
;; enable line numbers globally
(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; end of visual-configs - emacs visual configs

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
  "When our source file uses tabs, we use tabs, if spaces spaces.
If neither, we use the current indent-tabs-mode (spaces)."
  (let ((space-count (how-many-region (point-min) (point-max) "^ "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(add-hook 'prog-mode-hook
  (lambda ()
    (unless (derived-mode-p 'go-mode)
      (infer-indentation-style))))

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

(use-package emacs
  :config
  ;; UI related
  (setq inhibit-startup-message t)
  (setq ring-bell-function 'ignore)
  (setq visible-bell -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode t)
  (global-hl-line-mode 1)
  (show-paren-mode 1)
  (toggle-frame-maximized)

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

  ;; Performance optmizations
  (setq load-prefer-newer t) ;; Always load newest byte code
  (setq gc-cons-threshold (* 100 1024 1024)) ;; GC Threshold 100MB
  (setq large-file-warning-threshold (* 100 1024 1024)) ;; Warn at 100MB files

  ;; Editor behavior
  (fset 'yes-or-no-p 'y-or-n-p) ;; y-or-n instead of yes-or-no
  (setq shift-select-mode nil) ;; Classic select
  (setq-default indent-tabs-mode nil) ;; Indent with spaces by default

)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package diminish)

(use-package eldoc
  :diminish eldoc-mode)

(use-package subword
  :diminish subword-mode
  :hook ((clojure-mode . subword-mode)))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-c m" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy
  :after counsel
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t)

  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist)
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-rich
  :after ivy
  :init
  (setq ivy-format-function #'ivy-format-function-line
        ivy-rich-path-style 'abbrev)
  (setq ivy-rich-display-transformers-list
        '(ivy-witch-buffer
          (:columns ((ivy-rich-switch-buffer-icon (:width 2))
                     (ivy-rich-candidate (:width 40))
                     (ivy-rich-switch-buffer-size (:width 7))
                     (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                     (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                     (ivy-rich-switch-buffer-project (:width 15 :face sucess))
                     (ivy-rich-switch-buffer-path (:witdht (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                    :predicate (lambda (cand) (if-let ((buffer (get-buffer cand)))
                                                  (with-current-buffer buffer
                                                    (not (derived-mode-p 'exwm-mode))))))
          counsel-find-file
          (:columns ((ivy-read-file-transformer)
                     (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
          counsel-M-x
          (:columns ((counsel-M-x-transformer (:width 35))
                     (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns ((counsel-describe-function-transformer (:width 35))
                     (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns ((counsel-describe-variable-transformer (:width 35))
                     (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns ((ivy-rich-candidate (:width 25))
                     (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
                     (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
                     (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))
          counsel-projectile-find-file
          (:columns ((ivy-rich-switch-buffer-icon (:width 2))
                     (ivy-rich-candidate (:width 30))
                     (ivy-rich-switch-buffer-size (:width 7))
                     (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                     (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                     (ivy-rich-switch-buffer-project (:width 15 :face success))
                     (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate (lambda (cand) (get-buffer cand))))))

(use-package helpful
  :after counsel
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :pin melpa
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/projects/")
        projectile-enable-caching nil)
  :demand t
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ripgrep)

(use-package projectile-ripgrep
  :after projectile ripgrep)

(use-package counsel-projectile
  :after projectile-ripgrep
  :config
  (counsel-projectile-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magic
  :disabled)

(use-package smartparens
  :hook (go-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (go-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :pin gnu
  :diminish
  :hook ((prog-mode . rainbow-mode)))

(use-package company
  :bind (("C-c /" . company-complete))
  :hook (go-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-show-quick-access t
        company-echo-delay 0
        company-candidates-cache t
        company-tooltip-limit 10
        company-icon-size 20
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package flyspell
  :hook ((clojure-mode . flyspell-mode)
         (text-mode . flyspell-mode)
         (program-mode . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

(use-package flycheck
  :hook ((after-init . global-flycheck-mode)))

(use-package graphql-mode)

(use-package clojure-mode
  :mode "\\.clj\\'"
  :mode "\\.edn\\'"
  :config
  (setq default-fill-column 80
        clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode
  :diminish clojure-mode-extra-font-locking)

(use-package cider
  :after clojure-mode
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . company-mode))
  :config
  (setq nrepl-log-messages t
        cider-lein-command "lein"
        cider-repl-display-help-banner nil
        cider-ns-refresh-show-logger-buffer t
        cider-show-error-buffer t
        cider-font-lock-dynamically nil
        cider-eldoc-display-for-symbol-at-point nil
        cider-prompt-for-symbol nil
        cider-use-xref nil
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)
  (setq cider-test-defining-forms '("deftest" "defspec" "defflow"))
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-copmlete-at-point)))
  :bind
  (:map clojure-mode-map
        ("C-c c s"   . cider-jack-in)
        ("C-c c n r" . cider-ns-reload-all)
        ("C-c c c"   . comment-region)
        ("C-c c u"   . uncomment-region)
        ("C-c c i"   . indent-region)))

(use-package clj-refactor
  :after clojure-mode
  :hook ((clojure-mode . clj-refactor-mode))
  :init
  (setq cljr-warn-on-eval nil
        clojure-thread-all-but-last t
        cljr-magic-require-namespaces
        '(("s" . "schema.core")
          ("d" . "datomic.api")
          ("m" . "matcher-combinators.matchers")
          ("pp" . "clojure.pprint")))
  :config
  (setq cljr-add-ns-to-blank-clj-files nil
        cljr-eagerly-build-asts-on-startup nil))

(use-package flycheck-joker
  :after flycheck-mode)

(use-package flycheck-clojure
  :after flycheck-mode)

(use-package lsp-java
  :after lsp
  :config
  (setq lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t
        lsp-file-watch-ignored-directories
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build")))

(use-package dap-mode
  :after lsp-mode
  ;; :defer
  ;; :custom
  ;; (dap-auto-configure-mode t)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
;;  (require 'dap-java)
  (require 'dap-hydra)
  (require 'dap-dlv-go)
  (dap-register-debug-template
    "Go Debug Current File"
    (list :type "go"
          :request "launch"
          :name "Launch File"
          :mode "auto"
          :program "${fileDirname}"
          :buildFlags nil
          :args nil
          :env nil
          :envFile nil))
  :custom
  (dap-dlv-go-delve-path (executable-find "dlv")))

(use-package treemacs
  :config
  (setq treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)))

(use-package treemacs-all-the-icons
  :if (display-graphic-p)
  :after treemacs)

(use-package lsp-treemacs
  :after treemacs
  :init
  (setq treemacs-space-between-root-nodes nil)
  :config
  (setq lsp-treemacs-error-list-current-project-only t)
  :bind
  (:map lsp-mode-map
        ("C-c l t e" . lsp-treemacs-error-list)))

(use-package lsp-mode
  :hook ((go-mode . lsp-deferred)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (java-mode . lsp)
         (clojurescript-mode . lsp)
         ;(lsp-mode . lsp-enable-which-key-intergration)
         )
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-enable-indentation nil
        lsp-prefer-flymake nil
        lsp-log-io t
        lsp-enable-file-watchers t
        lsp-enable-symbol-highlighting nil
        lsp-lens-enable t
        lsp-semantic-tokens-enable t
        lsp-file-watch-threshold 15000
        lsp-signature-auto-activate nil)
  :custom
  ((lsp-clojure-server-command '("bash" "-c" "clojure-lsp")))
  :config
  (setq lsp-enable-file-watchers nil
        lsp-gopls-complete-unimported t
        lsp-gopls-staticcheck t
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-semantic-tokens-enable t
        lsp-idle-delay 0.2
        lsp-use-plist nil
        lsp-completion-sort-initial-results t
        lsp-completion-no-cache t
        lsp-completion-use-last-result nil)
  (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :bind
  (:map lsp-mode-map
        ("C-c l f r" . lsp-find-references)
        ("C-c l f d" . lsp-find-definition)
        ("C-c l f u" . pop-tag-mark)))

(use-package lsp-ivy
  :after ivy
  :ensure nil
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60
        lsp-ui-doc-enable nil
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

(use-package markdown-mode
  :mode ("\\.md'" . gfm-mode)
  :mode ("\\.markdown\\'" . gfm-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :preface
  (defun jekyll-insert-image-url ()
    (interactive)
    (let* ((files (directory-files "../assets/images"))
            (selected-file (completing-read "Select image: " files nil t)))
         (insert (format "![%s](/assets/images/%s)" selected-file selected-file))))
  (defun jekyll-insert-post-url ()
    (interactive)
    (let* ((files (remove "." (mapcar #'file-name-sans-extension (directory-files "."))))
           (selected-file (completing-read "Select article: " files nil t)))
      (insert (format "{%% post_url %s %%}" selected-file)))))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package xml-mode
  :ensure nil
  :mode "\\.wsdl\\'")

(use-package web-mode
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.erbl\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :mode "\\.html?\\'")

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package docker-compose-mode)

(use-package feature-mode)

(use-package go-mode
  :mode "'\\.go\\'"
  :custom
  (tab-width 4)
  :init
  (defun personalized-go-mode-setup ()
    (setq-local indent-tabs-mode t)
    (setq-local tab-width 4)
    (setq-local standard-indent 4)
    (setq-local go-tab-width 4)
    (setq whitespace-style '(face tabs tab-mark trailing))
    (setq-local gofmt-command "gofmt")
    (setq-local gofmt-args nil)
    (setq-local golang-format-on-save t)
    (setq-local tab-stop-list (number-sequence 4 200 4)))
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

(use-package protobuf-mode
  :mode "'\\.proto\\'")

(use-package org
  :bind (:map org-mode-map
              ("C-c o l" . org-store-link)
              ("C-c o a" . org-agenda)
              ("C-c o c" . org-capture)
              ("C-c o b" . org-switchb))
  :config (setq org-log-done 'time))

;; default font and size
(when (eq system-type 'gnu/linux)
  (defun lp/set-terminal-font-faces ()
    (set-face-attribute 'default nil
                        :family "Fira Code Retina"
                        :height 120
                        :weight 'normal
                        :width 'normal))
  (defun lp/set-ui-font-faces ()
    (set-face-attribute 'default nil
                        :family "Fira Code Retina"
                        :height 120)
    (set-face-attribute 'fixed-pitch nil
                        :family "Fira Code Retina"
                        :height 120)
    (set-face-attribute 'variable-pitch nil
                        :family "Fira Code Retina"
                        :height 120))
  (if (daemonp)
      (and (add-hook 'after-make-frame-functions
                     (lambda (frame)
                       (with-selected-frame frame
                         (lp/set-ui-font-faces))))
           (use-package dracula-theme
             :config (load-theme 'dracula t)))
      (lp/set-terminal-font-faces)))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil
                      :family "FiraCode Nerd Font Mono"
                      :height 120
                      :weight 'Regular
                      :width 'Regular))

;; (use-package catppuccin-theme
;;   :if window-system
;;   :config
;;   (load-theme 'catppuccin :no-confirm)
;;   (setq catppuccin-flavor 'macchiato))

(use-package dracula-theme
  :if window-system
  :config
  (load-theme 'dracula t))
;; end of packages - load emacs packages

;;; init.el ends here
