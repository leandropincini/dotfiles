;;; packages.el --- load emacs packages

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
(setq use-package-always-pin "melpa-stable")

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
  :pin melpa-stable
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/Projects/")
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
  (setq which-key-idle-delay 0.2))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magic
  :disabled)

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :pin gnu
  :diminish
  :hook ((prog-mode . rainbow-mode)))

(use-package company
  :bind (("C-c /" . company-complete))
  :config
  (setq company-idle-delay 0.2
        company-show-numbers t
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

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . cljure-mode))
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
        cider-repl-display-help-banner nil
        cider-ns-refresh-show-logger-buffer t
        cider-show-error-buffer t
        cider-font-lock-dynamically nil
        cider-eldoc-display-for-symbol-at-point nil
        cider-prompt-for-symbol nil
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)
  (setq cider-test-defining-forms '("deftest" "defspec" "defflow"))
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

(use-package treemacs)

(use-package treemacs-all-the-icons
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
  :pin melpa
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (java-mode . lsp)
         (clojurescript-mode . lsp)
         ;(lsp-mode . lsp-enable-which-key-intergration)
         )
  :commands lsp
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
  (setq lsp-headerline-breadcrumb-enable nil
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
  :mode (("\\.md'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
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

(use-package yaml-mode)

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(use-package xml-mode
  :ensure nil
  :mode ("\\.wsdl\\'" . xml-mode))

(use-package web-mode
  :mode ("\\.phtml\\'" . web-mode)
  :mode ("\\.tpl\\.php\\'" . web-mode)
  :mode ("\\.[agj]sp\\'" . web-mode)
  :mode ("\\.erbl\\'" . web-mode)
  :mode ("\\.mustache\\'" . web-mode)
  :mode ("\\.djhtml\\'" . web-mode)
  :mode ("\\.html?\\'" . web-mode))

(use-package dockerfile-mode
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode)

(use-package feature-mode)

(use-package go-mode)

(use-package org
  :bind (:map org-mode-map
              ("C-c o l" . org-store-link)
              ("C-c o a" . org-agenda)
              ("C-c o c" . org-capture)
              ("C-c o b" . org-switchb))
  :config (setq org-log-done 'time))

(use-package dracula-theme
  :if window-system
  :config
  (load-theme 'dracula t))

(provide 'packages)
;;; packages.el ends here
