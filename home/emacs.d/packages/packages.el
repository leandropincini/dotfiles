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

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; set package repo
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives gnu t)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package diminish)

(use-package eldoc
  :diminish eldoc-mode)

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
        enable-recursive-minibuffers t))

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
  (setq projectile-project-search-path '("~/Projects/"))
  :bind-keymap
  ("C-c p" . projectile-command-map))

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
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (setq default-fill-column 80))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package cider
  :after clojure-mode
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . company-mode))
  :config
  (setq nrepl-log-messages t
        cider-repl-display-help-banner nil)
  :bind
  (:map clojure-mode-map
        ("C-c c s"   . cider-jack-in)
        ("C-c c e"   . cider-eval-buffers)
        ("C-c c f"   . cider-eval-last-sexp)
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
          ("pp" . "clojure.pprint")))
  :config
  (setq cljr-add-ns-to-blank-clj-files nil))

(use-package flycheck-joker)

(use-package flycheck-clojure)

(use-package lsp-treemacs
  :init
  (setq treemacs-space-between-root-nodes nil)
  :bind
  (:map lsp-mode-map
        ("C-c l t e" . lsp-treemacs-error-list)))

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         ;(lsp-mode . lsp-enable-which-key-intergration)
         )
  :commands lsp
  :init
  (setq lsp-enable-indentation nil
        lsp-prefer-flymake nil
        lsp-log-io t
        lsp-enable-symbol-highlighting nil
        lsp-lens-enable t
        lsp-signature-auto-activate nil)
  :custom
  ((lsp-clojure-server-command '("bash" "-c" "clojure-lsp")))
  :config
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
        ("C-c l f d" . lsp-find-definition)))

(use-package lsp-ivy
  :after ivy
  :ensure nil
  :commands lsp-ivy-workspace-symbol)

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
