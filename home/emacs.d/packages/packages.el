;; packages.el - load emacs packages

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

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar _packages
  '(auto-package-update
    org
    use-package
    editorconfig
    projectile
    dracula-theme
    which-key
    move-text
    magit
    paredit
    rainbow-delimiters
    company
    dockerfile-mode
    docker-compose-mode
    feature-mode
    go-mode
    clojure-mode
    flycheck-joker
    clj-refactor
    lsp-mode
    yasnippet
    cider
    markdown-mode
    yaml-mode
    web-mode
    ivy))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      _packages)

;; use-package configs
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; editorconfig configs
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; projectile configs
(use-package projectile
  :pin melpa-stable
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-project-search-path '("~/Projects/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; ivy configs
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; wichkey configs
(use-package which-key
  :config
  (which-key-mode +1))

;; move-text configs
(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

;; magit-configs
(use-package magit
  :bind (("C-x g" . magit-status)))

;; paredit configs
(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

;; rainbow-delimiters configs
(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)))

;; testing
(use-package rainbow-mode
  :hook ((prog-mode . rainbow-mode)))

;; company-mode configs
(use-package company
  :bind (("C-c /" . company-complete))
  :config
  (setq company-idle-delay 0.2)
  (setq company-show-numbers t)
  ;(setq company-echo-delay 0)
  ;(setq company-candidates-cache t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the competion popup-isear-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

;; testing
(use-package flyspell
  :hook ((clojure-mode . flyspell-mode)
         (text-mode . flyspell-mode)
         (program-mode . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; testing
(use-package flycheck
  :hook ((after-init . global-flycheck-mode)))

;; clojure-mode configs
(use-package clojure-mode
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (setq default-fill-column 80)
  (define-clojure-indent
    (fact 1)
    (facts 1)
    (flow 1)
    (fnk 1)
    (provided 1)
    (clojure.test.check/quick-check 2)
    (clojure.test.check.properties/for-all 2)))

;; cider configs
(use-package cider
  :after clojure-mode
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . company-mode))
  :config
  (setq nrepl-log-messages t
        cider-repl-display-help-banner nil))

;; org-mode configs
(global-set-key "\C-ccs" 'cider-jack-in)
(global-set-key "\C-cce" 'cider-eval-buffer)
(global-set-key "\C-ccf" 'cider-eval-last-sexp)
(global-set-key "\C-ccnr" 'cider-ns-reload-all)

;; testing
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

;; flycheck-joker configs
(use-package flycheck-joker)

;; flycheck-clojure configs
(use-package flycheck-clojure)

;; lsp-mode
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
        lsp-enable-symbol-highlighting nil)
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
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

;; testing
(use-package lsp-ivy
  :ensure nil
  :commands lsp-ivy-workspace-symbol)

;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1))

;; markdown-mode configs
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

;; yaml-mode configs
(use-package yaml-mode)

;; testing
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; xml mode configs
(use-package xml-mode
  :ensure nil
  :mode ("\\.wsdl\\'" . xml-mode))

;; web-mode configs
(use-package web-mode
  :mode ("\\.phtml\\'" . web-mode)
  :mode ("\\.tpl\\.php\\'" . web-mode)
  :mode ("\\.[agj]sp\\'" . web-mode)
  :mode ("\\.erbl\\'" . web-mode)
  :mode ("\\.mustache\\'" . web-mode)
  :mode ("\\.djhtml\\'" . web-mode)
  :mode ("\\.html?\\'" . web-mode))

;; Dockerfile-mode configs
(use-package dockefile-mode
  :ensure nil
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

;; docker-comopose-mode configs
(use-package docker-compose-mode)

;; org-mode configs
(global-set-key "\C-col" 'org-store-link)
(global-set-key "\C-coa" 'org-agenda)
(global-set-key "\C-coc" 'org-capture)
(global-set-key "\C-cob" 'org-switchb)
(setq org-log-done 'time)

(provide 'packages)
;;; packages.el ends here
