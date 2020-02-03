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
(eval-when-compile
  (require 'use-package))

;; editorconfig configs
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; projectile configs
(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-project-search-path '("~/Projects/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; ivy configs
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; wichkey configs
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; move-text configs
(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

;; magit-configs
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; paredit configs
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook  #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;; rainbow-delimiters configs
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

;; company-mode configs
(use-package company
  :ensure t
  :bind (("C-c /" . company-complete))
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the competion popup-isear-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode +1))

;; clojure-mode configs
(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; cider configs
(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'clojure-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode))

;; flycheck-joker configs
(use-package flycheck-joker
  :ensure t)

;; markdown-mode configs
(use-package markdown-mode
  :ensure t
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
(use-package yaml-mode
  :ensure t)

;; xml mode configs
(use-package xml-mode
  :mode ("\\.wsdl\\'" . xml-mode))

;; web-mode configs
(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'" . web-mode)
  :mode ("\\.tpl\\.php\\'" . web-mode)
  :mode ("\\.[agj]sp\\'" . web-mode)
  :mode ("\\.erbl\\'" . web-mode)
  :mode ("\\.mustache\\'" . web-mode)
  :mode ("\\.djhtml\\'" . web-mode)
  :mode ("\\.html?\\'" . web-mode))

;; Dockerfile-mode configs
(use-package dockefile-mode
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

;; docker-comopose-mode configs
(use-package docker-compose-mode
  :ensure t)

;; org-mode configs
(global-set-key "\C-col" 'org-store-link)
(global-set-key "\C-coa" 'org-agenda)
(global-set-key "\C-coc" 'org-capture)
(global-set-key "\C-cob" 'org-switchb)
(setq org-log-done 'time)

(provide 'packages)
;; end of packages.el
