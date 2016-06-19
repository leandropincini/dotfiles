;; proxy.el - configure emacs proxy

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
	("http" . "proxy.com:8080")
	("https" . "proxy.com:8080")))

(setq url-http-proxy-basic-auth-storage
      (list (list "proxy.com:8080"
		  (cons "Input your LDAP UID !"
			(based64-encode-string "USER:PASS")))))

(provide 'proxy)

;; end of proxy.el
