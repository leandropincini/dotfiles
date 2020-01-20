;; mac-configs.el - emacs configs for mac osx

;; sets fn-delete to be right-delete
(global-set-key [kp-delete] 'delete-char)

(set-face-attribute 'default nil
  :family "Menlo"
  :height 160
  :weight 'normal
  :width 'normal)

(provide 'mac-configs)
;; end of mac-configs.el
