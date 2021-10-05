;; (use-package cuda-mode
;;   :ensure t
;;   ;; :hook (cuda-mode . lsp-deferred)
;;   )

;; (add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))


(provide 'setup-cuda)
