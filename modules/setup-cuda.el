;; (use-package cuda-mode
;;   :ensure t
;;   ;; :hook (cuda-mode . lsp-deferred)
;;   )

;; (add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-ts-mode))


(provide 'setup-cuda)
