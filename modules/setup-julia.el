(use-package julia-mode
  :ensure t
  :hook (julia-mode . lsp-deferred))

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode))

(use-package lsp-julia
  :ensure t
  :config
  (setq lsp-julia-package-dir nil
        lsp-julia-default-environment "~/.julia/environments/v1.6"))

(use-package flycheck-julia
  :config (add-hook 'flycheck-mode-hook #'flycheck-julia-setup))


(provide 'setup-julia)
