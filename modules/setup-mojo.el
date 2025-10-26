(use-builtin-package eglot
  :ensure t
    :defer t
    :hook ((mojo-mode . eglot-ensure))
    :config
    (add-to-list 'eglot-server-programs '(mojo-mode . ("mojo-lsp-server"))))

(provide 'setup-mojo)
