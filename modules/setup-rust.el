;; Conf for Rust programming
(setq cargo-root "~/.cargo")
(setq my-rls-executable (expand-file-name "bin/rls" cargo-root))

(defun my-lsp-rust-hook ()
  "Configure RLS as Rust backend for lsp"
  (setq lsp-rust-server my-rls-executable
        lsp-rust-rls-server-command my-rls-executable))

(use-package toml-mode)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred))

(add-hook 'lsp-mode 'my-lsp-rust-hook)

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'setup-rust)
