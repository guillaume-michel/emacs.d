;; Conf for Rust programming

(use-package rustic
  :ensure
  ;; :bind (:map rustic-mode-map
  ;;             ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  ;; (add-hook 'rustic-mode-hook 'lsp-rust-analyzer-inlay-hints-mode)
  )

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(setq cargo-root "~/.cargo")

(defun my-lsp-rust-hook ()
  "Configure Rust backend for lsp"
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil
        lsp-rust-analyzer-proc-macro-enable t

        ;; lsp-rust-analyzer-completion-add-call-parenthesis nil

        ;; lsp-rust-server my-rls-executable
        ;; lsp-rust-rls-server-command my-rls-executable
        ))


;; (setq my-rls-executable (expand-file-name "bin/rls" cargo-root))

;; (defun my-lsp-rust-hook ()
;;   "Configure RLS as Rust backend for lsp"
;;   (setq lsp-rust-server my-rls-executable
;;         lsp-rust-rls-server-command my-rls-executable))

(use-package toml-mode)

;; (use-package rust-mode
;;   :ensure t
;;   :hook (rust-mode . lsp-deferred))

;; (add-hook 'lsp-mode 'my-lsp-rust-hook)

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rustic-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; configure repl
;; based on: https://github.com/SerialDev/evcxr-mode

;; needed for evcxr
(use-package parsec
  :ensure t)

(straight-use-package
 '(evcxr
   :type git
   :host github
   :repo "serialdev/evcxr-mode"
   :config
   (add-hook 'rustic-mode-hook #'evcxr-minor-mode)
))

(provide 'setup-rust)
