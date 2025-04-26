;; disable package.el to be loaded
;; we are using straight.el
(setq package-enable-at-startup nil)

;; automatically compile elc files
(setq comp-deferred-compilation t)

;; for LSP booster
;; lsp-mode will use plist instead of hash-table
(setenv "LSP_USE_PLISTS" "true")
