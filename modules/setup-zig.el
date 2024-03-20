(use-package zig-mode
  :config
  (setq lsp-zig-zls-executable "~/.local/zls"))

(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(add-hook 'zig-mode-hook 'lsp-deferred)


(provide 'setup-zig)
