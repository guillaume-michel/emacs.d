(add-hook 'c++-mode-hook 'lsp-deferred)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; clang-format
(require 'clang-format)
;; (define-key c++-mode-map (kbd "<C-M-tab>") 'clang-format-buffer)
(fset 'c-indent-region 'clang-format-region)

(provide 'setup-cpp)
