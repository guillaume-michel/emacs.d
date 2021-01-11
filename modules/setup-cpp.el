(add-hook 'c++-mode-hook 'lsp-deferred)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(provide 'setup-cpp)
