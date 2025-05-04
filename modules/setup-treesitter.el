;; We use the builtin tree-sitter modules but we still grammars
;; treesit-auto install the grammars for us

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(
                        bash
                        c
                        cmake
                        cpp
                        make
                        markdown
                        python
                        yaml
                        ))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'setup-treesitter)
