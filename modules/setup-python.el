(defun my-python-hook ()
  (setq python-shell-interpreter "python3"
        dap-python-executable "python3"
        dap-python-debugger 'debugpy)
  (run-python-internal)
  (lsp-deferred)
  (require 'dap-python))

(add-hook 'python-mode-hook 'my-python-hook)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/externals/python-type-stubs"))
  )

(provide 'setup-python)
