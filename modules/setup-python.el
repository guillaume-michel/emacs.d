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
                          (lsp-deferred))))

(provide 'setup-python)
