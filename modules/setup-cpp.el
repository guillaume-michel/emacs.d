;; LLVM stuff
(setq llvm-root "/usr/lib/llvm-13")
(setq my-clangd-executable (expand-file-name "bin/clangd" llvm-root))
(setq my-clang-check-executable (expand-file-name "bin/clang-check" llvm-root))

;; Google style by default
(use-package google-c-style
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

;; Cuda is considered C++
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; LSP with C++
(add-hook 'c++-mode-hook 'lsp-deferred)

(defun my-lsp-c++-hook ()
  "Configure clangd as C++ backend for lsp"
  (setq lsp-clients-clangd-executable my-clangd-executable
        lsp-clients-clangd-args (list (concat "--query-driver=" llvm-root "**") "-background-index" "--log=verbose" "--folding-ranges")))

(add-hook 'lsp-mode 'my-lsp-c++-hook)

(add-hook 'c++-mode-hook (lambda ()
                           (require 'dap-cpptools)))
;; Use clangcheck for flycheck in C++ mode
(defun my-select-clangcheck-for-checker ()
  "Select clang-check for flycheck's checker."
  (require 'flycheck-clangcheck)
  (flycheck-set-checker-executable 'c/c++-clangcheck my-clang-check-executable)
  (flycheck-select-checker 'c/c++-clangcheck))

(use-package flycheck-clangcheck
  :ensure t
  :config
  (setq flycheck-clangcheck-analyze t
        flycheck-clangcheck-extra-arg-before '("-std=c++2a")
        ;; flycheck-clangcheck-extra-arg '("-Xanalyzer" "-analyzer-output=text")
        )
  :hook (c++-mode . my-select-clangcheck-for-checker))


(provide 'setup-cpp)
