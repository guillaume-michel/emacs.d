(setq llvm-root "/usr/lib/llvm-11")
(setq my-clangd-executable (expand-file-name "bin/clangd" llvm-root))
(setq my-clang-check-executable (expand-file-name "bin/clang-check" llvm-root))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (setq lsp-enable-indentation t
        lsp-auto-guess-root t
        lsp-clients-clangd-executable my-clangd-executable
        lsp-clients-clangd-args (list (concat "--query-driver=" llvm-root "**") "-background-index")
        lsp-prefer-flymake nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-max-width 100)
  (lsp-ui-doc-max-height 13)
  (lsp-ui-doc-delay 2))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-lsp
  :after company
  :ensure t
  :config
  (require 'company-lsp)
  (push 'company-lsp company-backends))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Header completion
(use-package company-c-headers
  :ensure t
  :config
  (push 'company-c-headers company-backends))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

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
        flycheck-clangcheck-extra-arg '("-Xanalyzer" "-analyzer-output=text"))
  :hook (c++-mode . my-select-clangcheck-for-checker))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

(defcustom python-shell-interpreter "python3"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python)

(add-hook 'python-mode-hook 'run-python-internal)

(provide 'setup-completion)
