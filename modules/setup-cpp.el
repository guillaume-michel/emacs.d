;; LLVM stuff
(setq llvm-root "/usr/lib/llvm-14")
(setq my-clangd-executable (expand-file-name "bin/clangd" llvm-root))
(setq my-clang-check-executable (expand-file-name "bin/clang-check" llvm-root))

;; Google style by default
(use-package google-c-style
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;; adjust google style to respect my tab-width
(add-hook 'c++-mode-hook (lambda ()
                           (setq c-basic-offset tab-width)))

;; (use-package clang-format
;;   :after (s)
;;   :init
;;   (defun get-clang-format-option (config-str field is-num)
;;     "Retrieve a config option from a clang-format config.

;; CONFIG-STR is a string containing the entire clang-format config.
;; FIELD is specific option, e.g. `IndentWidth'.  IS-NUM is a
;; boolean that should be set to 1 if the option is numeric,
;; otherwise assumed alphabetic."
;;     (if is-num
;;         (let ((primary-match (s-match (concat "^" field ":[ \t]*[0-9]+") config-str)))
;;           (if primary-match
;;               (string-to-number (car (s-match "[0-9]+" (car primary-match))))
;;             0))
;;       (let ((primary-match (s-match (concat "^" field ":[ \t]*[A-Za-z]+") config-str)))
;;         (if primary-match
;;             (car (s-match "[A-Za-z]+$" (car primary-match)))
;;           ""))))
;;   :hook (c-mode-common . (lambda ()
;;                            (let* ((clang-format-config (shell-command-to-string "clang-format -dump-config"))
;;                                   (c-offset (get-clang-format-option clang-format-config "IndentWidth" t))
;;                                   (tabs-str (get-clang-format-option clang-format-config "UseTab" nil))
;;                                   (base-style (get-clang-format-option clang-format-config "BasedOnStyle" nil)))
;;                              (progn
;;                                (if (> c-offset 0)
;;                                    (setq-local c-basic-offset c-offset)
;;                                  (if (not (equal "" base-style))
;;                                      (cond ((or (equal "LLVM" base-style)
;;                                                 (equal "Google" base-style)
;;                                                 (equal "Chromium" base-style)
;;                                                 (equal "Mozilla" base-style)) (setq-local c-basic-offset 2))
;;                                            ((equal "WebKit" base-style) (setq-local c-basic-offset 4)))))
;;                                (if (not (equal "" tabs-str))
;;                                    (if (not (string-equal "Never" tabs-str))
;;                                        (setq-local indent-tabs-mode t)
;;                                      (setq-local indent-tabs-mode nil))
;;                                  (if (not (equal "" base-style))
;;                                      (cond ((or (equal "LLVM" base-style)
;;                                                 (equal "Google" base-style)
;;                                                 (equal "Chromium" base-style)
;;                                                 (equal "Mozilla" base-style)
;;                                                 (equal "WebKit" base-style)) (setq-local indent-tabs-mode nil))))))))))


(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; LSP with C++
(add-hook 'c++-mode-hook 'lsp-deferred)

(defun my-lsp-c++-hook ()
  "Configure clangd as C++ backend for lsp"
  (setq lsp-clients-clangd-executable my-clangd-executable
        lsp-clients-clangd-args (list (concat "--query-driver=" llvm-root "**") "-background-index" "--log=verbose" "--folding-ranges" "--clang-tidy" "--inlay-hints" "-j$(($(nproc) / 2))")))

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
