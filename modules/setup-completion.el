(require 's)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  ;;:commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (setq lsp-enable-indentation t
        lsp-semantic-tokens-enable nil
        lsp-auto-guess-root t
        lsp-prefer-flymake nil)
  :config
  (lsp-enable-which-key-integration t)
  ;; (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
  ;;   (-let* (((&hash "value") contents)
  ;;           (groups (--partition-by (s-blank? it) (s-lines value)))
  ;;           (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
  ;;                          (-third-item groups)
  ;;                        (car groups)))
  ;;           (sig (--> sig_group
  ;;                     (--drop-while (s-equals? "```rust" it) it)
  ;;                     (--take-while (not (s-equals? "```" it)) it)
  ;;                     (s-join "" it))))
  ;;     (lsp--render-element (concat "```rust\n" sig "\n```"))))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-max-width 200)
  (lsp-ui-doc-max-height 13)
  (lsp-ui-doc-delay 2)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

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
  (company-idle-delay 0.500))

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

;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.config/emacs/snippets"))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))

;; (use-package lsp-sonarlint
;;   :custom
;;   ;; Allow sonarlint to download and unzip the official VSCode extension
;;   ;; If nil, you'll have to do that yourself. See also `lsp-sonarlint-download'
;;   ;; `lsp-sonarlint-download-url' and `lsp-sonarlint-download-dir'
;;   (lsp-sonarlint-auto-download t)

;;   ;; Choose which analyzers you want enabled. By default all are enabled
;;   ;; See command `lsp-sonarlint-available-analyzers' for the full list.
;;   (lsp-sonarlint-enabled-analyzers '("cfamily" "python"))

;;   (lsp-sonarlint-cfamily-compile-commands-path "${workspaceFolder}/build/compile_commands.json"))


(provide 'setup-completion)
