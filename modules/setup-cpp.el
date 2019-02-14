;; define custom style: "guillaume"
(c-add-style "guillaume"
             '("linux"
               (c-basic-offset . 4) ; 1 ident is 4 spaces
               (c-offsets-alist
                (innamespace . [0]) ; do not ident inside namespaces
                (case-label . +)    ; indent case labels by 1 ident
                )))

;; set style to "guillaume"
(setq c-default-style "guillaume")

;; This hack fixes indentation for C++11's "enum class" and other minor issues with template member functions in Emacs.
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class/6550361#6550361

(defun inside-template-p (pos)
  "Checks if POS is within a template definition"
  (ignore-errors
    (save-excursion
      (goto-char pos)
      ;;(message (thing-at-point 'line))
      (looking-at "template[ \t]*<.*>"))))

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]*"))))

(defun custom-topmost-intro-cont (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun custom-statement-cont (langelem)
  (cond ((inside-class-enum-p (c-langelem-pos langelem))
         ;;(message "inside enum class!!!!!!")
         '-)
        ((inside-template-p (c-langelem-pos langelem))
         ;;(message "inside template!!!!!!")
         0)
        (t
         ;;(message "default for statement-cont!!!!!!")
         '+)))

(defun fix-cpp11-indentation ()
  "Setup `c++-mode' to better handle C++11 code indentation"
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . custom-topmost-intro-cont))
  (add-to-list 'c-offsets-alist '(statement-cont . custom-statement-cont)))

(add-hook 'c++-mode-hook 'fix-cpp11-indentation)

;; Fix indentation issue for lambda
;; (defadvice c-lineup-arglist (around my activate)
;;   "Improve indentation of continued C++11 lambda function opened as argument."
;;   (setq ad-return-value
;;         (if (and (equal major-mode 'c++-mode)
;;                  (ignore-errors
;;                    (save-excursion
;;                      (goto-char (c-langelem-pos langelem))
;;                      ;; Detect "[...](" or "[...]{". preceded by "," or "(",
;;                      ;;   and with unclosed brace.
;;                      (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
;;             0                           ; no additional indent
;;           ad-do-it)))                   ; default behavior

;; clang-format
(require 'clang-format)
;; (define-key c++-mode-map (kbd "<C-M-tab>") 'clang-format-buffer)
;; (fset 'c-indent-region 'clang-format-region)

;; (defun clang-format-buffer-smart ()
;;   "Reformat buffer if .clang-format exists in the projectile root."
;;   (interactive)
;;   (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
;;     (clang-format-buffer)))

;; (defun clang-format-region-smart (start end)
;;   "Reformat region if .clang-format exists in the projectile root."
;;   (interactive)
;;   (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
;;     (clang-format-region start end)))

;; (add-hook
;;  'c++-mode-hook
;;  (lambda ()
;;    (local-set-key (kbd "C-M-\\") #'clang-format-region)
;;    (local-set-key (kbd "C-i") #'clang-format-buffer)))

;; (add-hook
;;  'c++-mode-hook
;;  (lambda ()
;;    (local-set-key (kbd "<tab>") #'clang-format-region)
;;    (local-set-key (kbd "C-i") #'clang-format-buffer)))

;; (add-hook
;;  'c++-mode-hook
;;  (lambda ()
;;    (local-set-key (kbd "C-i") #'clang-format-buffer)))

(provide 'setup-cpp)
