(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; Customized functions
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; show matching paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode t)
(smartparens-global-mode t)

;; highlight the current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#202020")
(set-face-foreground 'highlight nil)

;; line numbers
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

;; folding
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'caffe-mode-hook      'hs-minor-mode)
(add-hook 'latex-mode-hook      'hs-minor-mode)
(add-hook 'LaTeX-mode-hook      'hs-minor-mode)
(add-hook 'lua-mode-hook        'hs-minor-mode)

(defun my-toggle-hiding ()
  "custom toggle folding"
  (interactive)
  (hs-toggle-hidding))

(global-set-key (kbd "<f9>") (lambda () (interactive) (hs-toggle-hiding)))

;; disable bell
(setq visible-bell 1)

;; disable alarm completely
(setq ring-bell-function 'ignore)

(which-function-mode)

(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))

(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-format (delete (assoc 'which-func-mode
                                          mode-line-format) mode-line-format)
          header-line-format which-func-header-line-format)))

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))

;;(add-hook 'python-mode-hook     'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)

(defun display-buffer-window-below-and-shrink (buffer alist)
  (let ((window (or (get-buffer-window buffer)
                    (display-buffer-below-selected buffer alist))))
    (when window
      (fit-window-to-buffer window 80 00)
      (shrink-window-if-larger-than-buffer window)
      window)))

 ;; (add-hook 'flycheck-mode-hook (lambda ()
 ;;                               (add-to-list 'display-buffer-alist
 ;;                                            `(,(rx string-start (eval flycheck-error-list-buffer) string-end)
 ;;                                              (display-buffer-window-below-and-shrink . ((reusable-frames . t)))))))

;; (defadvice flycheck-error-list-refresh (around shrink-error-list activate)
;;   ad-do-it
;;   (-when-let (window (flycheck-get-error-list-window t))
;;     (with-selected-window window
;;       (fit-window-to-buffer window 80 00)
;;       (shrink-window-if-larger-than-buffer window))))

(defun flycheck-list-errors-only-when-errors ()
  (if flycheck-current-errors
      (flycheck-list-errors)
    (-when-let (buffer (get-buffer flycheck-error-list-buffer))
      (dolist (window (get-buffer-window-list buffer))
        (quit-window nil window)))))

;;(add-hook 'before-save-hook #'flycheck-list-errors-only-when-errors)

(defun lunaryorn-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(global-set-key (kbd "C-c q") #'lunaryorn-quit-bottom-side-windows)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; show column numbers in mode-line
(setq column-number-mode t)

;; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; rainbow
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'python-mode-hook     'rainbow-identifiers-mode)
;;(add-hook 'emacs-lisp-mode-hook 'rainbow-identifiers-mode)
;;(add-hook 'lisp-mode-hook       'rainbow-identifiers-mode)

;; Caffe prototxt
(autoload 'caffe-mode "caffe-mode" "Major mode for Caffe" t)
(add-to-list 'auto-mode-alist '("\\.prototxt\\'" . caffe-mode))

;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))

;; C++
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

;; modern cpp font lock
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; demangle mode
(add-hook 'llvm-mode-hook #'demangle-mode)

;; Cuda
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; Dockerfile
(require 'dockerfile-mode)

;; Scala
(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

;; Vertical split shows more of each line, horizontal split shows more lines.
;; This code toggles between them
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "<f8>") 'toggle-window-split)

;; flyspell
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)

(dolist (hook '(text-mode-hook latex-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))
;; (dolist (hook '(python-mode-hook))
;;       (add-hook hook (lambda () (flyspell-prog-mode))))

;; easy spell check
(global-set-key (kbd "<f7>") 'ispell-word)
(global-set-key (kbd "C-S-<f7>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f7>") 'flyspell-buffer)
(global-set-key (kbd "C-<f7>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f7>") 'flyspell-check-next-highlighted-word)

(if (file-exists-p "/usr/bin/hunspell")
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))


(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; git-timemachine
(use-package git-timemachine :defer t)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(use-package ox-reveal
  :ensure ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0")
(setq org-reveal-mathjax t)

(add-hook 'js-mode-hook 'prettier-js-mode)

(provide 'setup-editing)
