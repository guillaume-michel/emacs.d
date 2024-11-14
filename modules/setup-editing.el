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

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; show matching paren
(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; Package: smartparens
(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (sp-use-paredit-bindings))

;; highlight the current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#202020")
(set-face-foreground 'highlight nil)

;; ;; folding
;; (add-hook 'c-mode-common-hook   'hs-minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'java-mode-hook       'hs-minor-mode)
;; (add-hook 'lisp-mode-hook       'hs-minor-mode)
;; (add-hook 'perl-mode-hook       'hs-minor-mode)
;; (add-hook 'sh-mode-hook         'hs-minor-mode)
;; (add-hook 'python-mode-hook     'hs-minor-mode)
;; (add-hook 'latex-mode-hook      'hs-minor-mode)
;; (add-hook 'LaTeX-mode-hook      'hs-minor-mode)
;; (add-hook 'lua-mode-hook        'hs-minor-mode)

;; (defun my-toggle-hiding ()
;;   "custom toggle folding"
;;   (interactive)
;;   (hs-toggle-hidding))

;; (global-set-key (kbd "<f9>") (lambda () (interactive) (hs-toggle-hiding)))

(use-package origami
  :hook (prog-mode . origami-mode)
  :config
  (general-define-key
   "<f9>" '(origami-toggle-node :which-key "toggle origami hide/show node")))

(use-package lsp-origami
  :hook (lsp-after-open . lsp-origami-try-enable))

;; disable bell
(setq visible-bell 1)

;; disable alarm completely
(setq ring-bell-function 'ignore)

;; Package zygospore
(use-package zygospore
  :config
  (general-define-key
   "C-x 1" '(zygospore-toggle-delete-other-windows :which-key "toggle single window"))
  (my-leader-def
   "ts" '(zygospore-toggle-delete-other-windows :which-key "single window")))

;; CMake support
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :diminish
  :hook (cmake-mode . cmake-font-lock-activate))

(defun my-cmake-mode-hook ()
  (cmake-format-mode 1))

(require 'cmake-format)
;(add-hook 'cmake-mode-hook #'my-cmake-mode-hook)

;; (use-package cmake-format
;;   :init
;;   ;; optional: enable automatic formatting on save
;;   (add-hook 'cmake-mode-hook #'my-cmake-mode-hook)
;;   ;; :config
;;   ;; ;; optional:
;;   ;; (setq cmake-format-command "/path/to/cmake-format"
;;   ;;       cmake-format-args '("list" "of" "flags"))
;;   )

;; rainbow
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; GLSL
(use-package glsl-mode
  :mode ("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'" "\\.vsh\\'" "\\.fsh\\'"))

;; Dockerfile
(use-package dockerfile-mode)

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

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; git-timemachine
(use-package git-timemachine :defer t)

(use-package lua-mode
  :mode "\\.lua$")

(use-package ox-reveal
  :ensure ox-reveal
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0")
  (setq org-reveal-mathjax t))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

(use-package meson-mode
  :defer t)

(use-package terraform-mode
  ;; if using straight
  ;; :straight t

  ;; if using package.el
  ;; :ensure t
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(straight-use-package
  '(livedown :type git
             :host github
             :repo "shime/emacs-livedown"))

;; vertical indent highlighting
(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook
  (python-mode . highlight-indent-guides-mode))

(use-package multiple-cursors
  )

;; Org-mode
(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

(require 'org-tempo)


(use-package pickle)

(add-to-list 'auto-mode-alist '("\\.feature\\'" . pickle-mode))

(provide 'setup-editing)
