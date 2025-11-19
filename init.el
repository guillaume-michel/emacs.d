;;; init.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Test

;; Increase the number of bytes that are read by default from the process
(setq read-process-output-max (* 10 1024 1024))

;; ------------------- INIT PACKAGES --------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight when using use-package
;; so we don't have to specify `:straight t` each time
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(defmacro use-builtin-package (name &rest args)
  "Forces `use-package' to use builtin package instead of using external one
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     :straight (:type built-in)
     ,@args))

;; We need to import this package to add package archives.
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; EMACS
;;  This is biggest one. Keep going, plugins (oops, I mean packages) will be shorter :)
(use-builtin-package emacs
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (auto-save-default nil)                         ;; Disable automatic saving of buffers.
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (delete-selection-mode 1)                       ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative)           ;; Use relative line numbering in programming modes.
  (display-line-numbers-width 4)                  ;; minimum line number column width
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (history-length 25)                             ;; Set the length of the command history.
  (indent-tabs-mode nil)                          ;; Disable the use of tabs for indentation (use spaces instead).
  (inhibit-startup-message t)                     ;; Disable the startup message when Emacs launches.
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files nil)                         ;; Disable creation of backup files.
  ;; (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  ;; (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  ;; scroll one line at a time (less "jumpy" than defaults)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (scroll-step 1) ;; keyboard scroll one line at a time
  ;; nice scrolling
  (scroll-margin 10)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1)

  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines nil)                            ;; Disable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)              ;; Set the minimum level of warnings to display.
  (large-file-warning-threshold (* 50 1024 1024)) ;; warn when opening files bigger than the threshold
  ;; Copy/paste stuff (Do NOT work on terminal Emacs)
  (select-enable-clipboard t)
  (select-enable-primary t)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)

  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.
  (text-mode . display-line-numbers-mode)         ;; Enable line numbers in text modes.
  (conf-mode . display-line-numbers-mode)         ;; Enable line numbers in conf modes.
  (org-mode . (lambda () (display-line-numbers-mode 0)))         ;; Disable line numbers in org mode.

  :config
  ;; Configure font settings based on the operating system.
  ;; Ok, this kickstart is meant to be used on the terminal, not on GUI.
  ;; But without this, I fear you could start Graphical Emacs and be sad :(
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 90)
  ;; before:
  ;; (set-face-attribute 'default nil :height 90)
  ;; (set-face-attribute 'fixed-pitch nil :height 90)
  ;; (set-face-attribute 'variable-pitch nil :height 90 :weight 'regular)

  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  ;; You can M-x customize, M-x customize-group, or M-x customize-themes, etc.
  ;; The saves you do manually using the Emacs interface would overwrite this file.
  ;; The following makes sure those customizations are in a separate file.
  (setq custom-file (locate-user-emacs-file "custom.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                  ;; Load the custom file quietly, ignoring errors.

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init                        ;; Initialization settings that apply before the package is loaded.
  (global-hl-line-mode 1)                         ;; Enable highlight of the current line
  (set-face-background 'hl-line "#202020")        ;; more discret background for highlighted line
  (set-face-foreground 'highlight nil)            ;; more discret
  (global-auto-revert-mode 1)                     ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (recentf-mode 1)                                ;; Enable tracking of recently opened files.
  (savehist-mode 1)                               ;; Enable saving of command history.
  (save-place-mode 1)                             ;; Enable saving the place in files for easier return.
  (winner-mode 1)                                 ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)                            ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)                       ;; Enable shadowing of filenames for clarity.
  (set-fringe-mode 10)                            ;; Give some breathing room
  (toggle-frame-maximized)                        ;; maximize frame on startup

  ;; this is to use shift + arrow keys to switch between windows
  (windmove-default-keybindings)

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Add a hook to run code after Emacs has fully initialized.
  (add-hook 'after-init-hook
            (lambda ()
              (message "Emacs has fully loaded. This code runs after startup.")

              ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format
                         ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
                         (emacs-init-time)
                         (length (hash-table-keys straight--recipe-cache))))))))

;;; WINDOW
(use-builtin-package window
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|Flycheck errors\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))

;;; DIRED
(use-builtin-package dired
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-kill-when-opening-new-dired-buffer t))               ;; Close the previous buffer when opening a new `dired' instance.

;;; WHICH-KEY
(use-builtin-package which-key
  :defer t        ;; Defer loading Which-Key until after init.
  :hook
  (after-init . which-key-mode)) ;; Enable which-key mode after initialization.

;;; XCLIP
;; `xclip' is an Emacs package that integrates the X Window System clipboard
;; with Emacs. It allows seamless copying and pasting between Emacs and other
;; applications using the clipboard. When `xclip' is enabled, any text copied
;; in Emacs can be pasted in other applications, and vice versa, providing a
;; smooth workflow when working across multiple environments.
(use-package xclip
  :ensure t
  :defer t
  :hook
  (after-init . xclip-mode))     ;; Enable xclip mode after initialization.

;; ------------ TREE-SITTER ----------------------
;; Emacs 30 only support tree-sitter API 14
;; That's why we specify the versions so we control exactly what happens
(setq treesit-language-source-alist
      '(
        (bash        "https://github.com/tree-sitter/tree-sitter-bash"    "v0.23.3" "src")
        (c           "https://github.com/tree-sitter/tree-sitter-c"       "v0.23.3")
        (cmake       "https://github.com/uyha/tree-sitter-cmake"          "v0.7.2")
        (cpp         "https://github.com/tree-sitter/tree-sitter-cpp"     "v0.23.3")
        (json        "https://github.com/tree-sitter/tree-sitter-json"    "v0.24.8")
        (python      "https://github.com/tree-sitter/tree-sitter-python"  "v0.23.3")
        (rust        "https://github.com/tree-sitter/tree-sitter-rust"    "v0.23.3")
        (toml        "https://github.com/ikatyang/tree-sitter-toml"       "v0.5.1")
        (yaml        "https://github.com/ikatyang/tree-sitter-yaml"       "v0.5.0")
))

;; Auto-install missing grammars once (you can eval this block manually)
(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

;; Prefer the new ts-modes everywhere
(setq major-mode-remap-alist
      '(
        (c++-mode        . c++-ts-mode)
        (c-mode          . c-ts-mode)
        ;; (cmake-mode      . cmake-ts-mode)
        (json-mode       . json-ts-mode)
        (python-mode     . python-ts-mode)
        (rust-mode       . rust-ts-mode)
        (sh-mode         . bash-ts-mode)
        (toml-mode       . toml-ts-mode)
        (yaml-mode       . yaml-ts-mode)
))


;; ------------ MAC SPECIFIC WORKAROUND ------------------------
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

;; ----------------- KEY BINDINGS --------------------
;; general is used for easy keybinding configuration
;; that integrates well with which-key
(use-package general
  :config
  (general-create-definer my-leader-def
    :prefix "s-/")

  ;; Global keybindings
  (my-leader-def
   "t"  '(:ignore t :which-key "toggles")
   "tw" 'whitespace-mode)

  ;; (general-define-key
  ;;  "<escape>" 'keyboard-escape-quit)
  )

;; ------------------ UI Configuration ----------------

;; for compilation buffer
(use-package ansi-color)

(use-package eterm-256color
  :ensure t)

(add-hook 'term-mode-hook #'eterm-256color-mode)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
           (doom-modeline-buffer-file-name-style 'buffer-name)
           (doom-modeline-buffer-encoding t)
           (doom-modeline-vcs-max-length 20)))

;; Theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'orilla-brewer-dark t)

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 15)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Buffers
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ;; in terminal Emacs, C-` is interpreted as C-@
         ;; this way, we have the same bindings in terminal and GUI
         ;;("C-@"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '(help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

;; (use-package treesit-auto
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all))

;; Stateful keymaps with Hydra
(use-package hydra
  :defer 1)

;; setup general editing
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode python-ts-mode slim-mode haml-mode yaml-mode)
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

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :commands (treesit-fold-toggle
             treesit-fold-open
             treesit-fold-close
             treesit-fold-open-all
             treesit-fold-close-all)
  :init
  ;; Enable folding automatically for buffers that have a treesit parser
  ;; and are supported by treesit-fold.
  (global-treesit-fold-mode 1)
  :config
  ;; Optional: show number of lines in folded overlays
  (setq treesit-fold-line-count-show t)

  ;; Use F9 to toggle the fold at point (very similar to origami-toggle-node).
  (general-define-key
   :keymaps 'treesit-fold-mode-map
   "<f9>" '(treesit-fold-toggle :which-key "toggle treesit fold")))

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

;; (use-package ox-reveal
;;   :ensure ox-reveal
;;   :config
;;   (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0")
;;   (setq org-reveal-mathjax t))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

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
  (python-ts-mode . highlight-indent-guides-mode))

(use-package multiple-cursors
  )

;; Org-mode
(use-builtin-package toc-org
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

;;; VERTICO
;; Vertico enhances the completion experience in Emacs by providing a
;; vertical selection interface for both buffer and minibuffer completions.
;; Unlike traditional minibuffer completion, which displays candidates
;; in a horizontal format, Vertico presents candidates in a vertical list,
;; making it easier to browse and select from multiple options.
;;
;; In buffer completion, `switch-to-buffer' allows you to select from open buffers.
;; Vertico streamlines this process by displaying the buffer list in a way that
;; improves visibility and accessibility. This is particularly useful when you
;; have many buffers open, allowing you to quickly find the one you need.
;;
;; In minibuffer completion, such as when entering commands or file paths,
;; Vertico helps by showing a dynamic list of potential completions, making
;; it easier to choose the correct one without typing out the entire string.
(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode)           ;; Enable vertico after Emacs has initialized.
  :custom
  (vertico-count 10)                    ;; Number of candidates to display in the completion list.
  (vertico-resize nil)                  ;; Disable resizing of the vertico minibuffer.
  (vertico-cycle nil)                   ;; Do not cycle through candidates when reaching the end of the list.
  :config
  ;; Enable vertico-directory extras for file/path editing in the minibuffer.
  (require 'vertico-directory)

  ;; Clean up things like // and /../ in paths as you edit
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

;;; ORDERLESS
;; Orderless enhances completion in Emacs by allowing flexible pattern matching.
;; It works seamlessly with Vertico, enabling you to use partial strings and
;; regular expressions to find files, buffers, and commands more efficiently.
;; This combination provides a powerful and customizable completion experience.
(use-package orderless
  :ensure t
  :defer t                                    ;; Load Orderless on demand.
  :after vertico                              ;; Ensure Vertico is loaded before Orderless.
  :init
  (setq completion-styles '(orderless basic)  ;; Set the completion styles.
        completion-category-defaults nil      ;; Clear default category settings.
        completion-category-overrides '((file (styles partial-completion))))) ;; Customize file completion styles.

;;; MARGINALIA
;; Marginalia enhances the completion experience in Emacs by adding
;; additional context to the completion candidates. This includes
;; helpful annotations such as documentation and other relevant
;; information, making it easier to choose the right option.
(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode))

;;; CONSULT
;; Consult provides powerful completion and narrowing commands for Emacs.
;; It integrates well with other completion frameworks like Vertico, enabling
;; features like previews and enhanced register management. It's useful for
;; navigating buffers, files, and xrefs with ease.
;; NOTE(gmichel): `consult-line` is the equivalent of ivy swiper
(use-package consult
  :ensure t
  :defer t
  :init
  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

;; When the Consult prompt (or any minibuffer) is up,
  ;; make C-s/C-r walk the minibuffer history.
  (dolist (map (list minibuffer-local-map minibuffer-local-completion-map))
    (define-key map (kbd "C-s") #'previous-history-element) ; recall last query
    (define-key map (kbd "C-r") #'next-history-element))    ; go forward
  :bind (("C-s" . consult-line)))

;;; EMBARK
;; Embark provides a powerful contextual action menu for Emacs, allowing
;; you to perform various operations on completion candidates and other items.
;; It extends the capabilities of completion frameworks by offering direct
;; actions on the candidates.
;; Just `<leader> .' over any text, explore it :)
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; EMBARK-CONSULT
;; Embark-Consult provides a bridge between Embark and Consult, ensuring
;; that Consult commands, like previews, are available when using Embark.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.

;; setup term
(use-package vterm
    :ensure t
    :custom
    (vterm-always-compile-module t))

;; setup code completion
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
  :custom
  (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-indentation t)                            ;; Enable indentation.
  (lsp-semantic-tokens-enable nil)                      ;; Disable semantic tokens.
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-diagnostics-provider :flycheck)
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

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode)
  :config
  (setq flycheck-temp-prefix ".flycheck")
  (setq temporary-file-directory (expand-file-name "tmp/" user-emacs-directory))
  ;; only check on save
  ;;(setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs
        `(,(expand-file-name "snippets" user-emacs-directory)))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))

;; setup debuggers
(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-gdb-lldb)
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; setup c++ language support
;; LLVM stuff
(setq llvm-root "/usr/lib/llvm-20")
(setq my-clangd-executable (expand-file-name "bin/clangd" llvm-root))
(setq my-clang-check-executable (expand-file-name "bin/clang-check" llvm-root))

;; for C and C++
;; in Emacs 29/30:
;;
;; c++-ts-mode is just a function, autoloaded from the file c-ts-mode.el.
;;
;; The file actually provides the feature c-ts-mode, not c++-ts-mode.
;; So:
;; (require 'c++-ts-mode) → fails → Error (use-package): Cannot load c++-ts-mode
;; Later, when you open a C++ file, c++-ts-mode is called via its autoload, c-ts-mode.el is loaded, and everything works fine
;; So the mode itself is OK; it’s just use-package being too literal.
(use-builtin-package c-ts-mode
    :preface
    (defun orilla/c-ts-indent-style()
        `(;; do not indent namespace children
          ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)

          ;; append to bsd style
          ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
    :config
    (setq c-ts-mode-indent-offset 4)
    (setq c-ts-mode-indent-style #'orilla/c-ts-indent-style))

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

;; for CUDA
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-ts-mode))

;; LSP with C++
(add-hook 'c++-ts-mode-hook 'lsp-deferred)

(use-package clang-format
  :ensure t
  :hook
  (c++-ts-mode . my/c++-format-on-save)
  (c-ts-mode   . my/c++-format-on-save)
  :config
  (defun my/c++-format-on-save ()
    ;; Run clang-format before saving, but only in this buffer
    (add-hook 'before-save-hook #'clang-format-buffer nil t)))


(defun my-lsp-c++-hook ()
  "Configure clangd as C++ backend for lsp"
  (setq lsp-clients-clangd-executable my-clangd-executable
        lsp-clients-clangd-args (list (concat "--query-driver=" llvm-root "**") "-background-index" "--log=verbose" "--clang-tidy" "--inlay-hints" "-j$(($(nproc) / 2))" "--header-insertion=never" "--header-insertion-decorators" "--completion-style=detailed")))

(add-hook 'lsp-mode 'my-lsp-c++-hook)

(add-hook 'c++-ts-mode-hook (lambda ()
                              (require 'dap-cpptools)))
;; ;; Use clangcheck for flycheck in C++ mode
;; (defun my-select-clangcheck-for-checker ()
;;   "Select clang-check for flycheck's checker."
;;   (require 'flycheck-clangcheck)
;;   (flycheck-set-checker-executable 'c/c++-clangcheck my-clang-check-executable)
;;   (flycheck-select-checker 'c/c++-clangcheck))

;; (use-package flycheck-clangcheck
;;   :ensure t
;;   :config
;;   (setq flycheck-clangcheck-analyze t
;;         flycheck-clangcheck-extra-arg-before '("-std=c++2a")
;;         ;; flycheck-clangcheck-extra-arg '("-Xanalyzer" "-analyzer-output=text")
;;         )
;;   :hook (c++-mode . my-select-clangcheck-for-checker))

(with-eval-after-load 'flycheck
  ;; Don’t run native C/C++ checkers (clang/clang-tidy) to avoid temp files
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck c/c++-clang-tidy)))

;; setup python language support
(defun my-python-hook ()
  (setq python-shell-interpreter "python3"
        dap-python-executable "python3"
        dap-python-debugger 'debugpy)
  (run-python-internal)
  (lsp-deferred)
  (require 'dap-python))

(add-hook 'python-ts-mode-hook 'my-python-hook)

(use-package lsp-pyright
  :ensure t
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred)))
  :config
  (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/externals/python-type-stubs"))
  )

;; format on save with black
(use-package blacken
  :ensure t)

(use-builtin-package python
  :preface
  ;; Define the one-shot "save without running black" helper
  (defun orilla/save-without-blacken ()
    "Save current buffer without running blacken/black."
    (interactive)
    (let ((orig-hooks before-save-hook))
      ;; Temporarily remove blacken-buffer from before-save-hook
      (setq-local before-save-hook
                  (remove #'blacken-buffer before-save-hook))
      (unwind-protect
          (save-buffer)
        ;; Restore hooks so future saves still format
        (setq-local before-save-hook orig-hooks))))

  :hook
  ;; Turn on blacken-mode automatically in python-ts-mode buffers
  (python-ts-mode . blacken-mode)

  :bind
  ;; Add the special save key ONLY in python-ts-mode
  (:map python-ts-mode-map
        ("C-x C-M-s" . orilla/save-without-blacken)))

;; setup rust language support
;; Conf for Rust programming

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;;(setq rustic-lsp-server 'rust-analyzer)
  ;;(setq rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer"))
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(setq cargo-root "~/.cargo")

(defun my-lsp-rust-hook ()
  "Configure Rust backend for lsp"
  (setq lsp-rust-analyzer-server-command
        (list (substring (shell-command-to-string "rustup which rust-analyzer") 0 -1))
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil
        lsp-rust-analyzer-proc-macro-enable t

        ;; lsp-rust-analyzer-completion-add-call-parenthesis nil

        ;; lsp-rust-server my-rls-executable
        ;; lsp-rust-rls-server-command my-rls-executable
        ))


;; (setq my-rls-executable (expand-file-name "bin/rls" cargo-root))

;; (defun my-lsp-rust-hook ()
;;   "Configure RLS as Rust backend for lsp"
;;   (setq lsp-rust-server my-rls-executable
;;         lsp-rust-rls-server-command my-rls-executable))

(use-package toml-mode)

;; (use-package rust-mode
;;   :ensure t
;;   :hook (rust-mode . lsp-deferred))

(add-hook 'lsp-mode 'my-lsp-rust-hook)

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rustic-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; configure repl
;; based on: https://github.com/SerialDev/evcxr-mode

;; needed for evcxr
(use-package parsec
  :ensure t)

(straight-use-package
 '(evcxr
   :type git
   :host github
   :repo "serialdev/evcxr-mode"
   :config
   (add-hook 'rustic-mode-hook #'evcxr-minor-mode)
))

;; setup protobuf support
(use-package protobuf-mode :ensure t)

;; NOTE(gmichel): seems good without it. remove it?
;;(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; NOTE(gmichel): cleanup auto indent whitespace
;; it is still needed as of 20251026
;; see https://www.emacswiki.org/emacs/CleanAutoIndent
(use-package clean-aindent-mode
  :diminish
  :hook (prog-mode . clean-aindent-mode))

;; Automatically clean whitespace created during current editing
(use-builtin-package whitespace
  :defer t
  :hook (before-save-hook . whitespace-cleanup)
  ;; if we wanna remove this hook at any time, eval:
  ;; (remove-hook 'before-save-hook #'whitespace-cleanup)
  )

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; Stay in the current window after M-x compile
(advice-add 'compile :around
  (lambda (orig &rest args)
    (let ((w (selected-window)))
      (apply orig args)
      (select-window w))))

;; Package: projectile
(use-package projectile
  :diminish
  :config (projectile-mode)
  ;; (setq projectile-mode-line
  ;;       '(:eval (format " Projectile[%s(%s)]"
  ;;                       (projectile-project-name))))
  (setq projectile-mode-line "Projectile")
  (add-to-list 'projectile-other-file-alist '("h" "cpp" "c" "cc" "cu"))
  (add-to-list 'projectile-other-file-alist '("cu" "h"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/work")
    (setq projectile-project-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired)
  ;; Use native indexing + caching
  (setq projectile-indexing-method 'native
        projectile-enable-caching t
        ;; put cache somewhere writable and stable
        projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory)))

;; compilation helpers
;; stop at first error or keep scrolling
;;(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

;; Make the compilation window automatically disappear - from enberg on #emacs
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              ;; (run-at-time
              ;;  "2 sec" nil 'kill-buffer "*compilation*")
              ;; (run-at-time
              ;;  "1 sec" nil 'delete-window (get-buffer-window "*compilation*"))
              ;;(popper-toggle)
              (popper-close-latest)
              (message "No Compilation Errors!")))))

(use-package cmake-integration
  :straight '(cmake-integration :type git :host github :repo "darcamo/cmake-integration"
            :fork (:host github
                   :repo "guillaume-michel/cmake-integration"))
  ;; This runs before the package is loaded.
  :init
  (add-hook 'c++-ts-mode-hook
            (lambda ()
              ;; Load cmake-integration the first time we visit a C++ buffer.
              ;; When it loads, the :bind :map block below is applied.
              (require 'cmake-integration)))
  (add-hook 'c-ts-mode-hook
            (lambda ()
              ;; Load cmake-integration the first time we visit a C buffer.
              ;; When it loads, the :bind :map block below is applied.
              (require 'cmake-integration)))
  :config
  (setq cmake-integration-create-compile-commands-link nil)
  :bind (:map c++-ts-mode-map
              ([S-f5] . cmake-integration-save-and-compile) ;; Ask for the target name and compile it
              ([f5] . cmake-integration-save-and-compile-last-target) ;; Recompile the last target
              ([S-f12] . cmake-integration-run-last-target-with-arguments) ;; Ask for command line parameters to run the program
              ([f12] . cmake-integration-run-last-target) ;; Run the program (possible using the last command line parameters)
              ([S-f7] . cmake-integration-cmake-configure-with-preset) ;; Ask for a preset name and call CMake
              ([f7] . cmake-integration-cmake-reconfigure) ;; Call CMake with the last chosen preset
              ))

(add-hook 'c++-ts-mode-hook
      (lambda ()
        (define-key c++-ts-mode-map (kbd "<f6>") 'kill-compilation)))

(add-hook 'c-ts-mode-hook
      (lambda ()
        (define-key c-ts-mode-map (kbd "<f6>") 'kill-compilation)))

;; NOTE(gmichel): This messes up popper placement for the *compilation* buffer at the bottom of the frame
;; assure the compilation buffer is only opened once when multiple frames are open
;; (add-to-list 'display-buffer-alist
;;              '("\\*compilation\\*" . (display-buffer-reuse-window
;;                                       . ((reusable-frames . t)))))

;; enable color in compilation buffer
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Cap'n Proto syntax highlighting
(use-package capnp-mode
  :init
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq indent-line-function 'insert-tab)
)

;; magit
(use-package magit
  :ensure t)
(general-define-key
 "C-x g" 'magit-status)

(use-package magit-lfs
  :ensure t
  :after magit)

;; Google-this
(use-package google-this
  :ensure t
  :bind
  (("C-c <f1>" . google-this-cpp-reference)))

;; browse kill-ring
(use-package browse-kill-ring
  :ensure t
  :bind (("C-M-y" . browse-kill-ring)))

(defun slime-style-init-command (port-filename _coding-system extra-args)
  "Return a string to initialize Lisp."
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                  (concat slime-path slime-backend))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,(slime-to-lisp-filename (expand-file-name loader))
                     :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,(slime-to-lisp-filename port-filename)
                        ,@extra-args)))))

(defun slime-style (&optional style)
  (interactive
   (list (intern-soft (read-from-minibuffer "Style: " "nil"))))
  (lexical-let ((style style))
    (slime-start
     :init (lambda (x y)
             (slime-style-init-command
              x y `(:style ,style :dont-close t))))))

;; setup slime if present
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (if (file-exists-p slime-helper)
      (progn
        (load slime-helper)
        (setq inferior-lisp-program "/usr/local/bin/sbcl --dynamic-space-size 15000"))))
