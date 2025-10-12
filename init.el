;;; init.el --- Emacs Config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Increase the number of bytes that are read by default from the process
(setq read-process-output-max (* 10 1024 1024))

;; ------------------ VARIABLES -------------------------------
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; ------------------- INIT PACKAGES --------------------------
(require 'setup-packages)

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
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
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

;; ------------ MAC SPECIFIC WORKAROUND ------------------------
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

;; ------------------- SANE SETTINGS ---------------------------

(defalias 'yes-or-no-p 'y-or-n-p)

;; ;; scroll one line at a time (less "jumpy" than defaults)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;; ;; nice scrolling
;; (setq scroll-margin 10
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

;; ;; Enable line numbers for some modes
;; (dolist (mode '(text-mode-hook
;;                 prog-mode-hook
;;                 conf-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; ;; Override some modes which derive from the above
;; (dolist (mode '(org-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ;; Copy/paste stuff
;; (setq select-enable-clipboard t
;;       select-enable-primary t
;;       save-interprogram-paste-before-kill t
;;       mouse-yank-at-point t)

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

;; which-key is a useful UI panel that appears
;; when you start pressing any key binding in Emacs
;; to offer you all possible completions for the prefix
(use-builtin-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

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
           (doom-modeline-buffer-encoding nil)
           (doom-modeline-vcs-max-length 20)))

;; Theme
(require 'setup-theme)

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '(help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

;;; WINDOW
;; This section configures window management in Emacs, enhancing the way buffers
;; are displayed for a more efficient workflow. The `window' use-package helps
;; streamline how various buffers are shown, especially those related to help,
;; diagnostics, and completion.
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
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))

;; (use-package treesit-auto
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all))

;; Stateful keymaps with Hydra
(use-package hydra
  :defer 1)

;; setup general editing
(require 'setup-editing)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  )

(use-package counsel
  :diminish
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :after counsel
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; setup buffers behavior
;; (require 'setup-buffers)

;; setup term
(require 'setup-term)

;; setup code completion
(require 'setup-completion)

;; setup debuggers
(require 'setup-debugger)

;; setup c++ language support
(require 'setup-cpp)

;; setup python language support
(require 'setup-python)

;; setup rust language support
(require 'setup-rust)

;; setup julia language support
(require 'setup-julia)

;; setup cuda language support
(require 'setup-cuda)

;; setup protobuf support
(require 'setup-protobuf)

;; setup mojo language support
(require 'mojo-mode)
(require 'setup-mojo)

;; setup zig
(require 'setup-zig)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Package: clean-aindent-mode
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
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/work")
    (setq projectile-project-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; compilation helpers
(require 'setup-compilation)

;; Bazel
;; (use-package bazel-mode)

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

;; (use-package forge
;;   :after magit)

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
