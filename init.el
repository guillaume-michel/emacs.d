;; ---------------- STARTUP SPEEDUP --------------------------------------------
;; The default is 800 kilobytes.  Measured in bytes.
;; set high threshold to boost startup
(setq gc-cons-threshold (* 500 1000 1000))

;; Increase the number of bytes that are read by default from the process
(setq read-process-output-max (* 10 1024 1024))

;; Profile emacs startup & setup normal GC threshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            ;; reduce gc threshold to avoid freezes during GC
            (setq gc-cons-threshold (* 50 1000 1000))))

;; ------------------ VARIABLES -------------------------------
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; ------------------- INIT PACKAGES --------------------------
(require 'setup-packages)

;; ---------------- Keep .emacs.d clean ------------------------
;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; additional config for extra files management
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)  ; use versioned backups

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files no-littering-var-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; Keep customization settings apart from ~/.emacs.d
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; ------------ MAC SPECIFIC WORKAROUND ------------------------
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

;; ------------------- SANE SETTINGS ---------------------------

;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold (* 100 1024 1024))

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; minimal view setup
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; maximize frame
(toggle-frame-maximized)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; this is to use shift + arrow keys to switch between windows
(windmove-default-keybindings)

(global-auto-revert-mode 1)

;; show column number in modeline
(column-number-mode)

;; minimum line number column width
(setq-default display-line-numbers-width 4)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Copy/paste stuff
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

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
;; set font size
(set-face-attribute 'default nil :height 90)
(set-face-attribute 'fixed-pitch nil :height 90)
(set-face-attribute 'variable-pitch nil :height 90 :weight 'regular)

;; for compilation buffer
(use-package ansi-color)

;; which-key is a useful UI panel that appears
;; when you start pressing any key binding in Emacs
;; to offer you all possible completions for the prefix
(use-package which-key
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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :diminish
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

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

;; setup code completion
(require 'setup-completion)

;; setup debuggers
(require 'setup-debugger)

;; setup c++ language support
(require 'setup-cpp)

;; setup python language support
(require 'setup-python)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 2 spaces
(setq-default tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))

;; Package: clean-aindent-mode
(use-package clean-aindent-mode
  :diminish
  :hook (prog-mode . clean-aindent-mode))

;; Automatically clean whitespace created during current editing
(use-package ws-butler
  :diminish
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; remove trailing whitespaces before saving
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; Package: projectile
(use-package projectile
  :diminish
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/work")
    (setq projectile-project-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; compilation helpers
(require 'setup-compilation)

;;; winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Bazel
(use-package bazel-mode)

;; magit
(use-package magit)
(general-define-key
 "C-x g" 'magit-status)

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
