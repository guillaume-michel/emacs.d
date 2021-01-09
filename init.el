;; ---------------- STARTUP SPEEDUP --------------------------------------------
;; The default is 800 kilobytes.  Measured in bytes.
;; set high threshold to boost startup
(setq gc-cons-threshold (* 500 1000 1000))

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

(defconst orilla-packages
  '(
    ansi-color
    bind-key
    clang-format
    clean-aindent-mode
    cmake-mode
    cmake-font-lock
    company
    company-irony
    company-jedi
    dockerfile-mode
    flycheck
    flyspell
    git-timemachine
    glsl-mode
    google-c-style
    helm
    helm-bibtex
    helm-gtags
    helm-projectile
    helm-swoop
    irony
    jedi
    lua-mode
    magit
    modern-cpp-font-lock
    ox-reveal
    prettier-js
    projectile
    pip-requirements
    rainbow-delimiters
    rainbow-identifiers
    sbt-mode
    smartparens
    ws-butler
    yaml-mode
    yasnippet
    zygospore
    ))

;; ------------------ VARIABLES -------------------------------
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; ------------------- INIT PACKAGES --------------------------
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

 ;; Initialize use-package if needed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Uncomment this to get a reading on packages that get loaded at startup
;; (setq use-package-verbose t)

;; TODO remove when use-package is used everywhere
(require 'setup-packages)
(install-packages orilla-packages)

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

;; ------------------- Early customization ---------------------
;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

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

;; Theme
(require 'setup-theme)

(require 'setup-helm)
(require 'setup-helm-gtags)

;; setup general editing
(require 'setup-editing)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;; setup c++ indentation and style, fix C++11 issues
(require 'setup-cpp)

;; setup company and irony mode for code completion with clang
(require 'setup-completion)

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
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; remove trailing whitespaces before saving
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; shortcuts with SUPER but doesn't work on OSX
;;(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
;;(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
;;(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
;;(define-key projectile-mode-map [?\s-g] 'projectile-grep)

;; compilation helpers
(require 'setup-compilation)

;;; winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

(require 'bazel-mode)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

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
