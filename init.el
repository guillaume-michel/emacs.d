;; ------------ PACKAGES ----------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst orilla-packages
  '(bind-key
    clean-aindent-mode
    cmake-mode
    cmake-font-lock
    color-theme
    company
    company-irony
    diminish
    flycheck
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    irony
    magit
    projectile
    smartparens
    smooth-scroll
    ws-butler
    yasnippet
    zygospore
    ))

;; ------------------ VARIABLES -------------------------------
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; ------------------- MAC Specifics --------------------------
;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; ------------------- INIT PACKAGES --------------------------
(require 'setup-packages)
(install-packages orilla-packages)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; ------------------- Early customization ---------------------
;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

;; ------------------- SANE SETTINGS ---------------------------

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 100 1024 1024))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; disable menu bar
(menu-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; smooth scroll
;;(require 'smooth-scroll)
;;(smooth-scroll-mode t)
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; this is to use shift + arrow keys to switch between windows
(windmove-default-keybindings)

(global-auto-revert-mode 1)

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

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

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

;; place auto save files in "temp" dir
(setq
 backup-by-copying t
 backup-directory-alist `(("." . ,"~/.emacs.d/tmp")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)  ; use versioned backups

;; save auto save to tmp
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; backups to tmp instead of current directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; shortcuts with SUPER but doesn't work on OSX
;;(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
;;(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
;;(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
;;(define-key projectile-mode-map [?\s-g] 'projectile-grep)


;; setup slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
