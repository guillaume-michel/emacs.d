;; ------------ PACKAGES ----------------------
(defconst orilla-packages
  '(projectile
    smartparens
    smooth-scroll
    ))

;; ------------------ VARIABLES -------------------------------
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; ------------------- INIT PACKAGES --------------------------
(require 'setup-packages)
(install-packages orilla-packages)

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

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; smooth scroll
(require 'smooth-scroll)
(smooth-scroll-mode t)

;; this is to use shift + arrow keys to switch between windows
(windmove-default-keybindings)

;; setup general editing
(require 'setup-editing)

;; setup c++ indentation and style, fix C++11 issues
(require 'setup-cpp)

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
