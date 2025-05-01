;;; early-init.el -*- lexical-binding: t; -*-

;; disable package.el to be loaded
;; we are using straight.el
(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)

(setq native-comp-async-report-warnings-errors nil)

(setq debug-on-error t)

;; for LSP booster
;; lsp-mode will use plist instead of hash-table
(setenv "LSP_USE_PLISTS" "true")

;; Skipping a bunch of regular expression searching
;; in the file-name-handler-alist should improve start time.
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Garbage collection management for faster init time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold (* 128 1024 1024))
     (message "Emacs loaded in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract (current-time) before-init-time)))
              gcs-done)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(add-hook 'emacs-startup-hook '+reset-init-values)

;; Better Window Management handling
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; ignore Xresources
(advice-add #'x-apply-session-resources :override #'ignore)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

(provide 'early-init)
;;; early-init.el ends here
