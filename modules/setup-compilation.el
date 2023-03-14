;; Compile Netatmo code

;; stop at first error or keep scrolling
;;(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

(defun na-recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (let* ((cmake-ide-project-dir (projectile-project-root))
         (cmake-ide-build-dir (concat cmake-ide-project-dir "build"))
         (compile-command (format "cmake --build %s --parallel $(($(nproc) + 1))" cmake-ide-build-dir))
        ;;(compile-command "cmake --build $HOME/work/mastermind/build --parallel $(($(nproc) + 1))")
        (compilation-read-command nil))
    ;; (if (get-buffer "*compilation*") ; If old compile window exists
    ;;     (call-interactively 'recompile)
       (call-interactively 'compile);;)
    ))

;; (add-to-list 'safe-local-variable-values
;;              '(compile-command . (format "cmake --build %s --parallel $(($(nproc) + 1))" cmake-ide-build-dir)))


(defun compile-clean ()
  "Switches between compile command and clean command"
  (interactive)
  (let* ((cmake-ide-project-dir (projectile-project-root))
        (cmake-ide-build-dir (concat cmake-ide-project-dir "build"))
        (compile-command (format "cmake --build %s --target clean --parallel $(($(nproc) + 1))" cmake-ide-build-dir))
        ;;(compile-command . "cmake --build $HOME/work/mastermind/build --target clean --parallel $(($(nproc) + 1))")
        (compilation-read-command nil))
    (call-interactively 'compile)))

;; Make the compilation window automatically disappear - from enberg on #emacs
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'kill-buffer "*compilation*")
              ;; (run-at-time
              ;;  "2 sec" nil 'delete-window (get-buffer-window "*compilation*"))
              (message "No Compilation Errors!")))))

;; (setq split-height-threshold 0)
;; (setq compilation-window-height 20)

;; (defun my-compilation-hook ()
;;   (when (not (get-buffer-window "*compilation*"))
;;     (save-selected-window
;;       (save-excursion
;;         (let* ((w (split-window-vertically))
;;                (h (window-height w)))
;;           (select-window w)
;;           (switch-to-buffer "*compilation*")
;;           (shrink-window (- h compilation-window-height)))))))
;; (add-hook 'compilation-mode-hook 'my-compilation-hook)

(use-package cmake-integration
  :straight '(cmake-integration :type git :host github :repo "darcamo/cmake-integration"
            :fork (:host github
                   :repo "guillaume-michel/cmake-integration"))
  :bind (:map c++-mode-map
              ([M-f5] . cmake-integration-save-and-compile) ;; Ask for the target name and compile it
              ([f5] . cmake-integration-save-and-compile-last-target) ;; Recompile the last target
              ([M-f12] . cmake-integration-run-last-target-with-arguments) ;; Ask for command line parameters to run the program
              ([f12] . cmake-integration-run-last-target) ;; Run the program (possible using the last command line parameters)
              ([M-f7] . cmake-integration-cmake-configure-with-preset) ;; Ask for a preset name and call CMake
              ([f7] . cmake-integration-cmake-reconfigure) ;; Call CMake with the last chosen preset
              ))


;; (add-hook 'c++-mode-hook
;;       (lambda ()
;;         (define-key c++-mode-map (kbd "<f5>") 'na-recompile)))

;; (add-hook 'c++-mode-hook
;;       (lambda ()
;;         (define-key c++-mode-map (kbd "<S-f5>") 'compile-clean)))

(add-hook 'c++-mode-hook
      (lambda ()
        (define-key c++-mode-map (kbd "<f6>") 'kill-compilation)))

;; (add-hook 'c-mode-hook
;;       (lambda ()
;;         (define-key c-mode-map (kbd "<f5>") 'na-recompile)))

;; (add-hook 'c-mode-hook
;;       (lambda ()
;;         (define-key c-mode-map (kbd "<S-f5>") 'compile-clean)))

(add-hook 'c-mode-hook
      (lambda ()
        (define-key c-mode-map (kbd "<f6>") 'kill-compilation)))

;; assure the compilation buffer is only opened once when multiple frames are open
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" . (display-buffer-reuse-window
                                      . ((reusable-frames . t)))))

;; enable color in compilation buffer
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (use-package popwin
;;   :config
;;   (popwin-mode 1))

;; (push '(compilation-mode :noselect t) popwin:special-display-config)
;; (push '(compilation-mode :position bottom) popwin:special-display-config)
;; (push '(compilation-mode :stick t) popwin:special-display-config)


;; (global-set-key (kbd "C-z") popwin:keymap)

(provide 'setup-compilation)
