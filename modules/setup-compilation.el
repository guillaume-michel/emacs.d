;; Compile Netatmo code

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
  :config
  (setq cmake-integration-create-compile-commands-link nil)
  :bind (:map c++-mode-map
              ([S-f5] . cmake-integration-save-and-compile) ;; Ask for the target name and compile it
              ([f5] . cmake-integration-save-and-compile-last-target) ;; Recompile the last target
              ([S-f12] . cmake-integration-run-last-target-with-arguments) ;; Ask for command line parameters to run the program
              ([f12] . cmake-integration-run-last-target) ;; Run the program (possible using the last command line parameters)
              ([S-f7] . cmake-integration-cmake-configure-with-preset) ;; Ask for a preset name and call CMake
              ([f7] . cmake-integration-cmake-reconfigure) ;; Call CMake with the last chosen preset
              ))

(add-hook 'c++-mode-hook
      (lambda ()
        (define-key c++-mode-map (kbd "<f6>") 'kill-compilation)))

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

;; Godbolt inside Emacs!
(use-package rmsbolt
  :config
  (setq rmsbolt-automatic-recompile nil))

(provide 'setup-compilation)
