;; Compile Netatmo code

;; stop at first error or keep scrolling
;;(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

(defun na-recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (let ((compile-command "namake.sh")
        (compilation-read-command nil))
    (if (get-buffer "*compilation*") ; If old compile window exists
        (call-interactively 'recompile)
      (call-interactively 'compile))))

(defun compile-clean ()
  "Switches between compile command and clean command"
  (interactive)
  (let ((compile-command "namake.sh clean")
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
              (message "No Compilation Errors!")))))

(add-hook 'c++-mode-hook
      (lambda ()
        (define-key c++-mode-map (kbd "<f5>") 'na-recompile)))

(add-hook 'c++-mode-hook
      (lambda ()
        (define-key c++-mode-map (kbd "<S-f5>") 'compile-clean)))

(add-hook 'c++-mode-hook
      (lambda ()
        (define-key c++-mode-map (kbd "<f6>") 'kill-compilation)))

;; assure the compilation buffer is only opened once when multiple frames are open
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" . (display-buffer-reuse-window
                                      . ((reusable-frames . t)))))

;; enable color in compilation buffer
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'setup-compilation)
