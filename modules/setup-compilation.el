;; Compile Netatmo code
;; (defun na-compile ()

;;   (interactive)
;;   (defvar cmd "namake.sh")
;;   (shell-command cmd)
;; )

(setq compile-command "namake.sh")
(setq compile-clean "namake.sh clean")
(setq compilation-window-height 20)

;; stop at first error or keep scrolling
(setq compilation-scroll-output t)
;;(setq compilation-scroll-output 'first-error)

;; my version
(defun na-recompile ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (progn
    (if (get-buffer "*compilation*") ; If old compile window exists
        (progn
          (call-interactively 'recompile))
      (call-interactively 'compile))
    (enlarge-window 20)))

(defun compile-clean ()
  "Switches between compile command and clean command"
  (interactive)
  (let (  (compile-command compile-clean)
      (compilation-read-command nil))
    (call-interactively 'compile)
    (enlarge-window 20)))

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

(provide 'setup-compilation)
