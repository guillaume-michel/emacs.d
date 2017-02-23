;;(require 'color-theme)
;;(color-theme-initialize)

;;(color-theme-jsc-dark)
;;(color-theme-arjen)
;;(color-theme-billw)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'zenburn t)
;;(load-theme 'tomorrow-night-paradise t)
;;(load-theme 'ujelly t)
;;(load-theme 'base16-brewer-dark t)
;;(load-theme 'base16-bright-dark t)
;;(load-theme 'base16-tomorrow-dark t)
(load-theme 'orilla-brewer-dark t)

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 15)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(provide 'setup-theme)
