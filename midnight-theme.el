(deftheme midnight
  "midnight color theme")

(custom-theme-set-faces
 'midnight

 ;; basic
 '(default ((t (:background "black" :foreground "white"))))
 '(region ((t (:background "DarkBlue"))))
 '(fringe ((t (:background "#252323"))))
 '(border-color ((t (:background "black"))))

 ;; line highlighting
 '(highlight-current-line-face ((t (:background "grey12"))))
 '(hl-line ((t (:background "#252323"))))

 ;; Mouse and cursor
 '(mouse ((t (:foreground "black" :background "DarkRed"))))
 '(cursor ((t (:foreground "black" :background "green"))))

 ;; diff mode
 '(diff-added ((t (:foreground "green"))))
 '(diff-header ((t (:foreground "purple"))))
 '(diff-removed ((t (:foreground "DarkRed"))))

 ;; dired
 '(dired-directory ((t (:foreground "green"))))
 '(dired-flagged ((t (:foreground "DarkRed"))))
 '(dired-header ((t (:foreground "purple"))))

 ;; latex
 '(font-latex-sedate-face ((t (:foreground "cyan"))))

 ;; code editing faces
 '(font-lock-builtin-face ((t (:foreground "SkyBlue"))))
 '(font-lock-comment-face ((t (:italic t :foreground "grey60"))))
 '(font-lock-comment-delimiter-face ((t (:forground "grey60"))))
 '(font-lock-function-name-face ((t (:foreground "SlateBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan"))))
 '(font-lock-preprocessor-face ((t (:foreground "#9932CC"))))
 '(font-lock-string-face ((t (:foreground "DarkRed"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))
 '(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
 '(font-lock-type-face ((t (:foreground "#008080")))) ;; dark cyan
 '(paren-face-match-light ((t (:background "grey30"))))
 '(show-paren-match ((t (:background "purple"))))
 '(show-paren-mismatch ((t (:background "red"))))
 '(highlight ((t (:background "DarkBlue"))))

 ;; modeline
 '(modeline ((t (:background "black" :foreground "white"))))
 '(modeline-buffer-id ((t (:background "DarkRed" :foreground "white"))))
 '(modeline-mousable ((t (:background "#a5baf1" :foreground "black"))))
 '(modeline-mousable-minor-mode ((t (:background "#a5baf1" :foreground "#000000"))))

 ;; Selection
 '(primary-selection ((t (:background "#3B3B3F"))))
 '(isearch ((t (:background "#555555"))))
 '(zmacs-region ((t (:background "#555577"))))
 '(secondary-selection ((t (:background "#545459"))))

 ;; mini buffer
 '(minibuffer-prompt ((t (:bold t :foreground "DarkRed"))))

 ;; Underline and italics
 '(underline ((t (:underline t))))
 '(italic ((t (:italic)))))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'midnight)
