(add-to-list 'load-path "~/Documents/emacs/")

;; Packages
;; M-x package-refresh-contents to update package database
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(setq url-http-attempt-keepalives nil)

;; Mediawiki mode
(require 'mediawiki)
(defun mediawiki-mode-setup ()
  (turn-on-flyspell))
(add-hook 'mediawiki-mode-hook 'mediawiki-mode-setup)
(setq mediawiki-site-alist
      (quote
       (("PLUG"
         "http://plug.cs.fiu.edu/wiki/"
         "jesusramos" ""
         "Main Page"))))

;; Global Functions
(defun set-enter-newline-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(defun infer-indentation-style ()
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

;; DocView
(setq doc-view-continuous t)
(setq doc-view-resolution 200)

;; doc comments, doesn't work quite right
(defconst custom-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "///"
                                  limit gtkdoc-font-lock-doc-comments)))))
(setq-default c-doc-comment-style (quote (gtkdoc javadoc autodoc custom)))

;; GLSL mode
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; Git
(require 'git)
(require 'git-blame)

;; c-eldoc, custom version
(setq c-eldoc-includes
      "`pkg-config gtk+-2.0 --cflags` -I./ -I../include -I../../include -I../../../include")
(load "c-eldoc")
(defun c-eldoc-setup (map)
  (c-turn-on-eldoc-mode)
  (define-key map (kbd "C-c d") 'c-eldoc-force-cache-update))
(add-hook 'c-mode-hook
          (lambda ()
            (c-eldoc-setup c-mode-map)))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-eldoc-setup c++-mode-map)))

;; LaTeX/Auctex settings
(require 'tex-site)
(defun latex-mode-setup ()
  (latex-math-mode)
  (turn-on-reftex)
  (turn-on-flyspell)
  (set-enter-newline-indent))
(add-hook 'LaTeX-mode-hook 'latex-mode-setup)
(setq LaTeX-item-indent 0)
(setq reftex-plug-into-AUCTeX t)
(setq font-latex-match-slide-title-keywords (quote (("frametitle" "{"))))
(setq font-latex-match-warning-keywords
      (quote ("hline" "pause" "and" "hfill")))
(setq font-latex-match-function-keywords
      (quote
       (("titlepage" "") ("maketitle" "") ("frame" "") ("tableofcontents" "")
        ("noindent" "") ("usetheme" "{") ("usecolortheme" "{")
        ("institute" "[{") ("hypersetup" "{") ("lstinputlisting" "[{")
        ("includegraphics" "[{") ("title" "[{") ("href" "{{") ("url" "{")
        ("useoutertheme" "{") ("useinnertheme" "{") ("setbeamercolor" "{{")
        ("setbeamertemplate" "{{") ("setdescription" "{") ("lstset" "{")
        ("textcolor" "{") ("verbatiminput" "{") ("graphicspath" "{")
        ("fancyhead" "[{") ("fancyfoot" "[{") ("ProvidesPackage" "{")
        ("doublespacing" "") ("setbeamercolor" "*{{") ("setbeamerfont" "{{")
        ("setbeamertemplate" "{[") ("RequirePackage" "{") ("fontsize" "{{")
        ("selectcolormodel" "{") ("definecolor" "{{{"))))
(setq LaTeX-begin-regexp "begin\\b\\|If\\b\\|Else\\b")
(setq LaTeX-end-regexp "end\\b\\|EndIf\\b\\|Else\\b")
(setq LaTeX-indent-environment-list
      (quote
       (("verbatim" current-indentation)
        ("verbatim*" current-indentation)
        ("array") ("displaymath") ("eqnarray")
        ("eqnarray*") ("equation") ("equation*")
        ("picture") ("tabbing") ("table")
        ("table*") ("tabular") ("tabular*")
        ("algorithmic"))))
(setq LaTeX-paragraph-commands (quote ("If" "State")))

;; Line numbers
(require 'linum)
(global-linum-mode t)

;; org mode
(require 'org-latex)
(defun org-mode-setup ()
  (turn-on-flyspell))
(add-hook 'org-mode-hook 'org-mode-setup)
(setq org-export-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (sh . t)
   (emacs-lisp . t)
   (clojure . t)
   (C . t)))
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; Disable line numbers in certain buffers
(defcustom linum-disabled-modes-list
  '(eshell-mode wl-summary-mode compilation-mode org-mode dired-mode erc-mode term-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum)
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
              (and linum-disable-starred-buffers
                   (string-match "*" (buffer-name))))
    (linum-mode 1)))

;; Basic emacs settings
(setq initial-scratch-buffer nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default truncate-lines t)
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(if (featurep 'ns-win) (setq system-uses-terminfo nil))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (boundp 'tool-bar-mode) (tool-bar-mode 0))
(if (boundp 'menu-bar-mode) (menu-bar-mode 0))
(column-number-mode 1)
(setq-default fill-column 80)
(set-face-attribute 'default nil :height 100)
(global-auto-revert-mode t)
(setq-default frame-title-format '(buffer-file-name "%f" ("%b")))
(setq-default icon-title-format '(buffer-file-name "%f" ("%b")))
(setq visible-bell t)
(defun close-scratch-hook ()
  (kill-buffer "*scratch*"))
(add-hook 'emacs-startup-hook 'close-scratch-hook)
(blink-cursor-mode 1)
(setq x-stretch-cursor t)
(defalias 'yes-or-no-p 'y-or-n-p)
(savehist-mode t)
(setq-default require-final-newline t)
(size-indication-mode)
(setq-default write-region-inhibit-fsync t)
(global-hl-line-mode 1)
(display-time-mode 1)
(setq compilation-scroll-output t)
(auto-fill-mode t)

;; dired settings
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

;; Kill all buffers
(defun nuke-buffers ()
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows)
  ;; Cleanup TRAMP connections to avoid funkiness
  (tramp-cleanup-all-connections))

;; Emacs server
(require 'server)
(when (and (functionp 'server-running-p)
           (not (server-running-p))) (server-start))

;; Text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Make file either have all spaces or all tabs and remove trailing whitespace
(defun uniform-tabify ()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max))
    (tabify (point-min) (point-max))))
;; Uniform tabify may get you in trouble sometimes
(add-hook 'before-save-hook 'uniform-tabify)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; switch windows
(defun require-switch_window ()
  (require 'switch-window))
(add-hook 'emacs-startup-hook 'require-switch_window)

;; cscope
(require 'xcscope)
(define-key global-map [(control f3)] 'cscope-set-initial-directory)
(define-key global-map [(control f4)] 'cscope-unset-initial-directory)
(define-key global-map [(control f5)] 'cscope-find-this-symbol)
(define-key global-map [(control f6)] 'cscope-find-global-definition)
(define-key global-map [(control f7)] 'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)] 'cscope-pop-mark)
(define-key global-map [(control f9)] 'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)] 'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)

;; Speed up compilation output, useful for linux kernel compiles on slow
;; computers
(defun speed-up-compiler-output ()
  (interactive)
  (setq compilation-error-regexp-alist nil)
  (setq compilation-error-regexp-alist-alist nil))

;; Generic Indentation rules
;; no tabs, 4 space indent
(defun set-spaces-mode ()
  (setq c-default-style "bsd" c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (setq-default indent-tabs-mode nil)
  (setq tab-width 4))
;; Tabs, 8 space indent, no indent on case statements
(defun set-tabs-mode ()
  (setq c-default-style "bsd" c-basic-offset 8)
  (c-set-offset 'case-label 0)
  (setq-default indent-tabs-mode t)
  (setq tab-width 8))
(set-spaces-mode) ;; use spaces mode by default

;; Python
(setq python-indent 4)

;; impatient mode
;; http://localhost:8080/imp/ to see impatient mode buffers
(defun start-impatient-mode ()
  (httpd-start)
  (impatient-mode))
(add-hook 'html-mode-hook 'start-impatient-mode)
(add-hook 'css-mode-hook 'start-impatient-mode)

;; highlight matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; color theme
(add-to-list 'custom-theme-load-path "~/Documents/emacs")
(load-theme 'midnight t)

;; Common C derived buffers, enable auto indent
(add-hook 'c-mode-common-hook 'set-enter-newline-indent)

;; Terminal settings
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; Fix gcc output from ansi-term
(setq local-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Generic Emacs Hotkeys
(global-set-key [f9] 'compile)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Emacs IRC
(setq erc-auto-query 'buffer)
(setq erc-server "plug.cs.fiu.edu")
(setq erc-nick "jesus")

;; assembly mode
(defun asm-mode-setup ()
  (local-set-key (kbd "RET") 'newline)) ;; disable newline and enter
(add-hook 'asm-mode-hook 'asm-mode-setup)
(setq tab-stop-list
      (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80
                84 88 92 96 100 104 108 112 116 120)))

;; web browser
(if (eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (if (or (eq system-type 'ms-dos)
          (eq system-type 'windows-nt)
          (eq system-type 'cygwin))
      (setq browse-url-browser-function 'browse-url-default-windows-browser)
    (setq browse-url-generic-program (executable-find "chromium")
          browse-url-browser-function 'browse-url-generic)))

;; develock
(require 'develock)

;; semantic mode
;; (semantic-mode 1)

;; Linux kernel style
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))
(c-add-style "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))
(defun linux-kernel-style ()
  (interactive)
  (c-set-style "linux-tabs-only")
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (c-set-offset 'case-label 0)
  (when (called-interactively-p 'any)
    (font-lock-fontify-buffer)))
(defun linux-kernel-setup ()
  (let ((filename (buffer-file-name)))
    (when (and filename
               (string-match "linux" filename))
      (linux-kernel-style))))
;; (add-hook 'c-mode-hook 'linux-kernel-setup)

;; Fun stuff
(defun mandelbrot ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*mandelbrot*"))
  (let ((w 800) (h 600) (d 32))
    (fundamental-mode) (erase-buffer)
    (set-buffer-multibyte nil)
    (insert (format "P6\n%d %d\n255\n" w h))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((cx (* 1.5 (/ (- x (/ w 1.45)) w 0.45)))
               (cy (* 1.5 (/ (- y (/ h 2.0)) h 0.5)))
               (zr 0) (zi 0)
               (v (dotimes (i d d)
                    (if (> (+ (* zr zr) (* zi zi)) 4) (return i)
                      (psetq zr (+ (* zr zr) (- (* zi zi)) cx)
                             zi (+ (* (* zr zi) 2) cy))))))
          (insert-char (floor (* 256 (/ v 1.0 d))) 3))))
    (image-mode)))

(defun sierpinski (s)
  (pop-to-buffer (get-buffer-create "*sierpinski*"))
  (fundamental-mode) (erase-buffer)
  (labels ((fill-p (x y)
                   (cond ((or (zerop x) (zerop y)) "0")
                         ((and (= 1 (mod x 3)) (= 1 (mod y 3))) "1")
                         (t (fill-p (/ x 3) (/ y 3))))))
    (insert (format "P1\n%d %d\n" s s))
    (dotimes (y s) (dotimes (x s) (insert (fill-p x y) " "))))
  (image-mode))

;; Google
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search Google: "))))))
(global-set-key (kbd "C-x g") 'google)

;; Youtube
(defun youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))
(global-set-key (kbd "C-x y") 'youtube)

;; ELISP
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
