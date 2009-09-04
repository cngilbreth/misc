;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;
;;
;; Files:
;;
;;     open a recent file (recentf): C-a (my keybinding)
;;
;;     remote paths OR directories:
;;         ftp: C-x C-f /user@host:/path/to/file-or-dir
;;         ssh: C-x C-f /ssh:user@host:/path/to/whatever
;;         sftp doesn't seem to work.
;;
;; Dired:
;;
;;     ? for mini help
;;     h for help
;;
;;     m to mark a file
;;     u to unmark; U to unmark all
;;     D to delete marked files
;;     R to rename a single file
;;     R to move several files
;;
;;     i (over a directory) to insert it into current view
;;     v to view a file in read-only mode
;;     q to quit window (including view windows)
;;
;;     g or f5: refresh (f5 is mine)
;;
;;     ... lots of others (type h)
;;
;;
;; Navigation:
;;
;;     C-u C-<space>: Go to previous location of the mark.
;;
;;     C-l: scroll the window so the point is centered vertically
;;     C-<down>, C-<up>: move cursor by 4 lines (mine)
;;     <next>, <prior> (i.e. page up, down): scroll 5 lines (mine)
;;     M-<next>, M-<prior>: scroll 10 lines (mine)
;;     M-f: forward-whitespace (mine)
;;
;;     C-j: newline and indent
;;     M-j: newline and indent within comment
;;     C-o: insert newline and leave point before it
;;
;; Editing:
;;
;;     M-/: word completion!  I said word completion!
;;
;; Rectangles:
;;
;;     C-x r k: kill rectangle
;;     C-x r y: yank rectangle
;;
;;     C-x r r: copy rectangle to register
;;     C-x r i: insert register (general command)
;;
;;     C-x r d: delete rectangle
;;     C-x r c: clear rectangle
;;     C-x r o: open rectangle (shift text right to create open space)
;;
;; Cleaning up whitespace:
;;
;;     C-u 1 M-x whitespace-buffer
;;     ;; Above finds whitespace problems. Can then call
;;     M-x whitespace-cleanup
;;
;; Etags:
;;
;;     M-.: find tag
;;     M-*: Go back to where you where when you invoked M-. and friends
;;     C-x 4 .: find-tag-other-window
;;     C-x 5 .: find-tag-other-frame
;;
;;     C-u M-.: find next alternate definition of tag
;;     C-u - M-.: go back to previous found tag
;;     C-M-.: find-tag-regexp
;;     C-u C-m: find next tag whose name matches regexp
;;
;;     M-x tags-search
;;     M-,
;;     
;;
;; Imenu:
;;
;;     S-.: find tag through imenu
;;
;; F90:
;;     C-c f: f90-fill-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-accessible-directory-p "~/emacs/slime")
  (add-to-list 'load-path "~/emacs/slime")
  (when (file-accessible-directory-p "/usr/local/lib/sbcl")
    (setq inferior-lisp-program "/usr/local/bin/sbcl"))
  (require 'slime)
  (slime-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET and ECB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; CEDET

(when (file-accessible-directory-p "~/emacs/cedet")
  (add-to-list 'load-path "~/emacs/cedet/common")
  (load-file "~/emacs/cedet/common/cedet.el")
  (global-ede-mode 1)

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)


;;; ECB

  (when (file-accessible-directory-p "~/emacs/ecb")
    (add-to-list 'load-path "~/emacs/ecb")
    (require 'ecb-autoloads)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(customize-set-variable  'msb-mode 't)
(customize-set-variable 'tool-bar-mode 't)
(global-font-lock-mode 't)
(mouse-wheel-mode 't)
(column-number-mode 't)
(blink-cursor-mode 0)
(setq transient-mark-mode 't)
(setq inhibit-startup-message 't)
(customize-set-variable 'scroll-bar-mode 'right)

(customize-set-variable 'auto-fill-mode 't) ; very nice in f90 mode


(require 'recentf)
(recentf-mode 't)
(customize-set-variable 'recentf-exclude '("^/scp"))

(when (file-accessible-directory-p "~/emacs")
  (add-to-list 'load-path "~/emacs")
  (require 'ido)
  (ido-mode 'buffers))

;; http://c2.com/cgi/wiki?FixmeComment

 (setq fixme-modes '(erlang-mode java-mode c-mode emacs-lisp-mode scheme-mode f90-mode))
 (make-face 'font-lock-fixme-face)
 (mapc (lambda (mode)
	 (font-lock-add-keywords
	  mode
	  '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t))))
	fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" "Yellow" nil t nil t nil nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-accessible-directory-p "~/emacs")
  (add-to-list 'load-path "~/emacs"))


(when (file-accessible-directory-p "~/emacs/tramp/lisp")
  (add-to-list 'load-path "~/emacs/tramp/lisp")
  (require 'password)
  (require 'tramp)
  (add-to-list 'Info-default-directory-list "~/emacs/tramp/info/")
  (customize-set-variable 'password-cache-expiry nil))

;;(customize-set-variable 'tramp-default-method "scpc")
(customize-set-variable 'scheme-program-name "guile")
(customize-set-variable 'show-paren-mode t)


(require 'etags-select)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(customize-set-variable 'asm-comment-char ?#)
(defun my-asm-hook ()
  (setq asm-comment-char ?#))
(add-hook 'asm-mode-set-comment-hook 'my-asm-hook)

(defun my-asm-hook1 ()
  (local-set-key (kbd "TAB") 'indent-according-to-mode))
(add-hook 'asm-mode-hook 'my-asm-hook1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fortran helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To replace single-precision literals with double-precision ones
;; in fortran code
;; e.g. 1.0 -> 1.0_dp
;; 3.456E-10 -> 3.456E-10_dp
;; Unfortunately also matches double-precision literals.
(defun fortran-repalce-sp ()
  (interactive)
  (query-replace-regexp "\\([0-9]+\\.[0-9Ee+-]+\\)\\(_sp\\)?" "\\1_dp"))


(defun insert-to-end (char)
  (interactive "Mcharacter: ")
  (insert-char (aref char 0) (- 80 (current-column))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For making latex tables from tab-separated data
;; Really sloppy
(defun munge (point mark)
  (interactive "r")
  (replace-regexp "\\([0-9.-]\\)	\\([0-9-]\\)" "\\1 & \\2" nil point mark)
  (replace-regexp "^\\([0-9]\\)" "$\\1$" nil point (+ mark 40))
  (replace-regexp "\\([0-9.]\\)	*$" "\\1\\\\\\\\
    \\\\hline" nil point (+ mark 100))) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for my custom shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not 100% sure if the keyword option here works
(defun my-revert-buffer ()
  (interactive)
  (revert-buffer :ignore_auto nil))


(defvar scroll-divisor 5)
(defvar mega-scroll-divisor 2)

(defun my-scroll-up ()
  (interactive)
  (scroll-up (/ (window-height) scroll-divisor)))

(defun my-mega-scroll-up ()
  (interactive)
  (scroll-up (/ (window-height) mega-scroll-divisor)))

(defun my-scroll-down ()
  (interactive)
  (scroll-down (/ (window-height) scroll-divisor)))

(defun my-mega-scroll-down ()
  (interactive)
  (scroll-down (/ (window-height) mega-scroll-divisor)))

(defun my-forward-lines ()
  (interactive)
  (forward-line 4))

(defun my-backward-lines ()
  (interactive)
  (previous-line 4))

(defun eval-region-or-defun ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'eval-region)
    (call-interactively 'eval-defun)))

(defun is-whitespace (char)
  (or (eql char (aref " \t\n" 0))
      (eql char (aref " \t\n" 1))
      (eql char (aref " \t\n" 2))))
 
(defun delete-forward-whitespace ()
  (interactive)
  (let ((orig-pos (point)))
    (delete-region
	 orig-pos
       (progn
	 (skip-chars-forward " \t")
	 (constrain-to-field nil orig-pos t)))))

(defconst my-word-chars "[:alnum:]\"'")
(defconst my-non-word-chars "^[:space:]\n[:alnum:]\"'")

(defun my-kill-word ()
  (interactive)
  (kill-region (point) (my-forward-word)))

(defun my-backward-kill-word ()
  (interactive)
  (kill-region (point) (my-backward-word) (point)))



;; Havne't quite decided which behavior I like better. I do think having it set
;; to 't is a little more flexible, because one can always use forward-char if
;; one just wants to go forward one character.
;;
;; Of course since these functions are used in my kill functions, the same rules
;; will apply there too.

(defvar skip-prefixed 't
"If true, will make my-forward-word and my-backward-word traverse
over a word prefixed by a non-word-character as if it were one
entity. E.g.

   abc!-def-ghi -> abc-def!-ghi
      ^ point             ^

Because these functions are used in my kill-word functions, the
same behavior will apply there.")

(defun my-forward-word ()
  (interactive)
  (let ((c (char-after))
	(cp (char-after (+ 1 (point)))))
    (cond ((is-word-char c) (skip-chars-forward my-word-chars))
	  ((and skip-prefixed
		(is-non-word-char c)
		(is-word-char cp))
	   (forward-char)
	   (skip-chars-forward my-word-chars))
	  ((is-whitespace c) (skip-chars-forward "[:space:]\n"))
	  ('t (skip-chars-forward my-non-word-chars))))
  (point))

(defun my-backward-word ()
  (interactive)
  (let ((c (char-before))
	(cp (char-before (- (point) 1))))
    (cond ((is-word-char c) (skip-chars-backward my-word-chars))
	  ((and skip-prefixed
		(is-non-word-char c)
		(is-word-char cp))
	   (backward-char)
	   (skip-chars-backward my-word-chars))
	  ((is-whitespace c) (skip-chars-backward "[:space:]\n"))
	  ('t (skip-chars-backward my-non-word-chars))))
  (point))

(defun my-forward-whitespace ()
  (interactive)
  (if (is-whitespace (char-after))
      (skip-chars-forward "[:space:]\n")
    (skip-chars-forward "^[:space:]\n")))

(defun my-backward-whitespace ()
  (interactive)
  (if (is-whitespace (char-before))
      (skip-chars-backward "[:space:]\n")
    (skip-chars-backward "^[:space:]\n")))


(defun is-word-char (char)
  (if (string-match (concat "[" my-word-chars "]")
		    (char-to-string char))
      't
    nil))

(defun is-non-word-char (char)
  (if (or (string-match (concat "[" my-non-word-chars "]")
		    (char-to-string char))
	  (eql (aref " " 0) char))
      't
    nil))

(defun my-beginning-of-line ()
  "Moves to the beginning of the line text if the point is not
already there or at the beginning of the line. If it is, moves to
the beginning of the line."
  (interactive)
  (let ((p (point)))
    (beginning-of-line-text)
    (if (eql p (point))
	(beginning-of-line))))

(defun insert-stars ()
  (interactive)
  (insert-to-end "*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "s-.") 'imenu)
(global-set-key [f9] 'next-error)

(global-set-key (kbd "C-*") 'insert-stars)

(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-M-;") 'uncomment-region)

(add-hook 'python-mode-hook 
	  '(lambda () (local-set-key [f1] 'python-complete-symbol)))

(global-set-key (kbd "C-<backspace>") 'my-backward-kill-word)
(global-set-key (kbd "M-d") 'my-kill-word)
(global-set-key (kbd "C-<delete>") 'my-kill-word)
(global-set-key (kbd "M-<delete>") 'my-kill-word)

(global-set-key (kbd "C-<right>") 'my-forward-word)
(global-set-key (kbd "C-<left>")  'my-backward-word)

(global-set-key (kbd "M-<right>") 'my-forward-whitespace)
(global-set-key (kbd "M-<left>")  'my-backward-whitespace)

(global-set-key (kbd "C-,") 'my-beginning-of-line)
(global-set-key [f5] 'my-revert-buffer)

(global-set-key (kbd "<next>")      'my-scroll-up)
(global-set-key (kbd "<prior>")     'my-scroll-down)
(global-set-key (kbd "M-<next>")    'my-mega-scroll-up)
(global-set-key (kbd "M-<prior>")   'my-mega-scroll-down)

(global-set-key (kbd "C-<down>")    'my-forward-lines)
(global-set-key (kbd "C-<up>")      'my-backward-lines)
(global-set-key (kbd "M-<down>")    'forward-paragraph)
(global-set-key (kbd "M-<up>")      'backward-paragraph)
(global-set-key (kbd "A-<down>")    'forward-paragraph)
(global-set-key (kbd "A-<up>")      'backward-paragraph)

(global-set-key (kbd "C-x C-a")     'recentf-open-files)

(global-set-key (kbd "C-M-<down>")  'shrink-window)
(global-set-key (kbd "C-M-<up>")    'enlarge-window)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<left>")  'shrink-window-horizontally)

;; I should try to mod this so it can indent backward and indent even
;; if the cursor is placed in the middle of a word.
;; Follow the help for indent-relative-maybe to the lisp code for it.
(global-set-key (kbd "C-<tab>")	'indent-relative-maybe)

;; Why doesn't this work with C-M-x ?
(global-set-key (kbd "C-A-x")  'eval-region-or-defun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode 1)

(setq imenu-auto-rescan 1)
(add-hook 'c-mode-hook 'imenu-add-menubar-index)
(add-hook 'python-mode-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'imenu-add-menubar-index)
(add-hook 'TeX-mode-hook 'imenu-add-menubar-index)
(add-hook 'TeX-PDF-mode-hook 'imenu-add-menubar-index)
(add-hook 'docTeX-PDF-mode-hook 'imenu-add-menubar-index)
(add-hook 'f90-mode-hook 'imenu-add-menubar-index)
(add-hook 'fortran-mode-hook 'imenu-add-menubar-index)
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'lisp-mode-hook 'imenu-add-menubar-index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The python.org python-mode.el (as opposed to the GNU Emacs python.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-hook 'python-mode-hook
;	  '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; (when (file-accessible-directory-p "~/emacs/python-mode-1.0-patched")
;;     (add-to-list 'load-path "~/emacs/python-mode-1.0-patched/")
;;     (autoload 'python-mode "python-mode" "Python Mode." t)
;;     (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;     (add-to-list 'interpreter-mode-alist '("python" . python-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autosave and backup files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!

(defvar autosave-dir
 (concat "/home/" (user-login-name) "/.backups/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/home/" (user-login-name) "/.backups"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Silly things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
			   when (commandp s) collect s))
	   (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\\n"
	       "========================\\n\\n"
	       (describe-function command)
	       "\\n\\nInvoke with:\\n\\n"
	       (with-temp-buffer
		 (where-is command t)
		 (buffer-string)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (custom-set-faces
   '(default ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil \.\.\.))))
   '(font-lock-builtin-face ((((class color) (min-colors 88) (background light)) (:underline t))))
   '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face)) (((class color) (min-colors 16)) nil)))
   '(font-lock-comment-face ((t (:foreground "#830000" :slant italic))))
   '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "#0F0051"))))
   '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
   '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:box (:line-width 1 :color "grey50") :weight bold))))
   '(font-lock-keyword-face ((t (:foreground "#0F0051" :weight bold))))
   '(font-lock-negation-char-face ((t (:foreground "DarkRed"))))
   '(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground "DarkCyan"))))
   '(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground "DarkCyan"))))
   '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "dark slate grey"))))
   '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:underline "grey50" :weight bold))))
   '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "#1E3149"))))
   '(mode-line ((t (:background "#f6f6f6" :foreground "grey10" :box (:line-width 1 :color "grey50")))))
   '(mode-line-inactive ((default (:inherit mode-line :foreground "grey30")) (((class color) (min-colors 88) (background light)) nil)))
   '(ps-footer-font-size (quote (10 . 10)))
   '(ps-header-font-size (quote (10 . 10)))
   '(ps-header-title-font-size (quote (10 . 10)))
   '(region ((((class color) (min-colors 88) (background light)) (:background "#BDD5FC"))))
   '(sh-heredoc ((((class color) (background light)) (:foreground "dark slate gray"))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macbook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string= (system-name) "Physics-06-15")
  (when window-system
    (custom-set-variables
     ;; custom-set-variables was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
     '(scheme-program-name "guile-1.6")
     '(aquamacs-additional-fontsets nil t)
     '(aquamacs-customization-version-id 100 t)
     '(aquamacs-default-styles (quote ((default (color-theme color-theme-snapshot)
					 (font . "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1") (tool-bar-lines . 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Winter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when (string= (system-name) "Winter")
  (customize-set-variable 'lpr-command "gtklp")
  (customize-set-variable 'ps-lpr-command "gtklp")

  (customize-set-variable 'ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
  (customize-set-variable 'ecb-layout-window-sizes 
			  (quote (("left8" (0.18181818181818182
					    . 0.2878787878787879) (0.18181818181818182
					    . 0.24242424242424243) (0.18181818181818182
					    . 0.2878787878787879) (0.18181818181818182
					    . 0.16666666666666666))))))

;;;****************************************************************************
;;; Automatic emacs customizations
;;;****************************************************************************

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(directory-free-space-program "df")
 '(dired-listing-switches "-alh")
 '(ecb-auto-update-methods-after-save nil)
 '(ecb-expand-methods-switch-off-auto-expand nil)
 '(ecb-non-semantic-methods-initial-expand (quote (f90-mode)))
 '(ecb-options-version "2.40" t)
 '(ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|mod\\|cache\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-source-path (quote (("/home/chris/Projects/diag2/src" "diag2") ("/home/chris/Projects/afmc/current/source" "afmc") ("/home/chris/Projects/contact_int/src" "contact_int") ("/" "/") ("/home/chris/Projects/QHS" "QHS") ("/home/chris/Projects/QM/src" "QM") ("/home/chris/Projects/diag3/src" "diag3") ("/home/chris/Documents/Research/coldatom/code" "coldatom code") ("/home/chris/Projects/sym/src" "sym") ("/home/chris/Projects/exact_therm/src" "exact_therm"))))
 '(ecb-stealthy-tasks-delay 0.2)
 '(eshell-prompt-function (lambda nil (concat (eshell/basename (eshell/pwd)) (if (= (user-uid) 0) " # " " $ "))))
 '(f90-smart-end (quote noblink))
 '(fill-column 80)
 '(grep-command "grep -nH -r -i -e ")
 '(list-directory-verbose-switches "-l")
 '(ps-footer-font-size (quote (9 . 9)))
 '(ps-header-font-size (quote (9 . 9)))
 '(ps-header-title-font-size (quote (10 . 10)))
 '(recentf-max-menu-items 20)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 0)
 '(scroll-margin 0)
 '(scroll-step 6)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tex-dvi-view-command (quote (cond ((eq window-system (quote x)) "evince") ((eq window-system (quote w32)) "yap") (t "dvi2tty * | cat -s"))))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(visual-scroll-margin 0)
 '(x-select-enable-clipboard t))

