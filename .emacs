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
;;     C-x 4 .: find-tag-other-windo
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;(customize-set-variable 'asm-comment-char ?#)
(defun my-asm-hook ()
  (setq asm-comment-char ?#))
(add-hook 'asm-mode-set-comment-hook 'my-asm-hook)

(require 'etags-select)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(fset 'grab-energies
   (lambda (&optional arg) "Keyboard
   macro." (interactive "p") (kmacro-exec-ring-item (quote ([19
   108 101 118 101 108 115 13 18 74 122 42 50 44 32 13 1 67108896
   down down 134217847 24 111 25 return 24 111 19 108 101 118 101
   108 115 32 58 13 down 1 67108896 19 35 13 1 134217847 24 111
   25 24 111] 0 "%d")) arg)))


;; To bind a keyboard macro to f6: first call kmacro-name-last-macro
;; Then copy this line to a scratch buffer, modify and execute
;(global-set-key [f6] 'grab-energies)
;; I have this here because using the macro ring is sometimes difficult

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
;; Functions for working with ROBODOC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun robo-module (name description)
  (interactive "MName of module: \nMOne-line-description: ")
  (let ((doc
	 (concat
	  "!*******************************************************************************\n"
	  "!****h*" name "/" name "\n"
	  "! NAME" "\n"
	  "!   " name " -- " description "\n"
	  "!*******************************************************************************\n"
	  "! 2008 Chris Gilbreth \n"
	  "!*******************************************************************************\n\n")))
    (insert doc)))

(defun robo-function-extended (name description)
  (interactive "MName of function: \nMOne-line-description: ")
  (let ((doc
	 (concat
	  "!*******************************************************************************\n"
	  "!****f* <module>" "/" name "\n"
	  "! NAME\n"
	  "!   " name " -- " description "\n"
	  "! SYNOPSIS\n"
	  "!   \n"
	  "! INPUTS\n"
	  "!   *  - (in) \n"
	  "!   *  - (out) \n"
	  "! NOTES\n"
	  "!   \n"
	  "!*******************************************************************************\n")))
    (insert doc)))

(defun robo-function-concise (name description)
  (interactive "MName of function: \nMOne-line-description: ")
  (let ((doc
	 (concat
	  "!*******************************************************************************\n"
	  "!****f* <module>" "/" name "\n"
	  "! NAME\n"
	  "!   " name " -- " description "\n"
	  "! NOTES\n"
	  "!   \n"
	  "!*******************************************************************************\n")))
    (insert doc)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fortran helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fortran-add-variable (name type)
  (interactive "MName of variable: \nMType (including kind): ")
  (message "You typed %s and %s" name type))

(defvar line-comment-length 40)
(defun fortran-add-line-comment (str)
  (interactive "Mstring: ")
  (insert (concat "!-- " str " "))
  (insert-char (aref "-" 0) (- line-comment-length (length str) 6))
  (insert "!"))

		  
		  

;; To replace single-precision literals with double-precision ones
;; in fortran code
;; e.g. 1.0 -> 1.0_dp
;; 3.456E-10 -> 3.456E-10_dp
;; Unfortunately also matches double-precision literals.
(defun fortran-repalce-sp ()
  (interactive)
  (query-replace-regexp "\\([0-9]+\\.[0-9Ee+-]+\\)\\(_sp\\)?" "\\1_dp"))







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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "s-.") 'imenu)
(global-set-key [f9] 'next-error)

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

(when (file-accessible-directory-p "~/emacs/python-mode-1.0-patched")
    (add-to-list 'load-path "~/emacs/python-mode-1.0-patched/")
    (autoload 'python-mode "python-mode" "Python Mode." t)
    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
    (add-to-list 'interpreter-mode-alist '("python" . python-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autosave files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!

(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
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
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))

(setq backup-directory-alist (list (cons "." backup-dir)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color-theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(when (and window-system
;	   (file-accessible-directory-p "~/local/emacs/color-theme-6.6.0"))
;  (add-to-list 'load-path "~/local/emacs/color-theme-6.6.0")
;  (require 'color-theme)
;  (color-theme-initialize)
;  (color-theme-feng-shui))


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

(when 't
  (custom-set-variables
   '(scroll-conservatively 0)
   '(scroll-margin 4)
   '(scroll-step 6)
   '(visual-scroll-margin 4)
   '(recentf-max-menu-items 20)
   '(blink-cursor-mode nil)
   '(fill-column 80)
   '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
   '(transient-mark-mode t)
   '(f90-smart-end 'noblink)
   '(tool-bar-mode nil)))

(when window-system
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
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
     '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "#977563"))))
     '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:underline "grey50" :weight bold))))
     '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "#1E3149"))))
     '(mode-line ((t (:background "#f6f6f6" :foreground "grey10" :box (:line-width 1 :color "grey50")))))
     '(mode-line-inactive ((default (:inherit mode-line :foreground "grey30")) (((class color) (min-colors 88) (background light)) nil)))
     '(region ((((class color) (min-colors 88) (background light)) (:background "#BDD5FC"))))))


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
  (when window-system
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:stipple nil :background "#ffffff" :foreground "black"
		    :inverse-video nil :box nil :strike-through nil
		    :overline nil :underline nil :slant normal
		    :weight normal :height 120 :width normal
		    :family "terminus")))))

    (custom-set-variables
     ;; custom-set-variables was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:stipple nil :background "#ffffff" :foreground "black"
		    :inverse-video nil :box nil :strike-through nil
		    :overline nil :underline nil :slant normal
		    :weight normal :height 120 :width normal
		    :family "terminus")))))))

(put 'scroll-left 'disabled nil)
