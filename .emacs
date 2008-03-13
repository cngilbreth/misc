;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;
;;
;; Files:
;;
;;     open a recent file (recentf): C-x C-r
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
;;     C-<down>, C-<up>: move cursor by 4 lines (mine)
;;     <next>, <prior> (i.e. page up, down): scroll 5 lines (mine)
;;     M-<next>, M-<prior>: scroll 10 lines (mine)
;;     M-f: forward-whitespace (mine)
;;
;;
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

(require 'recentf)
(recentf-mode 't)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fortran helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fortran-add-variable (name type)
  (interactive "MName of variable: \nMType (including kind): ")
  (message "You typed %s and %s" name type))


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
;; icicles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(when (file-accessible-directory-p "~/local/emacs/icicles")
;  (add-to-list 'load-path "~/local/emacs/icicles/")
;  (require 'icicles))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for my custom shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not 100% sure if the keyword option here works
(defun my-revert-buffer ()
  (interactive)
  (revert-buffer :ignore_auto nil))

(defun my-scroll-up ()
  (interactive)
  (scroll-up 6))

(defun my-mega-scroll-up ()
  (interactive)
  (scroll-up 12))

(defun my-scroll-down ()
  (interactive)
  (scroll-down 6))

(defun my-mega-scroll-down ()
  (interactive)
  (scroll-down 12))

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



;; If true, will make my-forward-word and my-backward-word traverse over a
;; word prefixed by a non-word-character as if it were one entity. E.g.
;;
;;    abc!-def-ghi -> abc-def!-ghi
;;       ^ point             ^
;;
;; Havne't quite decided which behavior I like better. I do think having it set
;; to 't is a little more flexible, because one can always use forward-char if
;; one just wants to go forward one character.
;;
;; Of course since these functions are used in my kill functions, the same rules
;; will apply there too.

(defconst skip-prefixed 't)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-S-s") 'isearch-backward)

(global-set-key (kbd "C-<backspace>") 'my-backward-kill-word)
(global-set-key (kbd "M-d") 'my-kill-word)
(global-set-key (kbd "C-<delete>") 'my-kill-word)
(global-set-key (kbd "M-<delete>") 'my-kill-word)

(global-set-key (kbd "C-<right>") 'my-forward-word)
(global-set-key (kbd "C-<left>")  'my-backward-word)

(global-set-key (kbd "M-<right>") 'my-forward-whitespace)
(global-set-key (kbd "M-<left>")  'my-backward-whitespace)


(global-set-key (kbd "C-R") (quote beginning-of-line-text))
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
(add-hook 'c-mode-hook 'imenu-add-menubar-index)
(add-hook 'python-mode-hook 'imenu-add-menubar-index)
(add-hook 'latex-mode-hook 'imenu-add-menubar-index)
(add-hook 'tex-mode-hook 'imenu-add-menubar-index)
(add-hook 'f90-mode-hook 'imenu-add-menubar-index)
(add-hook 'fortran-mode-hook 'imenu-add-menubar-index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The python.org python-mode.el (as opposed to the GNU Emacs python.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-hook 'python-mode-hook
;	  '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

(when (file-accessible-directory-p "~/local/emacs/python-mode-1.0-patched")
    (add-to-list 'load-path "~/local/emacs/python-mode-1.0-patched/")
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
   '(tool-bar-mode 't)
   '(recentf-max-menu-items 20)
   '(blink-cursor-mode nil)
   '(fill-column 80)
   '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
   '(transient-mark-mode t)))

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
     '(mode-line ((t (:background "#f6f6f6" :foreground "grey30" :box (:line-width 1 :color "grey50")))))
     '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) nil)))
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
;; Autumn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when (string= (system-name) "Autumn")
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
			   :family "terminus"))))))
