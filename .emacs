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
(global-font-lock-mode t)
(mouse-wheel-mode t)

(setq inhibit-startup-message t)

(require 'recentf)
(recentf-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; icicles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(when (file-accessible-directory-p "~/local/emacs/icicles")
;  (add-to-list 'load-path "~/local/emacs/icicles/")
;  (require 'icicles))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts
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
      (eval-region)
    (eval-defun)))

(defun kill-word-or-delete-whitespace ()
  (interactive)
  (if (is-whitespace (char-after))
      (delete-forward-whitespace)
    (kill-word 1)))

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


(global-set-key (kbd "M-f") 'forward-whitespace)
(global-set-key (kbd "M-d") 'kill-word-or-delete-whitespace)
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
