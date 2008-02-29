;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode)
(global-font-lock-mode t)
(mouse-wheel-mode t)

(setq inhibit-startup-message t)

(require 'recentf)
(recentf-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-f") 'forward-whitespace)
(global-set-key "" (quote beginning-of-line-text))

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
;; customize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(string= (system-name) "Physics-06-15")

(when (string= (system-name) "Physics-06-15")
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(recentf-max-menu-items 20)
   '(blink-cursor-mode nil)
   '(fill-column 80)
   '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
   '(scheme-program-name "guile-1.6")
   '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
   '(transient-mark-mode t))
  '(aquamacs-additional-fontsets nil t)
  '(aquamacs-customization-version-id 100 t)
  '(aquamacs-default-styles (quote ((default (color-theme color-theme-snapshot)
				      (font . "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1") (tool-bar-lines . 0))))))


(when (string= (system-name) "Autumn")
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(recentf-max-menu-items 20)
   '(blink-cursor-mode nil)
   '(fill-column 80)
   '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
   '(scheme-program-name "guile-1.6")
   '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
   '(transient-mark-mode t)))


(when window-system
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil \.\.\.))))
   '(font-lock-builtin-face ((((class color) (min-colors 88) (background light)) (:underline t :weight bold))))
   '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face :weight bold)) (((class color) (min-colors 16)) nil)))
   '(font-lock-comment-face ((t (:foreground "bisque4" :slant italic))))
   '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "DarkCyan"))))
   '(font-lock-doc-face ((t (:inherit font-lock-string-face))))
   '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:box (:line-width 1 :color "grey50") :weight bold))))
   '(font-lock-keyword-face ((t (:foreground "DarkBlue" :weight bold))))
   '(font-lock-negation-char-face ((t (:foreground "DarkRed"))))
   '(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground "DarkCyan"))))
   '(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground "DarkCyan"))))
   '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:background "LightYellow1" :foreground "grey30"))))
   '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:underline "grey50" :weight bold))))
   '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "DarkGoldenrod4" :weight bold))))
   '(mode-line ((t (:background "#f6f6f6" :foreground "grey30" :box (:line-width 1 :color "grey50")))))
   '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) nil)))))
