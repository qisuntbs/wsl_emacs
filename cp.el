;; qi's .emacs
;; for competitive programming
;; supporting wsl emacs-nox

(prefer-coding-system 'utf-8-unix)
(setq inhibit-startup-message t) ;disable emacs welcome

;;MELPA
(require 'package)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(add-hook 'org-mode-hook 'auto-complete-mode)
(require 'yasnippet)
(add-hook 'c++-mode-hook #'yas-minor-mode)
(setq mode-require-final-newline nil)  ;; no additional line EOF
(menu-bar-mode -1)  ;; no menu

;; indent settings
(setq standard-indent 2)
(setq scroll-conservatively 10000) ;; smooth-scrolling
(global-font-lock-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-display-table-slot standard-display-table 'wrap ?\ ) ;remove line wrap

;; file ends with ~: enable backup files.
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Scrolling behavior for emacs-nox
(defun nox-scroll-down ()
  (interactive)
  (scroll-up 5))
(defun nox-scroll-up ()
  (interactive)
  (scroll-down 5))
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'nox-scroll-up)
  (global-set-key (kbd "<mouse-5>") 'nox-scroll-down))

;; no sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq default-major-mode 'text-mode)
(xterm-mouse-mode 1)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))  ;; shift+arrow

;; Ctrl-v to scroll half page
(global-set-key [(control ?v)]
		(lambda () (interactive (next-line (/ (window-height (selected-window)) 3)))))
(global-set-key [(meta ?v)]
		(lambda () (interactive (previous-line (/ (window-height (selected-window)) 3)))))

;; paren highlight
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "red")

;; smex
(require 'smex) ; Not needed if you use package.el
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(set-face-foreground 'font-lock-comment-face "green") ; comment
(set-face-foreground 'font-lock-keyword-face "brightred") ; function, if, end, etc.
(set-face-foreground 'font-lock-variable-name-face "default") ; i, j, etc.
(set-face-foreground 'font-lock-constant-face "color-133") ; numbers, etc
(set-face-foreground 'font-lock-string-face "yellow") ; string
(set-face-foreground 'font-lock-builtin-face "brightred")  ; sum, len, round
(set-face-foreground 'font-lock-function-name-face "brightblue") ; func name
(set-face-foreground 'mode-line-inactive "color-241")
(set-face-foreground 'font-lock-type-face "cyan")
(set-face-foreground 'minibuffer-prompt "brightblue")  ; M-x, load-file, etc.
(set-face-attribute 'region nil :background "color-240")  ; bg color of selection

;; copy emacs selection to Windows clipboard
;; copy in wsl
(defun wsl-copy (&optional b e)
  (interactive "r")
  (call-process-region b e "clip.exe")
  (deactivate-mark nil)
  )
(global-set-key (kbd "M-w") 'wsl-copy)
;; kill in wsl
(defun wsl-kill (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "clip.exe")
  (delete-region b e)
  )
(global-set-key (kbd "C-w") 'wsl-kill)
;; kill-line in wsl
(defun wsl-linekill ()
  (interactive)
  (wsl-kill
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )
(global-set-key (kbd "C-k") 'wsl-linekill)
;; paste-wsl
;; https://www.emacswiki.org/emacs/Emacs_and_the_Windows_Subsystem_for_Linux
(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as string."
  (let ((coding-system-for-read 'dos))
    (substring				; remove added trailing \n
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard") 0 -1)))
(defun wsl-paste-from-clipboard (arg)
  "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
  (interactive "P")
  (let ((clip (wsl-clipboard-to-string)))
    (insert clip)
    (if arg (kill-new clip))))
(global-set-key (kbd "C-y") 'wsl-paste-from-clipboard)

;; Enable Evil
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-insert-state-map "\C-a" 'beginning-of-line)
(define-key evil-visual-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-motion-state-map "\C-a" 'evil-beginning-of-line)
(define-key evil-visual-state-map "\C-u" 'evil-scroll-up)
(define-key evil-motion-state-map "\C-d" 'evil-scroll-down)
(define-key evil-insert-state-map "\C-c" 'evil-force-normal-state)
(define-key evil-visual-state-map "\C-c" 'evil-force-normal-state)

;; python
(setq python-shell-interpreter "ipython3"
    python-shell-interpreter-args "--simple-prompt -i")