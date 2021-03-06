;; qi's .emacs file 2016/03
;; supporting emacs-nox
;; list-colors-display
;; list-faces-display
;; win cmd - colors :
;;   Scrren Background 38,38,38
;;   Screen Text 198,198,198
;; win cmd - options:
;;   Cursor Size - Large, uncheck
;;   Quick Edit Mode, Enable Ctrl key shortcuts

(prefer-coding-system 'utf-8-unix)
(setq inhibit-startup-message t) ;disable emacs welcome

;;MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(ac-flyspell-workaround)
(ac-linum-workaround)

;indent settings
(setq standard-indent 2)
(setq-default indent-tabs-mode t)  ;; elpy indention of 2 if set to nil

(setq scroll-conservatively 10000) ;smooth-scrolling
(global-font-lock-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;;Define a macro that alone codes working under gui only
(defun is-in-gui()
  (display-graphic-p))
(defmacro when-gui (&rest body)
  `(when (is-in-gui) ,@body))
;;Define a macro that alone codes working under terminal only
(defun is-in-terminal()
      (not (display-graphic-p)))
(defmacro when-term (&rest body)
  "Works just like `progn' but will only evaluate expressions in VAR when Emacs is running in a terminal else just nil."
  `(when (is-in-terminal) ,@body))

;file ends with ~: enable backup files.
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
;; (setq make-backup-files t)

;; Enable versioning with default values (keeping the last five?)
;; (setq version-control t)
;; Save all backup file in this directory.

;; Disable menu bar etc.
;; EDITED (emacs-nox)
(menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (blink-cursor-mode -1)
;; Enable mouse support
;; replaces forward-sentence

;; Scrolling behavior for emacs-nox
(defun nox-scroll-down ()
  (interactive)
  (scroll-up 5))
(defun nox-scroll-up ()
  (interactive)
  (scroll-down 5))
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'nox-scroll-up)
  (global-set-key (kbd "<mouse-5>") 'nox-scroll-down)
)

;; no sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq default-major-mode 'text-mode)

;; enable visual feedback on selections
(setq transient-mark-mode t)
;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
;; default to unified diffs
(setq diff-switches "-u")
(require 'xt-mouse)
(xterm-mouse-mode 1)
;; (mouse-wheel-mode t)

;; Window move, Shift+Arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Ctrl-v to scroll half page
;; (global-set-key [(control ?v)]
;; 		(lambda () (interactive (next-line (/ (window-height (selected-window)) 2)))))
;; (global-set-key [(meta ?v)]
;; 		(lambda () (interactive (previous-line (/ (window-height (selected-window)) 2)))))

;; bracket highlight
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "red")
;; ALTERNATIVE CHOICE:
;; (require 'highlight-parentheses)
;; (add-hook 'python-mode-hook 'highlight-parentheses-mode)
;; (add-hook 'c++-mode-hook 'highlight-parentheses-mode)

(set-display-table-slot standard-display-table 'wrap ?\ ) ;remove line wrap
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-startup-folded "showall")
(setq org-export-with-section-numbers nil)

;; smex
(require 'smex) ; Not needed if you use package.el
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(when-term
 (set-face-foreground 'font-lock-comment-face "green") ; comment
 (set-face-foreground 'font-lock-keyword-face "brightblue") ; function, if, end, etc.
 (set-face-foreground 'font-lock-variable-name-face "default") ; i, j, etc.
 (set-face-foreground 'font-lock-constant-face "color-133") ; numbers, etc
 (set-face-foreground 'font-lock-string-face "yellow") ; string
 (set-face-foreground 'font-lock-builtin-face "brightred")  ; sum, len, round
 (set-face-foreground 'font-lock-function-name-face "brightred") ; func name
 (set-face-foreground 'mode-line-inactive "color-241")
 (set-face-foreground 'font-lock-type-face "cyan")
 (set-face-foreground 'minibuffer-prompt "brightblue")  ; M-x, load-file, etc.
 ;; (set-face-attribute 'region nil :background "#44475")
 (set-face-attribute 'region nil :background "color-240")  ; bg color of selection
)

;; elpy
;; (elpy-enable)
(setq python-shell-interpreter "ipython3"
    python-shell-interpreter-args "--simple-prompt -i")
;; (delete 'elpy-module-highlight-indentation elpy-modules) ; no highligh
;; (eval-after-load "elpy"
;;   '(define-key elpy-mode-map (kbd "C-c C-r") 'python-shell-send-region))
;; (setq elpy-rpc-python-command "python3")
;; (setq elpy-rpc-virtualenv-path 'current)

;; copy emacs selection to Windows clipboard
(when-term
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
 ) ;; end of when-term

(add-hook 'org-mode-hook 'auto-complete-mode)
(require 'yasnippet)
(add-hook 'c++-mode-hook #'yas-minor-mode)

;; switch buffer by skipping certain buffers
;; https://emacs.stackexchange.com/questions/17687/make-previous-buffer-and-next-buffer-to-ignore-some-buffers
(defcustom my-skippable-buffers '("*Messages*"
				  "*scratch*"
				  "*Help*"
				  "*compilation*"
				  "*Completions*"
				  "*Disabled Command*"
				  "*Flymake log*"
				  "*Shell Command Output*"
				  "*Python*")
  "Buffer names ignored by `my-next-buffer' and `my-previous-buffer'."
  :type '(repeat string))

(defun my-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) my-skippable-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun my-next-buffer ()
  "Variant of `next-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'next-buffer))

(defun my-previous-buffer ()
  "Variant of `previous-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'previous-buffer))
;; RE-binding keys:
(global-set-key [f1] 'my-previous-buffer)
(global-set-key [f2] 'my-next-buffer)
(global-set-key (kbd "<f3>") 'compile)
(global-set-key (kbd "<f5>") 'save-buffer)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

(setq mode-require-final-newline nil)  ;; no additional line EOF

;; Evil
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