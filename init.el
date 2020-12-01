;; qi's .emacs file 2020-11-28
;; for WSL 2
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
(setq make-backup-files t)
;; Enable versioning with default values (keep five last versions, I think!)
;; (setq version-control t)
;; Save all backup file in this directory.

;; Disable menu bar etc.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
;; no sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq default-major-mode 'text-mode)

;;; Mouse and scroll modes
;; enable visual feedback on selections
(setq transient-mark-mode t)
;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
;; default to unified diffs
(setq diff-switches "-u")
;; (require 'un-define)
(require 'xt-mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;Window move, Shift+Arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; scroll half screen
;; Ctrl-v to scroll half page
(global-set-key [(control ?v)]
		(lambda () (interactive (next-line (/ (window-height (selected-window)) 2)))))
(global-set-key [(meta ?v)]
		(lambda () (interactive (previous-line (/ (window-height (selected-window)) 2)))))

;; bracket highlight
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
;; (set-face-foreground 'show-paren-match-face "red")
;; (set-face-background 'show-paren-mismatch-face "red") ;mismatch allert

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
 (set-face-foreground 'font-lock-variable-name-face "yellow") ; i, j, etc.
 (set-face-foreground 'font-lock-constant-face "color-133") ; numbers, etc
 (set-face-foreground 'font-lock-string-face "default") ; string
 (set-face-foreground 'font-lock-builtin-face "brightred")  ; sum, len, round
 (set-face-foreground 'font-lock-function-name-face "brightred") ; func name
 (set-face-foreground 'mode-line-inactive "color-241")
 (set-face-foreground 'font-lock-type-face "cyan")
 (set-face-foreground 'minibuffer-prompt "brightblue")  ; M-x, load-file, etc.
 ;; (set-face-attribute 'region nil :background "#44475")
 (set-face-attribute 'region nil :background "color-240")  ; bg color of selection
)

;;elpy
(elpy-enable)
(setq python-shell-interpreter "ipython3"
    python-shell-interpreter-args "--simple-prompt -i")
(delete 'elpy-module-highlight-indentation elpy-modules) ; no highligh
(eval-after-load "elpy"
  '(define-key elpy-mode-map (kbd "C-c C-r") 'python-shell-send-region))
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-virtualenv-path 'current)

;; hunspell setup for latex
;; https://www.reddit.com/r/emacs/comments/dgj0ae/tutorial_spellchecking_with_hunspell_170_for/
;; https://sourceforge.net/projects/ezwinports/files/hunspell-1.3.2-3-w32-bin.zip/download
;; (setq ispell-program-name "hunspell")
;; (setq ispell-hunspell-dict-paths-alist
;;       '(("en_US" "C:/Users/Qi/Documents/emacs/hunspell/share/hunspell/en_US.aff")))
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
;; (flyspell-mode 1)
;; (global-set-key (kbd "M-\\") 'ispell-word)
;; ;; (dolist (hook '(text-mode-hook))
;; ;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; (add-hook 'text-mode-hook 'auto-complete-mode)

;; (defun cpp-compile ()
;;   (local-set-key (kbd "C-c C-r") 'compile))
;; (add-hook 'c++-mode-hook 'cpp-compile)

;; (add-to-list 'load-path "~/.emacs.d/elpa/cpplint")

;; (flycheck-mode 1)
;; (custom-set-variables
;;  '(flycheck-c/c++-googlelint-executable "C:/Users/Qi/AppData/Local/Programs/Python/Python37/Scripts/cpplint.exe"))
;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      (flycheck-add-next-checker 'c/c++-cppcheck
;;                                 'c/c++-googlelint 'append))) 
;; (setq-default indent-tabs-mode nil)
;; (setq-default c-basic-offset 2)

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
;; yas-mode
;; (add-to-list 'load-path
;;              "~/.emacs.d/elpa/yasnippet-0.14.0")
;; install yasnippet-snippets at first!
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
				  "*Shell Command Output*")
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

