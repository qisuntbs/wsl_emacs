;; qi's .emacs file 2019-11-21
;; list-colors-display
;; list-faces-display
;; win cmd - colors :
;;   Scrren Background 38,38,38
;;   Screen Text 198,198,198
;; win cmd - options:
;;   Cursor Size - Large, uncheck
;;   Quick Edit Mode, Enable Ctrl key shortcuts
;; (set-keyboard-coding-system nil) ;this is for iterm2 only
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
(setq standard-indent 2) ;indent as 2
(setq-default indent-tabs-mode t)

(setq scroll-conservatively 10000) ;smooth-scrolling
;; copy paste with clipboard
;; see detail: http://stackoverflow.com/questions/9985316/how-to-paste-to-emacs-from-clipboard

(global-font-lock-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;;gui only:
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
;(setq version-control t)
;; Save all backup file in this directory.

;; Disables
;; menu bar etc.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

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

;; no sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; bracket highlight
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
;; (set-face-foreground 'show-paren-match-face "red")
;; (set-face-background 'show-paren-mismatch-face "red") ;mismatch allert

;; (defun my-show-paren-any (orig-fun)
;;   (or (funcall orig-fun)
;;       (if (looking-at "\\s)")
;;           (save-excursion (forward-char 1) (funcall orig-fun)))))
;; (add-function :around show-paren-data-function #'my-show-paren-any)

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
;; (set-face-foreground 'font-lock-string-face "white")
 (set-face-foreground 'font-lock-comment-face "green") ; comment
 (set-face-foreground 'font-lock-keyword-face "magenta") ; function, if, end, etc.
 (set-face-foreground 'font-lock-variable-name-face "lightred") ; i, j, etc.
 (set-face-foreground 'font-lock-constant-face "brown") ; numbers, etc
 (set-face-foreground 'font-lock-string-face "default") ; string
 (set-face-foreground 'font-lock-builtin-face "brown")
 ;; (set-face-foreground 'font-lock-function-name-face "magenta") ; func name
 (set-face-foreground 'font-lock-function-name-face "brown") ; func name
 ;;(set-face-foreground 'mode-line-inactive "white")
 (set-face-background 'mode-line-inactive "default")
 (set-face-foreground 'font-lock-type-face "cyan") ; &rset figure(matlab)
 ;; (set-face-attribute 'region nil :background "darkgray")
 )

;;elpy
(elpy-enable)
(setq python-shell-interpreter "ipython3"
    python-shell-interpreter-args "--simple-prompt -i")
(delete 'elpy-module-highlight-indentation elpy-modules) ; no highligh
(eval-after-load "elpy"
  '(define-key elpy-mode-map (kbd "C-c C-r") 'python-shell-send-region))
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
;; Could not use Consolas Font:
;; https://github.com/microsoft/terminal/issues/367

(when-term
 ;; copy in wsl
 (defun wsl-copy (&optional b e)
   (interactive "r")
   (call-process-region b e "clip.exe")
   (deactivate-mark nil)
   )
 (global-set-key (kbd "M-w") 'wsl-copy)
 ;; paste in wsl
 (defun wsl-paste ()
   (interactive)
   (let ((coding-system-for-read 'dos)
	 (default-directory "/mnt/c/"))
     (insert (shell-command-to-string
	      "powershell.exe -command 'Get-Clipboard'"))))
 (global-set-key (kbd "C-y") 'wsl-paste)
 )


(add-hook 'org-mode-hook 'auto-complete-mode)

;; yas-mode
;; (add-to-list 'load-path
;;              "~/.emacs.d/elpa/yasnippet-0.14.0")
;; install yasnippet-snippets at first!
(require 'yasnippet)
(add-hook 'c++-mode-hook #'yas-minor-mode)

;; (setq explicit-shell-file-name "C:/MinGW/msys/1.0/bin/bash.exe")
;; (setq shell-file-name "bash")
;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
;; (setenv "SHELL" shell-file-name)
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; no *Message* buffer
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; switch buffer by skipping certain buffers
;; https://emacs.stackexchange.com/questions/17687/make-previous-buffer-and-next-buffer-to-ignore-some-buffers
(defcustom my-skippable-buffers '("*Messages*" "*scratch*" "*Help*" "*compilation*" "*Completions*")
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

(global-set-key [f1] 'my-previous-buffer)
(global-set-key [f2] 'my-next-buffer)

(global-set-key (kbd "<f3>") 'compile)
(global-set-key (kbd "<f5>") 'save-buffer)

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

