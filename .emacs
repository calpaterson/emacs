;; .emacs

;; Fix the colours on the sun machines
(set-foreground-color "white")
(set-background-color "black")

;; Line numbering
(require 'linum)
(global-linum-mode)

;; Keybindings for playing with windowing
(global-set-key (kbd "C-x -") #'shrink-window)
(global-set-key (kbd "C-x C-_") #'shrink-window-horizontally)
(global-set-key (kbd "C-x +") #'enlarge-window)
(global-set-key (kbd "C-x C-+") #'enlarge-window-horizontally)
(global-set-key (kbd "C-c =") #'balance-windows)
;; Buffers
; Try and be like GNU Screen
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x \"") 'buffer-menu)
; A Small Nice Thing to improve deleting words
; FIXME: Add more word work things
; Don't forget transpose-*
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x \"") 'list-buffers)
; FIXME: Window cycling
;(global-set-key (kbd "C-x 8") 'other-window -1) ;broken
;(global-set-key (kbd "C-x 9") 'other-window)
; FIXME: Add window swapping
; FIXME: Window splitting
; FIXME: Write a keybinding to delete a window
(global-set-key (kbd "C-x v") 'split-window-horizontally)
(global-set-key (kbd "C-x m") 'split-window-vertically)
(global-set-key (kbd "C-p") 'kill-region)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "M-/") 'dabbrev-expand)

; Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;; Add auto-completion
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete)
(global-auto-complete-mode t)

; All Languages
; Always do syntax highlighting
(global-font-lock-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)
(setq scroll-conservatively 1) ; scroll line by line with no jumps

; Turn on column numbers
(column-number-mode t)

; Turn on ido-mode
(ido-mode t)

; Pasting shortcuts
(global-set-key (kbd "<C-mouse-2>") #'kill-region)
(global-set-key (kbd "<C-mouse-3>") #'yank)

; The GUI is really quite annoying so turn it all off
; These seems to be a void variables for the debian emacs22-nox package
; -- except now the GUI is lovely and xft'd!  Turn it all on!
;(menu-bar-mode -1)
(tool-bar-mode -1) 
(scroll-bar-mode -1)

; Set the height of the window
(if (window-system) (set-frame-size (selected-frame) 132 33))

; FIXME: Figure out how copy and paste work

; FIXME: Tab completion
; IDEA: Etags

; FIXME: Add double line jumping

; Text functions
; FIXME: Make C-w delete only string of alphanumeric characters

; Start the emacs server
(server-start)

;; Colour Themes
;; (add-to-list 'load-path "~/.emacs.d/color-theme/color-theme-6.6.0/color-theme.el")
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-hober)))

; Text
(add-hook 'text-mode-hook
	  (lambda ()
	    (auto-fill-mode t)
	    (set-variable 'fill-column 80)))

; C
(add-hook 'c-mode-hook
	  (lambda ()
	    (c-set-style "gnu")
	    (local-set-key (kbd "C-c C-c") #'compile)))

(setq compilation-window-height 8)

(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            (message "compilation errors, press C-x ` to visit")
	  (message "Compile success."))))


; Latex
(add-hook 'latex-mode-hook
	  (lambda ()
	    (add-to-list 'load-path "~/.emacs.d/latex/auctex-11.85/")
	    (load "auctex.el" nil t t)
	    (LaTeX-mode)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq reftex-plug-in-to-AUCTeX t)
	    (TeX-PDF-mode)
	    (reftex-mode)))

; Erlang
; this is requires debian, so is not portable, and thus uncommented
;; (setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.1/emacs"
;; 		       load-path))
;; (setq erlang-root-dir "/usr/lib/erlang")
;; (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;; (require 'erlang-start)


; Python
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-to-list 'load-path "~/.emacs.d/python/python-mode-1.0")
	    (add-to-list 'load-path "~/.emacs.d/python/")
;; 	    (add-to-list 'load-path "~/.emacs.d/python/pymacs")
;; 	    (add-to-list 'load-path "~/.emacs.d/python/ropemacs")
;; 	    (require 'pymacs)
	    (require 'ipython)
	    (auto-fill-mode)
;;	    (pymacs-load "ropemacs" "rope-")
	    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
	    (add-to-list 'interpreter-mode-alist '("python" . python-mode))))

;; Slime
; FIXME: Figure out how to turn on slime-doc-mode
; cldoc causes problems:
;(add-to-list 'load-path "~/.emacs.d/cl") 

(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (require 'slime)
	    (cliki:start-slime)))

; Stop the extremely annoying lisp-mode from ever starting
; Possibly unneeded?
;; (setq auto-mode-alist
;;       (remove-if #'(lambda (x) (equal (cdr x) 'lisp-mode)) auto-mode-alist))
;; (setq auto-mode-alist
;;       (acons "\\.lisp\\'" 'lisp-mode auto-mode-alist))

;; ;FIXME: c-l-i-f doesn't seem to be defined
;; ;(setq (make-local-variable lisp-indent-function) 'common-lisp-indent-function))
;; (setq slime-complete-symbol-function 'slime-simple-complete-symbol) 
;; (setq slime-net-coding-system 'utf-8-unix)

; Haskell
; turn-on-haskell-indent is the bad indentation, turn-on-haskell-indentation is the good...
(add-to-list 'load-path "~/.emacs.d/haskell/haskell-mode-2.4/")
(add-to-list 'load-path "~/.emacs.d/haskell/")
(require 'haskell-mode)
(require 'haskell-indent)
(require 'inf-haskell)
(require 'hs-lint)
(add-hook 'haskell-mode-hook (lambda ()
                               (turn-on-haskell-indent)
			       (turn-on-haskell-doc-mode)
			       (local-set-key "\C-cl" 'hs-lint)))
; magic for making :cd work inside inferior-haskell
(setq inferior-haskell-find-project-root nil)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
;FIXME: haskell-font-lock-symbols doesn't seem to work.


;; 	  'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook (set-variable 'haskell-font-lock-symbols 'unicode))

; Prolog
(add-hook 'prolog-mode-hook
	  (lambda ()
	    (add-to-list 'load-path "~/.emacs.d/prolog")
	    (require 'prolog)
	    (setq prolog-system 'swi)
	    (local-set-key (kbd "C-c C-c") (lambda ()
					     (interactive)
					     (mark-whole-buffer)
					     (prolog-consult-region)))))

; Scheme
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (setq scheme-program-name "scheme48"
		  scheme-mit-dialect nil)
	    (add-to-list 'load-path "~/.emacs.d/scheme")
	    (require 'scheme-complete)
	    (local-set-key (kbd "TAB") 'scheme-complete-or-indent)
	    (local-set-key (kbd "C-c C-c") 'run-scheme)))
(add-hook 'inferior-scheme-mode-hook
	  (lambda ()
	    (local-set-key (kbd "TAB") 'scheme-complete-or-indent)))

;; Elisp
; These lines include a piece of cleverness:
; When you enter emacs-lisp-mode and the buffer *ielm* doesn't exist
; you are dropped into ielm.  This is designed to run ielm when emacs starts.
;; (add-hook 'emacs-lisp-mode-hook
;; 	  (progn (unless (get-buffer "*ielm")
;; 		   (ielm))
;; 		 (eldoc-mode t)))

(setq eldoc-idle-delay 0)

(defun if-not-already (mode-name)
; FIXME: Finish this.
  "Runs the mode if a buffer of '*mode*' does not already exist.
Used because it simplifies the code for ielm and eshell"
  (format "Unfinished" ))

(defun range (start end)
  (if (eq start end)
      (list end)
    (cons start (range (+ 1 start) end))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(cldoc-idle-delay 0)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(debug-on-error nil)
 '(default-input-method "rfc1345")
 '(frame-background-mode nil)
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-doc-show-global-types t)
 '(haskell-literate-default (quote bird))
 '(indent-tabs-mode t)
 '(ipython-command "ipython")
 '(prolog-program-name "/aber/clp8/prolog.sh")
 '(py-python-command "ipython")
 '(py-python-command-args (quote ("-i" "-colors" "Linux")))
 '(py-shell-switch-buffers-on-execute nil)
 '(py-smart-indentation t)
 '(python-python-command "python")
 '(remote-shell-program "ssh")
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(sql-database "mydb")
 '(sql-electric-stuff nil)
 '(sql-user "cal")
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)