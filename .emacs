;;;; .emacs

;;; FIXES

;; ;; Fix the colours on sun machines
;; (set-foreground-color "white")
;; (set-background-color "black")

;;; SUITABLE FOR ALL MODES

;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/yas/")
(require 'yasnippet-bundle)
(yas/initialize)

;; Line numbering
(require 'linum)
(global-linum-mode)

;; Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

;; Add auto-completion
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete)
(global-auto-complete-mode t)

;; Always do syntax highlighting
(global-font-lock-mode 1)

;; Match parenthesis
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; Try to fix the scrolling behaviour
(setq scroll-conservatively 1) ; scroll line by line with no jumps

;; Turn on column numbers in the mode line
(column-number-mode t)

;; Turn on ido-mode
(ido-mode t)

;; Hide the tool bar and the scroll bar
(tool-bar-mode -1) 
(scroll-bar-mode -1)

;; Window size
(if (window-system) (set-frame-size (selected-frame) 132 33))

;; Use the emacs server
(server-start)

;; Make C-w delete only string of alphanumeric characters
; FIXME

;; Colour Themes
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/color-theme.el")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;; Fill modes
; FIXME

;; Egg
(add-to-list 'load-path "~/.emacs.d/egg/")
(require 'egg)

;;; SPECIAL MAJOR MODE HOOKS

;; Latex
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

;; Python
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/pymacs")
(require 'pymacs)
(require 'init-python)

;; Haskell
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

;;; KEYBINDINGS

;; Keybindings for playing with windowing
(global-set-key (kbd "C-x -") #'shrink-window)
(global-set-key (kbd "C-x C-_") #'shrink-window-horizontally)
(global-set-key (kbd "C-x +") #'enlarge-window)
(global-set-key (kbd "C-x C-+") #'enlarge-window-horizontally)
(global-set-key (kbd "C-c =") #'balance-windows)
;; Buffers
; Try and be like GNU Screen
(global-set-key (kbd "C-x C-n") 'next-buffer)
;; (global-set-key (kbd "C-x n") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)
;; (global-set-key (kbd "C-x p") 'previous-buffer)
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
(global-set-key [f11] 'toggle-fullscreen)
(define-key minibuffer-local-map (kbd "M-/") 'dabbrev-expand)
; Pasting shortcuts
(global-set-key (kbd "<C-mouse-2>") #'kill-region)
(global-set-key (kbd "<C-mouse-3>") #'yank)


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