;;;; .emacs

;;; FIXES

;; ;; Fix the colours on sun machines
(set-foreground-color "white")
(set-background-color "black")

;; Set a sensible fill column
(setq-default fill-column 79)

;;; SUITABLE FOR ALL MODES

;; Zone when idle
;; (zone-when-idle 300)

;; Make tramp use URL syntax instead of tramp syntax
(setq tramp-syntax 'url)

;; Use chromium instead of ff
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Set the colors correctly
(setq default-frame-alist
      (append default-frame-alist
	      '((foreground-color . "white")
		(background-color . "Black")
		(cursor-color . "White"))))

;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/yas/")
(require 'yasnippet-bundle)
(yas/initialize)

;; Dired+
(add-to-list 'load-path "~/.emacs.d/diredplus/")
(load-file "~/.emacs.d/diredplus/dired+.el")

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
(if (window-system)
    (set-frame-size (selected-frame) 80 27))

;; Use the emacs server
(server-start)

;; Fill modes
(add-to-list 'load-path "~/.emacs.d/fill-column-indicator/")
(require 'fill-column-indicator)
(set-face-background 'fci-shading "Dark Gray")

;; Egg
(add-to-list 'load-path "~/.emacs.d/egg/")
(require 'egg)
(egg-minor-mode)

;;; SPECIAL MAJOR MODE HOOKS

;; Mediawiki
(add-to-list 'load-path "~/.emacs.d/mediawiki/")
(require 'mediawiki)

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

;; Javascript
(add-to-list 'load-path "~/.emacs.d/javascript/")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Haskell
; turn-on-haskell-indent is the bad indentation,
; turn-on-haskell-indentation is the good...
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

;; Crontab
(add-to-list 'load-path "~/.emacs.d/crontab/")
(require 'crontab-mode)

;; Markup
;; (load "~/.emacs.d/nxhtml/autostart.el")
;; (setq mumamo-background-colors nil) 
;; (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

;;; KEYBINDINGS

;; Keybindings for playing with windowing
(global-set-key [f11] 'toggle-fullscreen)

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
 '(fringe-mode 0 nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-doc-show-global-types t)
 '(haskell-literate-default (quote bird))
 '(indent-tabs-mode t)
 '(ipython-command "ipython")
 '(mediawiki-site-alist (quote (("Wikipedia" "http://en.wikipedia.org/w/" "CalPaterson" "plmoknijb" "Main Page") ("Rangespan" "http://wiki.rangespan.com/w/" "" "" "Main Page"))))
 '(mediawiki-site-default "Rangespan")
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
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode (quote (only . t)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)