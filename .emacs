;;;; .emacs

;;; FIXES

;; Revert all buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Reverted open files."))

;; ;; Fix the colours on sun machines
(set-foreground-color "white")
(set-background-color "black")

;; Get rid of the splash screen
(setq inhibit-splash-screen t)

;; Set a sensible fill column
(setq-default fill-column 79)

;;; SUITABLE FOR ALL MODES

;; Compile
(global-set-key [f5] 'compile)

;; Zone when idle
;; (zone-when-idle 300)

;; Set small enough font-lock-mode
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono 8")

;; Nuclear whitespace mode
(add-hook 'write-file-hooks (lambda () (if (not indent-tabs-mode)
          (save-excursion (untabify (point-min) (point-max))
          (delete-trailing-whitespace)))))

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

;; Pivotal
(add-to-list 'load-path "~/.emacs.d/pivotal/")
(require 'pivotal-tracker)

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
;; (ido-mode t)

;; Turn on icicles mode
(add-to-list 'load-path "~/.emacs.d/icicles/")
(require 'icicles)
(icicle-mode)

;; Hide the tool bar and the scroll bar FIXME: Disabled because of unity bug
;;(tool-bar-mode -1) 
;;(scroll-bar-mode -1)

;; Window size
;; FIXME
;;(if (window-system)
;;    (set-frame-size (selected-frame) 143 67))

;; Use the emacs server
;; (server-start)

;; Fill modes
(add-to-list 'load-path "~/.emacs.d/fill-column-indicator/")
(require 'fill-column-indicator)
(set-face-background 'fci-shading "Dark Gray")

;; Egg
(add-to-list 'load-path "~/.emacs.d/egg/")
(require 'egg)
(egg-minor-mode)

;;; SPECIAL MAJOR MODE HOOKS

(add-to-list 'load-path "~/.emacs.d/yaml-mode/")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Nginx
(add-to-list 'load-path "~/.emacs.d/nginx")
(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("[.]nginx$" . nginx-mode))

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
(add-to-list 'load-path "~/.emacs.d/python-mode")
(require 'python-mode)
(require 'auto-complete-config)
(ac-config-default)
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<left>") 'backward-word)
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (pymacs-load "bikeemacs" "brm-")
;; (brm-init)

;; Javascript
;; (add-to-list 'load-path "~/.emacs.d/javascript/")
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Scala
(add-to-list 'load-path "~/.emacs.d/scala-emacs")
(require 'scala-mode-auto)

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (scala-mode-feature-electric-mode)))

(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'load-path "/home/cal/.emacs.d/ensime_2.9.2-0.9.8.1/elisp")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(push "/home/cal/bin/scala-2.9.2/bin" exec-path)
(push "/home/cal/bin/sbt/bin" exec-path)

;; Haskell
; turn-on-haskell-indent is the bad indentation,
; turn-on-haskell-indentation is the good...
(load "~/.emacs.d/haskell/haskell-mode-2.8.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; (add-to-list 'load-path "~/.emacs.d/haskell/haskell-mode-2.4/")
;; (add-to-list 'load-path "~/.emacs.d/haskell/")
;; (require 'haskell-mode)
;; (require 'haskell-indent)
;; (require 'inf-haskell)
;; (require 'hs-lint)
;; (add-hook 'haskell-mode-hook (lambda ()
;;                                (turn-on-haskell-indent)
;; 			       (turn-on-haskell-doc-mode)
;; 			       (local-set-key "\C-cl" 'hs-lint)))
;; ; magic for making :cd work inside inferior-haskell
(setq inferior-haskell-find-project-root nil)
;; (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

;; Cucumber
(add-to-list 'load-path "~/.emacs.d/cucumber")
;; ;; optional configurations
;; ;; default language if .feature doesn't have "# language: fi"
;; ;(setq feature-default-language "fi")
;; ;; point to cucumber languages.yml or gherkin i18n.yml to use
;; ;; exactly the same localization your cucumber uses
;; ;(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
;; ;; and load feature-mode
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Crontab
(add-to-list 'load-path "~/.emacs.d/crontab/")
(require 'crontab-mode)

;; Markup
;; (load "~/.emacs.d/nxhtml/autostart.el")

;;; KEYBINDINGS

;; Keybindings for playing with windowing
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key (kbd "C-x \"") 'list-buffers)

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
 '(flymake-compilation-prevents-syntax-check t)
 '(flymake-log-level 2)
 '(frame-background-mode nil)
 '(fringe-mode 0 nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-doc-show-global-types t)
 '(haskell-literate-default (quote bird))
 '(icicle-buffers-ido-like-flag t)
 '(indent-tabs-mode t)
 '(prolog-program-name "/aber/clp8/prolog.sh")
 '(py-shell-switch-buffers-on-execute nil)
 '(py-smart-indentation t)
 '(python-shell-virtualenv-path "/home/cal/src/recall/")
 '(remote-shell-program "ssh")
 '(ropemacs-completing-read-function (quote completing-read))
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(save-place t nil (saveplace))
 '(scala-mode-feature:electric-expand-delimiters-list nil)
 '(scala-mode-feature:electric-on-per-default nil)
 '(scroll-bar-mode nil)
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
(put 'downcase-region 'disabled nil)
