;;; satyanash's .emacs
;;; 
;;; Code:

;; Package Management
(progn
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (package-initialize))

(require 'use-package)

(progn
  ;; Pulled from iTerm2 ANSI color scheme
  (defconst color-black   "#000000")
  (defconst color-red     "#c91b00")
  (defconst color-green   "#00c200")
  (defconst color-yellow  "#c7c400")
  (defconst color-blue    "#0082ff")
  (defconst color-magenta "#c930c7")
  (defconst color-cyan    "#00c5c7")
  (defconst color-white   "#c7c7c7")

  ;; Custom Colors
  (defconst color-orange  "#ff9900"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4e392ca6744909f952a9f479fca580f30424404d53d20c328ac4f391ae29e903" default)))
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(line-number-mode t)
 '(column-number-mode t)
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (use-package vterm leetcode go-playground eyebrowse dockerfile-mode evil-smartparens ranger smartparens cider clojure-mode-extra-font-locking jsonnet-mode toml-mode package-lint helm evil-collection helm-projectile lsp-origami origami ag nyan-mode magit evil-tabs terraform-mode yaml-mode nov writeroom-mode json-mode markdown-mode golden-ratio mode-line-bell helm-ag projectile lsp-mode flycheck go-mode neotree evil)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(writeroom-fullscreen-effect (quote maximized))
 '(writeroom-width 40))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; Get "Monego" from https://github.com/cseelus/monego
 '(default ((t (:height 150 :family "Monego"))))

 ;; vterm colors
 `(vterm-color-default ((t (:foreground ,color-orange  :background nil :inherit default))))
 `(vterm-color-black   ((t (:foreground ,color-black   :background ,color-black))))
 `(vterm-color-red     ((t (:foreground ,color-red     :background ,color-black))))
 `(vterm-color-green   ((t (:foreground ,color-green   :background ,color-black))))
 `(vterm-color-yellow  ((t (:foreground ,color-yellow  :background ,color-black))))
 `(vterm-color-blue    ((t (:foreground ,color-blue    :background ,color-black))))
 `(vterm-color-magenta ((t (:foreground ,color-magenta :background ,color-black))))
 `(vterm-color-cyan    ((t (:foreground ,color-cyan    :background ,color-black))))
 `(vterm-color-white   ((t (:foreground ,color-white   :background ,color-black)))))

(progn ; visuals
  (show-paren-mode 1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq inhibit-startup-screen t
	initial-scratch-message ";;;(setq eval-expression-print-level 5\n;;;      eval-expression-print-length 200)\n\n\n"
	visible-bell t)
  (progn
    (setq nyan-animate-nyancat t
	  nyan-wavy-trail t)
    (nyan-mode))
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name)))
  (load-theme 'vivid-chalk); get themes with (custom-available-themes)
  (mode-line-bell-mode)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)); get that fancy dark transparent title bar
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      ))

(setq command-error-function
  (lambda (data context caller)
    "Ignore the various errors related to read-only text and motion; pass the rest to the default handler."
    (let ((err (car data))
	  (skip-errors '(text-read-only beginning-of-buffer end-of-buffer beginning-of-line end-of-line)))
	(unless (member err skip-errors)
	  (command-error-default-function data context caller)))))

;;; fix temp file behaviour
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(progn ; projectile, neotree
  (setq
   helm-projectile-fuzzy-match t
   projectile-completion-system 'helm
   projectile-switch-project-action 'neotree-projectile-action
   projectile-project-search-path '("~/Code/")
   neo-hidden-regexp-list '("^\\."
			    "\\.pyc$"
			    "~$"
			    "^#.*#$"))
  (helm-projectile-on))

(use-package evil
  :ensure t
  :init (setq evil-want-C-u-scroll t
	      dabbrev-case-fold-search nil
	      evil-want-keybinding nil
	      evil-collection-setup-minibuffer t
	      evil-vsplit-window-right t
	      evil-split-window-below t
	      evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (evil-collection-init)
  (seq-do
   (lambda (tup) (define-key evil-normal-state-map (kbd (car tup)) (nth 1 tup)))
   '(("C-w C-l" evil-window-right)
     ("C-w C-h" evil-window-left)
     ("C-w C-j" evil-window-down)
     ("C-w C-k" evil-window-up)
     ("C-k" previous-line)
     ("C-j" next-line)
     ("C-c C-j" eval-print-last-sexp)
     ("C-p" helm-projectile-find-file)
     ))
  (add-hook 'neotree-mode-hook
	    (lambda ()
	      (seq-do
	       (lambda (tup) (define-key evil-normal-state-local-map (kbd (car tup)) (nth 1 tup)))
	       '(("TAB" neotree-enter)
		 ("SPC" neotree-quick-look)
		 ("q" neotree-hide)
		 ("RET" neotree-enter)
		 ("r" neotree-refresh)
		 ("n" neotree-next-line)
		 ("p" neotree-previous-line)
		 ("A" neotree-stretch-toggle)
		 ("C" neotree-change-root)
		 ("H" neotree-hidden-file-toggle)))))
  (seq-do
   (lambda (tup) (define-key evil-insert-state-map (kbd (car tup)) (nth 1 tup)))
   '(
     ("C-n" evil-complete-next)
     ("C-w" evil-delete-backward-word)
     ("C-h" evil-delete-backward-char)
     ("C-p" evil-complete-previous)
					;("C-c" evil-normal-state)
     ))
  (with-eval-after-load 'helm
    (seq-do
     (lambda (tup)
       (define-key helm-map (kbd (car tup)) (nth 1 tup)))
     '(("C-h" evil-delete-backward-char)
       ("C-w" evil-delete-backward-word)))))
 ; (seq-do
					;  (lambda (tup) (define-key evil-command-window-mode-map (kbd (car tup)) (nth 1 tup)))
 ;  '(
 ;    ("C-j" evil-command-window-execute)
 ;    ("C-p" evil-complete-previous)
 ;    ))
  ;(define-key evil-command-window-mode-map (kbd "C-j") 'evil-command-window-execute)
  ;(define-key evil-command-window-mode-map (kbd "C-p") 'evil-command-window-execute))

(use-package golden-ratio
  :ensure t
  :init (setq golden-ratio-extra-commands ; evil mode fix
	      '(evil-window-next
		evil-window-prev
		evil-window-right
		evil-window-left
		evil-window-down
		evil-window-up)
	      golden-ratio-auto-scale t
	      window-combination-resize t)
  :config
  (golden-ratio-mode 1)
  (defadvice align-regexp (around align-regexp-with-spaces)
    "Never use tabs for alignment."
    (let ((indent-tabs-mode nil))
      ad-do-it))
  (ad-activate 'align-regexp))

(progn ; configure eyebrowse
  (setq eyebrowse-wrap-around t
	eyebrowse-new-workspace t)
  (require 'eyebrowse)
  (eyebrowse-mode t)
  (eyebrowse-setup-evil-keys))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init (setq gofmt-command "goimports")
  :config (add-hook 'before-save-hook 'gofmt-before-save))

(use-package lsp-mode
  :ensure t
  :hook ((ruby-mode . lsp)
         (go-mode . lsp-deferred)
         (terraform-mode . lsp)))

(use-package ido
  :ensure t
  :init (setq ido-enable-flex-matching t)
  :config (ido-mode t))

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "kramdown"
	      markdown-enable-math t))

(use-package nov
  :ensure t
  :commands (nov-mode)
  :init (setq nov-text-width 120
	      visual-fill-column-center-text t)
  :mode "\\.epub\\'"
  :config (progn ; epub reader
	    (add-hook 'nov-mode-hook 'visual-line-mode)
	    (add-hook 'nov-mode-hook 'visual-fill-column-mode)
	    (add-hook 'nov-mode-hook
		      (lambda ()
			(face-remap-add-relative 'variable-pitch
						 :family "Georgia"
						 :height 400)))
	    (add-hook 'nov-mode-hook
		      (lambda ()
			(seq-do
			 (lambda (tup) (define-key evil-normal-state-local-map (kbd (car tup)) (nth 1 tup)))
			 '(("C-i" nov-history-back)
			   ("t" nov-goto-toc)
			   ("l" evil-forward-char)
			   ("n" nov-next-document)
			   ("p" nov-previous-document)
			   ("C-o" nov-history-forward)))))))

(use-package autoinsert
  :ensure t
  :hook (find-file . auto-insert)
  :init (setq auto-insert-alist ()
	      auto-insert-mode t
	      auto-insert-query nil
	      auto-insert t)
  :config (progn (define-auto-insert
		   '(markdown-mode . "slip box skeleton")
		   '("Markdown Slip Box skeleton:"
		     "---" \n
		     "date: " (format-time-string "%Y-%m-%dT%T%z") \n
		     "type: fleeting" \n
		     "tags: " _ \n
		     "---" \n))
		 (define-auto-insert
		   '(org-mode . "slip box skeleton")
		   '("Org Slip Box skeleton:"
		     "#+TITLE: " _ \n
		     "#+DATE: " (format-time-string "%Y-%m-%dT%T%z") \n
		     "#+TAGS: "))))

(use-package writeroom-mode
  :ensure t
  :config (progn
	    (add-hook 'writeroom-mode-hook #'visual-line-mode)
	    (with-eval-after-load 'writeroom-mode
	      (define-key writeroom-mode-map (kbd "C-M--") #'writeroom-decrease-width)
	      (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-increase-width)
	      (define-key writeroom-mode-map (kbd "C-M-0") #'writeroom-adjust-width))))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

(use-package org
  :ensure t
  :init (setq org-hierarchical-todo-statistics nil
	      org-checkbox-hierarchical-statistics nil)
  :config (add-hook 'org-mode-hook #'visual-line-mode))

(progn ; configure eww
  (add-hook 'eww-mode-hook #'visual-line-mode))

(use-package smartparens
  :ensure t
  :hook (clojure-mode . smartparens-strict-mode))

(use-package evil-smartparens
  :ensure t
  :hook (clojure-mode . evil-smartparens-mode))

(use-package cider
  :ensure t
  :hook (clojure-mode . cider-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package leetcode
  :init (setq leetcode-prefer-language "golang")
  :ensure t)

(provide 'main)
