;;; satyanash's .emacs
;;; 
;;; Code:

;; Package Management
(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  ;; bootstrap use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq package-selected-packages '(use-package vterm leetcode go-playground eyebrowse dockerfile-mode evil-smartparens ranger smartparens cider clojure-mode-extra-font-locking jsonnet-mode toml-mode package-lint helm evil-collection helm-projectile lsp-origami origami ag nyan-mode magit evil-tabs terraform-mode yaml-mode nov writeroom-mode json-mode markdown-mode golden-ratio mode-line-bell helm-ag projectile lsp-mode flycheck go-mode neotree evil)))

(progn ; visuals
  ;; Configure Default Face Attributes
  ;; Get "Monego" from https://github.com/cseelus/monego
  (set-face-attribute 'default nil :height 150 :family "Monego")
  (show-paren-mode t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (setq inhibit-startup-screen t
	initial-scratch-message ";;;(setq eval-expression-print-level 5\n;;;      eval-expression-print-length 200)\n\n\n"
	visible-bell t
	custom-safe-themes '("4e392ca6744909f952a9f479fca580f30424404d53d20c328ac4f391ae29e903" default))
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

;;; fix temp file behaviour
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(use-package nyan-mode
  :ensure t
  :init (setq nyan-animate-nyancat t
	  nyan-wavy-trail t)
  :config (nyan-mode))

(use-package projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t
	projectile-completion-system 'helm
	projectile-switch-project-action 'neotree-projectile-action
	projectile-project-search-path '("~/Code/"))
  :config (helm-projectile-on))

(use-package neotree
  :ensure t
  :init
  (setq neo-hidden-regexp-list '("^\\."
				 "\\.pyc$"
				 "~$"
				 "^#.*#$")))

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
  (setq command-error-function
	(lambda (data context caller)
	  "Ignore the various errors related to read-only text and motion; pass the rest to the default handler."
	  (let ((err (car data))
		(skip-errors '(text-read-only beginning-of-buffer end-of-buffer beginning-of-line end-of-line)))
	    (unless (member err skip-errors)
	      (command-error-default-function data context caller)))))
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

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-wrap-around t
	eyebrowse-new-workspace t)
  :config
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
  :config
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
		 ("C-o" nov-history-forward))))))

(use-package autoinsert
  :ensure t
  :hook (find-file . auto-insert)
  :init (setq auto-insert-alist ()
	      auto-insert-mode t
	      auto-insert-query nil
	      auto-insert t)
  :config
  (define-auto-insert
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
      "#+TAGS: ")))

(use-package writeroom-mode
  :ensure t
  :init (setq writeroom-fullscreen-effect 'maximized
	      writeroom-width 40)
  :config
  (add-hook 'writeroom-mode-hook #'visual-line-mode)
  (with-eval-after-load 'writeroom-mode
    (define-key writeroom-mode-map (kbd "C-M--") #'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-0") #'writeroom-adjust-width)))

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

(use-package eww
  :ensure t
  :config (add-hook 'eww-mode-hook #'visual-line-mode))

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

(use-package vterm
  :ensure t
  :config
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
  (defconst color-orange  "#ff9900")

  ;; Configure Face Attributes for vterm
  (set-face-attribute 'vterm-color-default nil :foreground color-orange  :background nil :inherit 'default)
  (set-face-attribute 'vterm-color-black   nil :foreground color-black   :background color-black)
  (set-face-attribute 'vterm-color-red     nil :foreground color-red     :background color-black)
  (set-face-attribute 'vterm-color-green   nil :foreground color-green   :background color-black)
  (set-face-attribute 'vterm-color-yellow  nil :foreground color-yellow  :background color-black)
  (set-face-attribute 'vterm-color-blue    nil :foreground color-blue    :background color-black)
  (set-face-attribute 'vterm-color-magenta nil :foreground color-magenta :background color-black)
  (set-face-attribute 'vterm-color-cyan    nil :foreground color-cyan    :background color-black)
  (set-face-attribute 'vterm-color-white   nil :foreground color-white   :background color-black))

(provide 'main)
