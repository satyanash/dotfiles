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
  (require 'use-package))

;; Neutralize custom.el by pointing it to a file that we don't plan to load.
(setq custom-file "~/.emacs.d/customize-generated.el")

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

(use-package mode-line-bell
  :ensure t
  :config (mode-line-bell-mode))

(use-package evil
  :ensure t
  :init (setq evil-want-C-u-scroll t
	      dabbrev-case-fold-search nil
	      evil-want-keybinding nil
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

(use-package evil-collection
  :ensure t
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(use-package org
  :ensure t
  :init (setq org-hierarchical-todo-statistics nil
	      org-checkbox-hierarchical-statistics nil
	      org-src-tab-acts-natively t)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'ob-tangle t))

(use-package eww
  :ensure t
  :config (add-hook 'eww-mode-hook #'visual-line-mode))


(defun org-babel-load-init-file (org-file)
  "Given an org file, tangle all elisp code into a new file and then load it."
  (let* ((curr-dir (file-name-directory (or load-file-name buffer-file-name)))
	 (org-file-abs-path (concat curr-dir org-file))
	 (elisp-dir "~/.emacs.d/ob-init-elisp/")
	 (elisp-file (concat elisp-dir (file-name-base org-file) ".el")))
    (make-directory elisp-dir :parents)
    (org-babel-tangle-file org-file-abs-path elisp-file "emacs-lisp")
    (load-file elisp-file)))

(org-babel-load-init-file "babel.org")

(provide 'main)
