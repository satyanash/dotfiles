;;; satyanash's .emacs
;;; 
;;; TODOS (shit to fix, make emacs behave like vim)
;;; * fix projectile switch project
;;; * fix evil jump list to have files opened through neotree
;;; * fix :q, :w, :wq and :wa behaviour for evil-tabs
;;; * fix neotree behaviour with evil-tabs
;;; * fix "Back to top level" message
;;; * fix C-w, C-h, C-j and other shortcuts in the minibuffer
;;; * fix neotree to be able to open per-frame
;;; * intelligent frame sizing when 
;;; * commands getting cancelled when your error handler is triggered in the minibuffer
;;; * debug behaviour where clipboard is overwritten as soon as emacs gets focus
;;; * fix vim style i based 'inside' edits, to use the current cursor position, not some last one.
;;; * fix vim style i based edit, where error handler messes up and all you see is the message "Back to top level".
;;; * cursor should be placed at the end of the line when scrolling through history
;;; * fix `w`, `gg`, `G` in dired mode
;;; * fix issue when emacsclient prompts in an existing frame instead of creating a new frame and then prompting.
;;;    This appears as though nothing has happened, but a frame in the background is requesting input.
;;;    It's then annoying to have to remember that something is happening in the background and manually switch focus to it.
;;; * :q, :wq and friends should kill the buffer (emacs behaves as if :set hidden)
;;; * % should jump for do..end blocks and quotes
;;; * fix placement of swap/backup files in a central directory, instead of cluttering the git directory.
;;; * fix word_boundary to ignore underscores. (maybe use the capital letters? That's the write way anyway, especially in lisp)
;;; * make emacs disappear from the Alt-Tab menu if there are no active frames.
;;; * cannot pres up/down when searching with /
;;; 

(provide 'main)

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(line-number-mode t)
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (evil-collection helm-projectile lsp-origami origami ag nyan-mode magit evil-tabs terraform-mode yaml-mode nov writeroom-mode json-mode markdown-mode golden-ratio mode-line-bell helm-ag projectile lsp-mode flycheck go-mode neotree evil))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 150 :family "Monaco")))))

(progn
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (package-initialize))

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
  (load-theme 'vivid-chalk t); get themes with (custom-available-themes)
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

(progn ; projectile, neotree
  (setq
   projectile-completion-system 'helm
   projectile-switch-project-action 'neotree-projectile-action
   projectile-project-search-path '("~/Code/")
   neo-hidden-regexp-list '("^\\."
			    "\\.pyc$"
			    "~$"
			    "^#.*#$"))
  (helm-projectile-on))

(progn
  (setq evil-want-C-u-scroll t
	dabbrev-case-fold-search nil
	evil-want-keybinding nil
	evil-collection-setup-minibuffer t)
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
     ("C-p" projectile-find-file)
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
       ("C-w" evil-delete-backward-word))))
  )
 ; (seq-do
					;  (lambda (tup) (define-key evil-command-window-mode-map (kbd (car tup)) (nth 1 tup)))
 ;  '(
 ;    ("C-j" evil-command-window-execute)
 ;    ("C-p" evil-complete-previous)
 ;    ))
  ;(define-key evil-command-window-mode-map (kbd "C-j") 'evil-command-window-execute)
  ;(define-key evil-command-window-mode-map (kbd "C-p") 'evil-command-window-execute))

(progn
  (require 'golden-ratio)
  (setq golden-ratio-extra-commands ; evil mode fix
	'(evil-window-next
	  evil-window-prev
	  evil-window-right
	  evil-window-left
	  evil-window-down
	  evil-window-up))
  (golden-ratio-mode 1))

(progn
    (require 'lsp-mode)
    (add-hook 'ruby-mode-hook #'lsp)
    (add-hook 'terraform-mode-hook #'lsp)
    (add-hook 'go-mode-hook #'lsp))

(progn
    (require 'ido)
    (setq ido-enable-flex-matching t)
    (ido-mode t))

(progn ; markdown-mode settings
  (setq markdown-command "kramdown"
	markdown-enable-math t))

(progn ; epub reader
  (setq nov-text-width 120
	visual-fill-column-center-text t)
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
		 ("C-o" nov-history-forward)))))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(progn
  (require 'autoinsert)
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert-alist ()
	auto-insert-mode t
	auto-insert-query nil
	auto-insert t)
  (define-auto-insert
    '(markdown-mode . "slip box skeleton")
    '("Slip Box skeleton:"
      "---" \n
      "date: " (format-time-string "%Y-%m-%dT%T%z") \n
      "type: fleeting" \n
      "tags: " _ \n
      "---" \n)))

(progn
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(progn
  (require 'terraform-mode)
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . yaml-mode)))