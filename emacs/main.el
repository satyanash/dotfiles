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

;;; fix temp file behaviour
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(use-package org
  :ensure t
  :init (setq org-hierarchical-todo-statistics nil
	      org-checkbox-hierarchical-statistics nil
	      org-src-tab-acts-natively t)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'ob-tangle t))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(defadvice org-babel-tangle (around time-it activate compile)
  "Display the time taken by org-babel to tangle files."
  (let ((tim (current-time)))
    ad-do-it
    (message "org-babel-tangle took %f sec" (float-time (time-subtract (current-time) tim)))))

;; Add the current directory to the theme load path.
(add-to-list 'custom-theme-load-path
	     (file-name-as-directory (file-name-directory load-file-name)))

(defun satyanash--load-org-babel-file (org-file)
  "Given an org file, tangle all elisp code into a new file and then load it."
  (let* ((curr-dir (file-name-directory (or load-file-name buffer-file-name)))
	 (org-file-abs-path (concat curr-dir org-file))
         (org-file-last-mod (nth 5 (file-attributes org-file-abs-path)))
	 (elisp-dir "~/.emacs.d/ob-init-elisp/")
	 (elisp-file (concat elisp-dir (file-name-base org-file) ".el"))
         (elisp-file-last-mod (or (nth 5 (file-attributes elisp-file))
                                  0)) ;;default to epoch if the file does not exist
         (org-file-changed? (time-less-p elisp-file-last-mod org-file-last-mod)))
    (when org-file-changed?
      (make-directory elisp-dir :parents)
      (org-babel-tangle-file org-file-abs-path elisp-file "emacs-lisp")
      (byte-compile-file elisp-file))
    (load (file-name-sans-extension elisp-file))))

(satyanash--load-org-babel-file "babel.org")

(provide 'main)
