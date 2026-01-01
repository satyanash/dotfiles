;;; satyanash's .emacs
;;;
;;; Code:

;; Package Management with straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Setup use-package
(straight-use-package 'use-package)

;; Add the current directory to the theme load path.
(add-to-list 'custom-theme-load-path
	     (file-name-as-directory (file-name-directory load-file-name)))

;; Setup Org Mode, Babel and tangling
(use-package org
  :straight t
  :init (setq org-hierarchical-todo-statistics nil
	      org-checkbox-hierarchical-statistics nil
	      org-src-tab-acts-natively t
              org-startup-indented t)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'ob-tangle t))

(defadvice org-babel-tangle (around time-it activate compile)
  "Measure the time taken by org-babel to tangle files."
  (let ((tim (current-time)))
    ad-do-it
    (message "org-babel-tangle took %f sec" (float-time (time-subtract (current-time) tim)))))

(defun satyanash/load-org-babel-file (org-file)
  "Given an org file, tangle all elisp code into a new file and then load it."
  (let* ((curr-dir (file-name-directory (or load-file-name buffer-file-name)))
	 (org-file-abs-path (file-truename (concat curr-dir org-file)))
         (org-file-last-mod (nth 5 (file-attributes org-file-abs-path)))
	 (elisp-dir (file-truename "~/.emacs.d/ob-init-elisp/"))
	 (elisp-file (concat elisp-dir (file-name-base org-file) ".el"))
         (epoch-timestamp 0)
         (elisp-file-last-mod (or (nth 5 (file-attributes elisp-file))
                                  epoch-timestamp))
         (org-file-changed? (time-less-p elisp-file-last-mod org-file-last-mod)))
    (when org-file-changed?
      (message "Source %s is newer than Destination %s" org-file-abs-path elisp-file)
      (make-directory elisp-dir :parents)
      (org-babel-tangle-file org-file-abs-path elisp-file "emacs-lisp")
      ;;(byte-compile-file elisp-file)
      )
    (load (file-name-sans-extension elisp-file))))

(defmacro satyanash/time-it (label &rest body)
  "Prints time taken to execute body in secs"
  `(let ((--start (current-time))
         (--result (progn ,@body)))
     (message "%s: %fs" ,label (time-to-seconds (time-since --start)))
     --result))

(satyanash/time-it "babel.org"
                   (satyanash/load-org-babel-file "babel.org"))

(message "Emacs init time: %s" (emacs-init-time))
