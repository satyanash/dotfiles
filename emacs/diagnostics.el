(defun test-file-open-timing (file-path)
  (let ((before-file-load (current-time)))
    (find-file file-path)
    (message "%s took %f seconds to open."
             file-path
             (time-to-seconds (time-since before-file-load)))
    (kill-buffer)))

(setq inhibit-compacting-font-caches t)

(test-file-open-timing "./babel.org")

(test-file-open-timing "./main.el")
