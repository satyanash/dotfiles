(defun satyanash--test-file-open-timing (file-path)
  (let ((before-file-load (current-time)))
    (profiler-start 'cpu+mem)
    (find-file file-path)
    (profiler-stop)
    (message "%s took %f seconds to open."
             file-path
             (time-to-seconds (time-since before-file-load)))
    (kill-buffer)
    (profiler-report)))

(setq inhibit-compacting-font-caches t)

(satyanash--test-file-open-timing "./babel.org")

(satyanash--test-file-open-timing "./main.el")
