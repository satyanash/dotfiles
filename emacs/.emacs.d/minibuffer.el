
(make-frame '((width . 80)
              (height . 1)
              (fullscreen . nil)
              (minibuffer . only)
              (auto-raise . t)
              (auto-lower . t)
              (no-focus-on-map . t)
              (skip-taskbar . t)
              (undecorated . t)
              (left . 800)
              (top . 500)))

(make-frame '((minibuffer . nil)
              (width . 80)
              (height . 24)
              (fullscreen . nil)
              (left . 400)
              (top . 200)))

(frame-list)

(setq default-minibuffer-frame (car (frame-list)))

default-minibuffer-frame

(setq minibuffer-exit-hook '(lambda ()
                              (lower-frame default-minibuffer-frame)
                              (make-frame-invisible default-minibuffer-frame)))

(delete-frame (car (frame-list)))
