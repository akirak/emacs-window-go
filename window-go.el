;;; window-go.el --- A collection of window operation utilities -*- lexical-binding: t -*-

(require 'dash)

(defun window-go-bottom ()
  "Select the bottom window in the current frame."
  (interactive)
  (select-window (-last
                  (lambda (obj)
                    (and (window-valid-p obj)
                         (window-live-p obj)
                         (not (window-minibuffer-p obj))))
                  (-flatten (window-tree)))))

(defun window-go-first-file-window ()
  "Select the first window in the current frame displaying a file buffer."
  (interactive)
  (let (window match)
    (setq window (frame-first-window))
    (while (not (setq match (buffer-file-name (window-buffer window))))
      (setq window (next-window window)))
    (when match
      (select-window window))))

(defun window-go-previous ()
  "Select the previous window in the frame."
  (interactive)
  (other-window -1))

(defun window-go-master ()
  "Select the first window in the frame."
  (interactive)
  (select-window (frame-first-window)))

(defun window-go-swap (w1 w2)
  "Swap two windows W1 and W2."
  (unless (and (windowp w1) (windowp w2))
    (error "not a window"))
  (when (equal w1 w2)
    (error "same window"))
  (let ((b1 (window-buffer w1))
        (b2 (window-buffer w2)))
    (with-temp-buffer
      (set-window-buffer w2 (current-buffer))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1))))

(defun window-go-swap-master ()
  "Swap the current window with the master window and then select the master."
  (interactive)
  (let ((master (frame-first-window))
        (target (selected-window)))
    (unless (equal master target)
      (window-go-swap master target)
      (window-go-master))))

(defcustom window-go-h-resize-ratio 0.08
  "Ratio for horizontally resizing."
  :group 'window-go)

(defcustom window-go-resize-ratio 0.08
  "Ratio for vertical resizing."
  :group 'window-go)

(defun window-go-shrink-horizontally ()
  "Shrink the pane horizontally."
  (interactive)
  (let ((pixels (floor (* (frame-width) window-go-h-resize-ratio))))
    (if (= (nth 0 (window-edges)) 0)
        (exwm-layout-shrink-window-horizontally pixels)
      (exwm-layout-enlarge-window-horizontally pixels))))

(defun window-go-grow-horizontally ()
  "Grow the pane horizontally."
  (interactive)
  (let ((pixels (floor (* (frame-width) window-go-h-resize-ratio))))
    (if (= (nth 0 (window-edges)) 0)
        (exwm-layout-enlarge-window-horizontally pixels)
      (exwm-layout-shrink-window-horizontally pixels))))

(defun window-go-grow ()
  "Grow the pane vertically."
  (interactive)
  (let ((pixels (floor (* (frame-width) window-go-resize-ratio))))
    (if (= (nth 1 (window-edges)) 0)
        (exwm-layout-enlarge-window pixels)
      (exwm-layout-shrink-window pixels))))

(defun window-go-shrink ()
  "Shrink the pane vertically."
  (interactive)
  (let ((pixels (floor (* (frame-width) window-go-resize-ratio))))
    (if (= (nth 1 (window-edges)) 0)
        (exwm-layout-shrink-window pixels)
      (exwm-layout-enlarge-window pixels))))

(defun window-go-split-sensibly (&optional arg)
  "Split the window sensibly.

With a universal prefix argument (C-u), switch to the selected window.

With two universal prefix arguments (C-u C-u), switch to the selected window
and display another buffer."
  (interactive "P")
  (let ((direction (if (> (window-body-width) 160)
                       'right
                     'below)))
    (pcase direction
      ('right (split-window-right))
      ('below (split-window-below)))
    (when arg
      (other-window 1)
      (when (equal arg '(16))
        (switch-to-buffer (other-buffer))))))

(defun window-go-term-in-split-window ()
  "Split the window sensibly and open a dedicated multi-term."
  (interactive)
  (window-go-split-sensibly)
  (multi-term)
  (set-window-dedicated-p (selected-window) t))

(provide 'window-go)
;;; window-go.el ends here
