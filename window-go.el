;;; window-go.el --- A collection of window operation utilities -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.12"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides miscellaneous functions for window manipulation.
;; It tries to emulate behaviors that are somewhat similar to those of XMonad,
;; but it does not aim at perfection.

;;; Code:

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
    (error "Not a window"))
  (when (equal w1 w2)
    (error "Same window"))
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

(defun window-go-split-sensibly (&optional arg)
  "Split the window sensibly.

With a universal prefix argument ARG, switch to the selected window.

With two universal prefix arguments, switch to the selected window
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
