;;; exwm-window-go.el --- window-go with exwm dependencies -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (exwm "0.18") (window-go "0.1.0"))

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

;; FIXME

;;; Code:

(require 'exwm)
(require 'window-go)
(require 'cl-lib)
(require 'seq)

(defcustom exwm-window-go-h-resize-ratio 0.08
  "Ratio for horizontally resizing."
  :type 'float
  :group 'window-go)

(defcustom exwm-window-go-resize-ratio 0.08
  "Ratio for vertical resizing."
  :type 'float
  :group 'window-go)

(defcustom exwm-window-go-cycle-visible-workspaces t
  "Cycle through visible workspaces."
  :type 'boolean
  :group 'window-go)

(defun exwm-window-go-shrink-horizontally ()
  "Shrink the pane horizontally."
  (interactive)
  (let ((pixels (floor (* (frame-width) exwm-window-go-h-resize-ratio))))
    (if (= (nth 0 (window-edges)) 0)
        (exwm-layout-shrink-window-horizontally pixels)
      (exwm-layout-enlarge-window-horizontally pixels))))

(defun exwm-window-go-grow-horizontally ()
  "Grow the pane horizontally."
  (interactive)
  (let ((pixels (floor (* (frame-width) exwm-window-go-h-resize-ratio))))
    (if (= (nth 0 (window-edges)) 0)
        (exwm-layout-enlarge-window-horizontally pixels)
      (exwm-layout-shrink-window-horizontally pixels))))

(defun exwm-window-go-grow ()
  "Grow the pane vertically."
  (interactive)
  (let ((pixels (floor (* (frame-width) exwm-window-go-resize-ratio))))
    (if (= (nth 1 (window-edges)) 0)
        (exwm-layout-enlarge-window pixels)
      (exwm-layout-shrink-window pixels))))

(defun exwm-window-go-shrink ()
  "Shrink the pane vertically."
  (interactive)
  (let ((pixels (floor (* (frame-width) exwm-window-go-resize-ratio))))
    (if (= (nth 1 (window-edges)) 0)
        (exwm-layout-shrink-window pixels)
      (exwm-layout-enlarge-window pixels))))

(defun exwm-window-go--visible-workspaces ()
  "Return a list of visible workspaces, sorted by position."
  (cl-sort (cl-loop for frame in exwm-workspace--list
                    when (exwm-workspace--active-p frame)
                    collect frame)
           'window-go--compare-frame-positions))

(defun exwm-window-go-next-visible-workspace (&optional arg)
  "Switch to the next visible workspace."
  (interactive "P")
  (when-let
      ((items (exwm-window-go--visible-workspaces))
       (pos (seq-position items (selected-frame)))
       (new-pos (+ pos (if (numberp arg) arg 1)))
       (j (cond
           ((< new-pos 0) (when exwm-window-go-cycle-visible-workspaces
                            (+ new-pos (length items))))
           ((< new-pos (length items)) new-pos)
           (t (when exwm-window-go-cycle-visible-workspaces
                (- new-pos (length items)))))))
    (select-frame (seq-elt items j))))

(defun exwm-window-go-previous-visible-workspace (&optional arg)
  "Switch to the previous visible workspace."
  (interactive "P")
  (exwm-window-go-next-visible-workspace (if (numberp arg)
                                             (- arg)
                                           -1)))

(defun exwm-window-go--hidden-workspaces ()
  "Return a list of hidden workspaces."
  ;; TODO: Think about how to order the workspace list
  (let* ((all-workspaces exwm-workspace--list)
         (pos (seq-position all-workspaces (selected-frame))))
    (cl-remove-if #'exwm-workspace--active-p
                  (append (seq-subseq all-workspaces (1+ pos))
                          (seq-subseq all-workspaces 0 pos)))))

(defun exwm-window-go-next-hidden-workspace (&optional arg)
  "Switch to the next hidden workspace in the workspace list."
  (interactive "P")
  (let* ((ws (exwm-window-go--hidden-workspaces))
         (pos (if (numberp arg) (1- arg) 0))
         (frm (elt ws (cond
                       ((< pos 0) (+ pos (length ws)))
                       ((< pos (length ws)) pos)
                       (t (- pos (length ws)))))))
    (when frm (select-frame frm))))

(defun exwm-window-go-previous-hidden-workspace (&optional arg)
  "Switch to the previous hidden workspace in the workspace list."
  (interactive "P")
  (exwm-window-go-next-hidden-workspace (if (numberp arg) (- arg) 0)))

(provide 'exwm-window-go)
;;; exwm-window-go.el ends here
