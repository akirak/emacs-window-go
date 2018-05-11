;;; exwm-window-go.el --- window-go with exwm dependencies -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") exwm)

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

(defcustom exwm-window-go-h-resize-ratio 0.08
  "Ratio for horizontally resizing."
  :group 'window-go)

(defcustom exwm-window-go-resize-ratio 0.08
  "Ratio for vertical resizing."
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

(provide 'exwm-window-go)
;;; exwm-window-go.el ends here
