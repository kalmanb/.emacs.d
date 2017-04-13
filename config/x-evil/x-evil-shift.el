;;; x-evil-shift.el --- Evil shift commands which keep region active.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'evil)

(defun x-evil-shift-left (&optional beg end)
  "Shift left, keeping the region active.

BEG and END are the bounds of the active region."
  (interactive "r")
  (evil-shift-left beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun x-evil-shift-right (&optional beg end)
  "Shift right, keeping the region active.

BEG and END are the bounds of the active region."
  (interactive "r")
  (evil-shift-right beg end)
  (evil-normal-state)
  (evil-visual-restore))

(provide 'x-evil-shift)

;;; x-evil-shift.el ends here
