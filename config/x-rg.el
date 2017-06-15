;;; x-rg.el --- Configuration for rg and related utils.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package wgrep
  :defer t
  :preface
  (progn
    (autoload 'wgrep-finish-edit "wgrep")

    (defun x-rg-wgrep-finish-edit-kill-buffer ()
      "Finish the current wgrep edit and kill the wgrep buffer."
      (interactive)
      (let ((buf (current-buffer)))
        (prog1 (wgrep-finish-edit)
          (kill-buffer buf)))))

  :config
  (progn
    (setq wgrep-auto-save-buffer t)
    (define-key wgrep-mode-map [remap wgrep-finish-edit] #'x-rg-wgrep-finish-edit-kill-buffer)))

(provide 'x-rg)

;;; x-rg.el ends here
