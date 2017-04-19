;;; x-faces.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

;; Load themes.

(defvar x-faces-dark-mode-p nil)

(defun x-faces/toggle-dark-mode ()
  "Toggle between light and dark mode."
  (interactive)
  (if x-faces-dark-mode-p
      (load-theme 'x-light t)
    (load-theme 'x-dark t))
  (setq x-faces-dark-mode-p (not x-faces-dark-mode-p)))

(spacemacs-keys-set-leader-keys "t t" #'x-faces/toggle-dark-mode)

(let ((this-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'custom-theme-load-path (concat this-dir "x-faces/")))

(load-theme 'x-dark t)

;; Configure packages

(use-package x-ligatures
  :if (display-graphic-p)
  :functions (x-ligatures-init)
  :config
  (progn
    (add-hook 'prog-mode-hook #'x-ligatures-init)
    (add-hook 'text-mode-hook #'x-ligatures-init)
    (global-prettify-symbols-mode +1)))

(provide 'x-faces)

;;; x-faces.el ends here
