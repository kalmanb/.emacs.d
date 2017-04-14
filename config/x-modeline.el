;;; x-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package x-header-line-format
  :defines x-header-line-format
  :config
  (setq-default header-line-format x-header-line-format))

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode)
  :init
  (setq-default mode-line-format " "))

(use-package x-header-line-mode
  :commands (x-header-line-global-mode x-header-line-mode x-header-line-mode-on)
  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "tM" #'x-header-line-mode
      "tm" #'x-header-line-global-mode)
    (add-hook 'after-init-hook #'x-header-line-global-mode))
  :config
  (setq x-header-line-function (lambda () x-header-line-format)))

(provide 'x-modeline)

;;; x-modeline.el ends here
