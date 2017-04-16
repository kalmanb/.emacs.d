;;; x-company.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :commands (global-company-mode)

  :bind
  (("S-<return>" . company-complete))

  :preface
  (defun x-company--set-company-vars ()
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :commands (company-select-next
             company-select-previous
             company-show-doc-buffer)

  :init
  (add-hook 'after-init-hook #'global-company-mode)

  :config
  (progn
    (setq company-idle-delay 0.3)
    (setq company-require-match nil)

    (dolist (map (list company-active-map company-search-map company-filter-map))
      (define-key map (kbd "C-n") #'company-select-next)
      (define-key map (kbd "C-p") #'company-select-previous)
      (define-key map (kbd "C-h") #'company-show-doc-buffer)
      (define-key map (kbd "C-w") nil))

    (add-hook 'company-mode-hook #'x-company--set-company-vars)))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(provide 'x-company)

;;; x-company.el ends here
