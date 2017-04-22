;;; x-go.el --- Configuration for golang.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 's)
(require 'spacemacs-keys)

(autoload 'evil-define-key "evil-core")
(autoload 'projectile-project-p "projectile")

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)

  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "me" "playground")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mg" "goto")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mh" "help")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mi" "imports")

    (spacemacs-keys-set-leader-keys-for-major-mode 'go-mode
      "hh" 'godoc-at-point
      "ig" 'go-goto-imports
      "ia" 'go-import-add
      "ir" 'go-remove-unused-imports
      "eb" 'go-play-buffer
      "er" 'go-play-region
      "ed" 'go-download-play
      "ga" 'ff-find-other-file
      "gc" 'go-coverage))

  :preface
  (progn
    (defun x-go-lookup-go-root ()
      (-let* ((default-directory (or (projectile-project-p) default-directory))
              (output (s-lines (s-trim (shell-command-to-string "go env"))))
              ((&alist "GOROOT" go-root)
               (--map (-let* (((var val) (s-split "=" it))
                              ((_ val) (s-match (rx "\"" (group (*? nonl)) "\"") val)))
                        (cons var val))
                      output)))
        go-root))

    (defun x-go--set-local-vars ()
      (setq-local tab-width 4)
      (setq-local indent-tabs-mode t)
      (with-no-warnings
        (setq-local evil-shift-width 4))
      (unless (getenv "GOROOT")
        (setenv "GOROOT" (x-go-lookup-go-root)))))

  :config
  (progn
    (setq gofmt-show-errors nil)
    (evil-define-key 'normal go-mode-map (kbd "K") #'godoc-at-point)

    (add-hook 'go-mode-hook #'x-go--set-local-vars)
    (add-hook 'before-save-hook #'gofmt-before-save))

  :functions (gofmt-before-save godoc-at-point))

(use-package company-go
  :after go-mode

  :preface
  (progn
    (autoload 'company-mode "company")

    (defun x-go-company-setup ()
      (with-no-warnings
        (setq-local company-backends '(company-go)))
      (company-mode)))

  :config
  (progn
    (with-no-warnings
      (setq company-go-show-annotation t))
    (add-hook 'go-mode-hook #'x-go-company-setup)))

(use-package go-eldoc
  :after go-mode
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package x-go-run
  :after go-mode
  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mt" "test")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mx" "execute")
    (spacemacs-keys-set-leader-keys-for-major-mode
      'go-mode
      "tt" 'x-go-run-test-current-function
      "ts" 'x-go-run-test-current-suite
      "tp" 'x-go-run-package-tests
      "tP" 'x-go-run-package-tests-nested
      "x" 'x-go-run-main))
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*go " (or "test" "run") "*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 0)
                 (window-height   . 0.2))))

(use-package autoinsert
  :preface
  (defconst x-go-autoinsert-form
    '((go-mode . "Go")
      nil
      "package " (s-lower-camel-case (f-no-ext (f-filename (buffer-file-name)))) \n \n
      _ \n))

  :config
  (add-to-list 'auto-insert-alist x-go-autoinsert-form))

(use-package go-peg-mode
  :mode ("\\.peg\\'" . go-peg-mode))

(provide 'x-go)

;;; x-go.el ends here
