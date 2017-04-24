;;; x-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'dash)
(autoload 'f-split "f")

(use-package web-mode
  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)

  :defer t

  :preface
  (autoload 'sp-local-pair "smartparens")

  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

    ;; Disable web-mode-reload binding
    (define-key web-mode-map (kbd "C-c C-r") nil)

    ;; Use line comments when commenting in JS.

    (setf (cdr (assoc "javascript" web-mode-comment-formats)) "//")

    ;; Change default indentation behaviour.

    (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)

    ;; Treat es6 files as JS files.

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
    (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))))

(use-package x-web-modes
  :defer t
  :mode (("\\.json\\'" . x-web-json-mode)
         ("\\.eslintrc\\'" . x-web-json-mode)
         ("\\.babelrc\\'" . x-web-json-mode)
         ("\\.es6\\'"  . x-web-js-mode)
         ("\\.tsx?\\'"  . x-web-typescript-mode)
         ("\\.jsx?\\'" . x-web-js-mode)
         ("\\.css\\'"  . x-web-css-mode)
         ("\\.scss\\'"  . x-web-css-mode)
         ("\\.html\\'" . x-web-html-mode))
  :defines (flycheck-html-tidy-executable)
  :config
  (with-eval-after-load 'flycheck
    (let ((tidy-bin "/usr/local/bin/tidy"))
      (when (file-exists-p tidy-bin)
        (setq flycheck-html-tidy-executable tidy-bin)))

    (flycheck-add-mode 'typescript-tslint 'x-web-typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'x-web-js-mode)
    (flycheck-add-mode 'css-csslint 'x-web-css-mode)
    (flycheck-add-mode 'json-jsonlint 'x-web-json-mode)
    (flycheck-add-mode 'html-tidy 'x-web-html-mode)))

(use-package flycheck
  :defer t
  :commands (flycheck-select-checker)
  :functions (flycheck-add-next-checker flycheck-add-mode)
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")
    (defun x-web--add-node-modules-bin-to-path ()
      "Use binaries from node_modules, where available."
      (when-let (root (projectile-project-p))
        (make-local-variable 'exec-path)
        (add-to-list 'exec-path (f-join root "node_modules" ".bin")))))

  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
    (add-to-list 'flycheck-disabled-checkers 'css-csslint)

    (add-hook 'x-web-typescript-mode-hook #'x-web--add-node-modules-bin-to-path)
    (add-hook 'x-web-css-mode-hook #'x-web--add-node-modules-bin-to-path)
    (add-hook 'x-web-js-mode-hook #'x-web--add-node-modules-bin-to-path)))

(use-package emmet-mode
  :defer t
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode emmet-expand-line)
  :preface
  (progn
    (defun x-web--set-jsx-classname-on ()
      (setq-local emmet-expand-jsx-className? t))

    (defun x-web--maybe-emmet-mode ()
      (cond
       ((derived-mode-p 'x-web-html-mode 'html-mode 'nxml-mode)
        (emmet-mode +1))

       ((and (derived-mode-p 'x-web-js-mode)
             (buffer-file-name)
             (memq "components" (f-split (buffer-file-name))))
        (emmet-mode +1)))))

  :init
  (add-hook 'web-mode-hook #'x-web--maybe-emmet-mode)
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (define-key emmet-mode-keymap (kbd "TAB") #'emmet-expand-line)
    (add-hook 'x-web-js-mode-hook #'x-web--set-jsx-classname-on)))

(use-package x-flow-checker
  :disabled t
  :defer t
  :after flycheck)

(use-package flycheck-flow
  :after flycheck
  :config
  (progn
    (flycheck-add-mode 'javascript-flow 'x-web-js-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))

(use-package x-flycheck-stylelint
  :after flycheck
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")
    (defun x-web--set-stylelintrc ()
      "Set either local or root stylelintrc"
      (-if-let* ((root (projectile-project-p))
                 (root-rc (f-join root ".stylelintrc.json")))
          (setq-local flycheck-stylelintrc root-rc))
      (f-join user-emacs-directory "lisp" ".stylelintrc.json")))
  :config
  (progn
    (flycheck-add-mode 'css-stylelint 'x-web-css-mode)
    (add-hook 'x-web-css-mode-hook #'x-web--set-stylelintrc)))

(use-package flow-minor-mode
  :after x-web-modes
  :commands (flow-minor-type-at-pos
             flow-minor-status
             flow-minor-suggest
             flow-minor-coverage
             flow-minor-jump-to-definition)
  :preface
  (progn
    (autoload 's-matches? "s")
    (defun x-flow-insert-flow-annotation ()
      "Insert a flow annotation at the start of this file."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (if (s-matches? (rx (or (and "//" (* space) "@flow")
                                (and "/*" (* space) "@flow" (* space) "*/")))
                        (buffer-substring (line-beginning-position) (line-end-position)))
            (user-error "Buffer already contains an @flow annotation")
          (insert "// @flow\n")
          (message "Inserted @flow annotation.")))))
  :config
  (progn
    (add-hook 'x-web-js-mode 'flow-minor-enable-automatically))
  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'x-web-js-mode "m f" "flow")
    (spacemacs-keys-set-leader-keys-for-major-mode 'x-web-js-mode
      "fi" #'x-flow-insert-flow-annotation
      "ft" #'flow-minor-type-at-pos
      "fs" #'flow-minor-suggest
      "fS" #'flow-minor-status
      "fc" #'flow-minor-coverage
      "fd" #'flow-minor-jump-to-definition)))

(use-package prettier-js
  :after x-web-modes
  :commands (prettier
             prettier-before-save)
  :config
  (progn
    (setq prettier-args '("--single-quote" "--trailing-comma=es5"))
    (setq prettier-target-mode "x-web-js-mode")
    (add-hook 'before-save-hook #'prettier-before-save))
  :init
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'x-web-js-mode
      "." #'prettier)))

(use-package company-flow
  :after x-web-modes
  :config
  (progn
    (setq company-flow-modes '(x-web-js-mode))

    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-flow))))

(use-package tern
  :defer t
  :functions (tern-mode)
  :commands (tern-find-definition
             tern-pop-find-definition
             tern-get-type
             tern-get-docs)
  :init
  (add-hook 'x-web-js-mode-hook #'tern-mode)
  :config
  (progn
    (setq tern-command (add-to-list 'tern-command "--no-port-file" t))

    (unless (getenv "NODE_PATH")
      (let* ((node-version
              (replace-regexp-in-string "\n\\'" ""
                                        (shell-command-to-string "node --version")))
             (node-path (format "~/.nvm/versions/node/%s/lib/node_modules" node-version)))
        (setenv "NODE_PATH" node-path)))

    (spacemacs-keys-declare-prefix-for-mode 'x-web-js-mode "m t" "tern")
    (spacemacs-keys-set-leader-keys-for-major-mode 'x-web-js-mode
      "tD" #'tern-find-definition
      "tp" #'tern-pop-find-definition
      "tt" #'tern-get-type
      "td" #'tern-get-docs)))

(use-package company-tern
  :after x-web-modes
  :config
  (progn
    (setq company-tern-meta-as-single-line t)
    (setq company-tern-property-marker " <p>")

    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern))))

(use-package aggressive-indent
  :defer t
  :preface
  (defun x-web--in-flow-strict-object-type? ()
    (when (derived-mode-p 'x-web-js-mode)
      (-let [(depth start) (syntax-ppss)]
        (and (plusp depth)
             (eq (char-after start) ?{)
             (eq (char-after (1+ start)) ?|)))))
  :config
  (progn
    (add-to-list 'aggressive-indent-dont-indent-if '(x-web--in-flow-strict-object-type?))
    (add-hook 'aggressive-indent-stop-here-hook #'x-web--in-flow-strict-object-type?)))


(provide 'x-web-mode)

;;; x-web-mode.el ends here
