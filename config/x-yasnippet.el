;;; x-yasnippet.el --- Configure yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)
(require 's)

(use-package yasnippet
  :defer 1

  :preface
  (progn
    (autoload 'sp-backward-delete-char "smartparens")
    (autoload 'evil-define-key "evil-core")

    (defun x-yasnippet-preserve-indentation (f &rest args)
      (let ((col
             (save-excursion
               (back-to-indentation)
               (current-column))))
        (apply f args)
        (save-excursion
          (atomic-change-group
            (goto-char (line-beginning-position))
            (delete-horizontal-space)
            (indent-to col)))))

    (defun x-yasnippet--maybe-goto-field-end ()
      "Move to the end of the current field if it has been modified."
      (when-let (field (x-yasnippet--current-field))
        (when (and (yas--field-modified-p field)
                   (yas--field-contains-point-p field))
          (goto-char (x-yasnippet--end-of-field)))))

    (defun x-yasnippet-goto-field-end (&rest _)
      (x-yasnippet--maybe-goto-field-end)
      (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-insert-state))
        (evil-insert-state)))

    (defun x-yasnippet--current-field ()
      "Return the current active field."
      (and (boundp 'yas--active-field-overlay)
           yas--active-field-overlay
           (overlay-buffer yas--active-field-overlay)
           (overlay-get yas--active-field-overlay 'yas--field)))

    (defun x-yasnippet--start-of-field ()
      (when-let (field (x-yasnippet--current-field))
        (marker-position (yas--field-start field))))

    (defun x-yasnippet--end-of-field ()
      (when-let (field (x-yasnippet--current-field))
        (marker-position (yas--field-end field))))

    (defun x-yasnippet--current-field-text ()
      "Return the text in the active snippet field."
      (when-let (field (x-yasnippet--current-field))
        (yas--field-text-for-display field)))

    (defun x-yasnippet-clear-blank-field (&rest _)
      "Clear the current field if it is blank."
      (when-let ((beg (x-yasnippet--start-of-field))
                 (end (x-yasnippet--end-of-field))
                 (str (x-yasnippet--current-field-text)))
        (when (s-matches? (rx bos (+ space) eos) str)
          (delete-region beg end)
          t)))


    (defun x-yasnippet-space ()
      "Clear and skip this field if it is unmodified.  Otherwise insert a space."
      (interactive "*")
      (let ((field (x-yasnippet--current-field))
            ;; (sp-mode? (and (boundp 'smartparens-mode) smartparens-mode))
            )
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              ;; (sp-mode?
              ;;  (sp-generic-prog-space))
              (t
               (call-interactively #'self-insert-command)))))

    (defun x-yasnippet-backspace ()
      "Clear the current field if the current snippet is unmodified.
Otherwise delete backwards."
      (interactive "*")
      (let ((field (x-yasnippet--current-field))
            (sp-mode? (and (boundp 'smartparens-mode) smartparens-mode)))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              ;; ((and sp-mode? (derived-mode-p 'prog-mode))
              ;;  (sp-generic-prog-backspace))
              (sp-mode?
               (call-interactively #'sp-backward-delete-char))
              (t
               (call-interactively #'backward-delete-char))))))

  :init
  (progn
    (spacemacs-keys-declare-prefix "y" "yasnippet")
    (spacemacs-keys-set-leader-keys
      "yf" #'yas-visit-snippet-file
      "ye" #'yas-expand
      "yn" #'yas-new-snippet
      "yy" #'yas-insert-snippet)

    ;; Fix malformed face decl

    (defface yas-field-highlight-face
      '((t (:inherit region)))
      "The face used to highlight the currently active field of a snippet"))

  :config
  (progn
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions '(yas-completing-prompt))
    (setq yas-verbosity 0)
    (setq yas-minor-mode-map (make-sparse-keymap))
    (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))

    (yas-global-mode +1)

    (add-to-list 'yas-dont-activate-functions (lambda () (derived-mode-p 'term-mode)))

    ;; Define key bindings for fancy snippet navigation.

    (bind-key (kbd "TAB") #'yas-expand yas-minor-mode-map)
    (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") #'yas-expand)

    (evil-define-key 'insert yas-keymap (kbd "SPC") #'x-yasnippet-space)
    (bind-key (kbd "<backspace>") #'x-yasnippet-backspace yas-keymap)

    ;; Advise editing commands.
    ;;
    ;; Pressing SPC in an unmodified field will clear it and switch to the next.
    ;;
    ;; Pressing S-TAB to go to last field will place point at the end of the field.

    (advice-add #'yas-next-field :before #'x-yasnippet-clear-blank-field)
    (advice-add #'yas-prev-field :before #'x-yasnippet-clear-blank-field)
    (advice-add #'yas-next-field :after #'x-yasnippet-goto-field-end)
    (advice-add #'yas-prev-field :after #'x-yasnippet-goto-field-end)

    ;; Ensure yasnippet expansion preserves current indentation. This can be a
    ;; problem in modes with significant whitespace, where the indentation
    ;; command unconditionally indents one step.

    (advice-add 'yas--expand-or-prompt-for-template :around #'x-yasnippet-preserve-indentation))

  :commands
  (yas-expand
   yas-global-mode
   yas-insert-snippet
   yas-new-snippet
   yas-next-field
   yas-prev-field
   yas-visit-snippet-file)

  :functions
  (yas--skip-and-clear
   yas--field-contains-point-p
   yas--field-text-for-display))

(use-package warnings
  :defer t
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package x-yas-elisp
  :after yasnippet)

(use-package x-yas-js
  :after yasnippet)

(provide 'x-yasnippet)

;;; x-yasnippet.el ends here
