;;; x-leader-keys.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)

(autoload 'evil-window-rotate-downwards "evil-commands")
(autoload 'x/alternate-buffer "x-alternate-buffer")
(autoload 'x/copy-buffer-path "x-copy-buffer-path")
(autoload 'x/rename-file-and-buffer "x-rename-file-and-buffer")
(autoload 'x/sudo-edit "x-sudo-edit")
(autoload 'x/toggle-window-split "x-toggle-window-split")
(autoload 'x-goto-init-file "x-goto")
(autoload 'x-goto-readme-file "x-goto")
(autoload 'x-goto-messages "x-goto")
(autoload 'x-goto-personal-config "x-goto")
(autoload 'org-narrow-to-subtree "org")

(use-package x-delete-current-buffer-and-file
  :commands (x/delete-current-buffer-and-file)
  :preface
  (progn
    (autoload 'projectile-invalidate-cache "projectile")
    (autoload 'projectile-project-p "projectile")

    (defun x-leader-keys--invalidate-cache (_path)
      (when (and (featurep 'projectile) (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))))

  :config
  (add-hook 'x-delete-current-buffer-and-file-functions #'x-leader-keys--invalidate-cache))

(use-package which-key
  :preface
  (progn
    (autoload 'which-key-mode "which-key")
    (autoload 'which-key-add-key-based-replacements "which-key"))

  :config
  (progn
    (setq which-key-special-keys nil)
    (setq which-key-use-C-h-commands t)
    (setq which-key-echo-keystrokes 0.02)
    (setq which-key-max-description-length 32)
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-idle-delay 0.4)
    (setq which-key-allow-evil-operators t)

    ;; Rename functions shown by which-key for legibility.

    (add-to-list 'which-key-description-replacement-alist
                 (cons (rx bos "x" (* (not (any "/"))) "/" (group (+ nonl)) eos) "\\1"))

    (which-key-add-key-based-replacements
      "SPC ,"   "smartparens"
      "SPC a"   "applications"
      "SPC b"   "buffers"
      "SPC c"   "comments"
      "SPC f"   "files"
      "SPC g"   "git/goto"
      "SPC h"   "help"
      "SPC h d" "describe"
      "SPC h f" "find"
      "SPC k"   "kill"
      "SPC n"   "narrow"
      "SPC o"   "org"
      "SPC p"   "project"
      "SPC w"   "window"
      "SPC s"   "symbols"
      "SPC t"   "toggles"
      "SPC m"   '("major-mode-cmd" . "Major mode commands"))

    (which-key-mode +1)))

(use-package spacemacs-keys
  :preface
  (progn
    (autoload 'evil-window-next "evil-commands")
    (autoload 'evil-window-split "evil-commands")
    (autoload 'evil-window-vsplit "evil-commands")
    (autoload 'counsel-git-log "counsel")

    (defun x-leader-keys/reload-file ()
      "Revisit the current file."
      (interactive)
      (when-let (path (buffer-file-name))
        (find-alternate-file path))))

  :config
  (progn
    (define-key universal-argument-map (kbd (concat "SPC u")) #'universal-argument-more)

    (spacemacs-keys-set-leader-keys
      "u"   #'universal-argument
      "SPC" #'mode-line-other-buffer
      "TAB" #'x/alternate-buffer
      "|"   #'x/toggle-window-split

      "!"   #'shell-command

      "b d" #'kill-this-buffer
      "b b" #'ivy-switch-buffer
      "b v" #'x-leader-keys/reload-file

      "C" #'compile

      "c r" #'comment-or-uncomment-region

      "f D" #'x/delete-current-buffer-and-file
      "f F" #'find-file-other-window
      "f R" #'x/rename-file-and-buffer
      "f e" #'x/sudo-edit
      "f f" #'find-file
      "f s" #'save-buffer
      "f S" #'save-some-buffers
      "f W" #'write-file
      "f v" #'x-leader-keys/reload-file
      "f y" #'x/copy-buffer-path

      "g i" #'x-goto-init-file
      "g r" #'x-goto-readme-file
      "g m" #'x-goto-messages
      "g p" #'x-goto-personal-config

      "g l" #'counsel-git-log

      "h d c" #'describe-face
      "h d k" #'describe-key
      "h d m" #'describe-mode
      "h f c" #'find-face-definition
      "h f f" #'find-function
      "h f l" #'find-library
      "h f v" #'find-variable
      "h i"   #'info

      "k b" #'kill-this-buffer
      "k w" #'delete-window

      "n d" #'narrow-to-defun
      "n f" #'narrow-to-defun
      "n r" #'narrow-to-region
      "n s" #'org-narrow-to-subtree
      "n w" #'widen

      "q" #'delete-window

      "w =" #'balance-windows
      "w w" #'evil-window-next
      "w o" #'delete-other-windows
      "w q" #'delete-window
      "w r" #'evil-window-rotate-downwards
      "w -" #'evil-window-split
      "w /" #'evil-window-vsplit)))

(use-package x-scale-font-transient-state
  :commands (x-scale-font-transient-state/body)
  :init
  (spacemacs-keys-set-leader-keys
    "zx" #'x-scale-font-transient-state/body))

(use-package x-buffer-transient-state
  :commands (x-buffer-transient-state/body
             x-buffer-transient-state/next-buffer
             x-buffer-transient-state/previous-buffer)
  :init
  (spacemacs-keys-set-leader-keys
    "bn" #'x-buffer-transient-state/next-buffer
    "bN" #'x-buffer-transient-state/previous-buffer
    "bp" #'x-buffer-transient-state/previous-buffer))


(provide 'x-leader-keys)

;;; x-leader-keys.el ends here
