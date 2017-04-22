;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.
;;
;; One main difference from other configurations out there is that I use git subtrees for
;; many core packages, instead of relying on the Emacs package manager.


;;; Code:
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Initialize package.el
;;
;; Most packages are installed using git subtrees, but some packages (such as
;; flycheck) break unless installed via package.el.

(require 'package)
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package.

(require 'seq)
(require 'subr-x)

(defun x-init/init-load-path (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (config-dir (expand-file-name "config" user-emacs-directory))
         (git-subtrees
          (seq-filter #'file-directory-p
                      (directory-files lisp-dir t "^[^.]")))
         (config-subtrees
          (seq-filter #'file-directory-p
                      (directory-files config-dir t "^[^.]"))))
    (dolist (path (append (list lisp-dir config-dir) config-subtrees git-subtrees))
      (add-to-list 'load-path path)
      (add-to-list 'Info-default-directory-list path)
      (add-to-list 'load-path (concat path "/emacs"))
      (add-to-list 'load-path (concat path "/elisp"))
      (add-to-list 'load-path (concat path "/lisp")))

    ;; TODO
    ;; (add-to-list 'load-path (concat lisp-dir "/org-mode/contrib/lisp"))
    ;; (add-to-list 'load-path (concat lisp-dir "/gocode/emacs-company"))

    (setq load-path (seq-filter #'file-directory-p load-path))
    (setq Info-default-directory-list (seq-filter #'file-directory-p Info-default-directory-list))

    (when interactive-p
      (if-let (added (seq-difference load-path before))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

(x-init/init-load-path)
(defconst use-package-verbose t)
(require 'use-package)


;; Load features
(use-package x-emacs)
(use-package x-darwin :if (equal system-type 'darwin))
(use-package x-basic-settings)
(use-package x-leader-keys)
(use-package x-evil)
(use-package x-ivy)
(use-package x-ag)
(use-package x-projectile)
(use-package x-elisp)
(use-package x-modeline)
(use-package x-company)
(use-package x-autoinsert)
(use-package x-faces)
(use-package x-smartparens)
(use-package x-flycheck)
(use-package x-magit)
(use-package x-avy)
(use-package x-yasnippet)
(use-package x-dired)
(use-package x-undo-tree)
(use-package x-ws-butler)
(use-package x-parentheses)
(use-package x-aggressive-indent)
(use-package x-ibuffer)
(use-package x-markdown)

;; TODO


;; (use-package cb-auto-save)
;; (use-package cb-server)
;; (use-package cb-hexl)
;; (use-package cb-volatile-highlights)
;; (use-package cb-info)
;; (use-package cb-highlight-todo)
;; (use-package cb-neotree)
;; (use-package cb-mu4e)
;; (use-package cb-ahs)
;; (use-package cb-org)
;; (use-package cb-ledger)
;; (use-package cb-scala)
;; (use-package cb-groovy)
;; (use-package cb-rust)
;; (use-package cb-yaml)
;; (use-package cb-go)
;; (use-package cb-spelling)
;; (use-package cb-diff)
;; (use-package cb-coffeescript)
;; (use-package cb-web-mode)
;; (use-package cb-apidoc)
;; (use-package cb-restclient)
;; (use-package cb-calc)
;; (use-package cb-haskell)
;; (use-package cb-shell)
;; (use-package cb-csv)
;; (use-package cb-highlight-thing)
;; (use-package cb-idris)
;; (use-package cb-docker)

;; (use-package personal-config
;;   :load-path "~/Sync/emacs")

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))


(provide 'init)

;;; init.el ends here
