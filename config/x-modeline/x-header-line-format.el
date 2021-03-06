;;; x-header-line-format.el --- Functions for constructing the header line format string.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'subr-x)

(autoload 'magit-get-current-branch "magit-git")
(autoload 'projectile-project-p "projectile")

(defgroup x-header-line-format nil
  "Utilities for constructing the header line."
  :group 'themes
  :prefix "x-header-line-format-")

(defface x-header-line-format-nonemphased-element
  '((t
     (:inherit header-line)))
  "Face for non-emphasised elements in the header line."
  :group 'x-header-line-format)

(defface x-header-line-format-project-name
  '((t
     (:inherit header-line)))
  "Face for project name in header line."
  :group 'x-header-line-format)

(defface x-header-line-format-branch-name
  '((t
     (:inherit header-line)))
  "Face for git branch in header line."
  :group 'x-header-line-format)

(defface x-header-line-format-host-name
  '((t
     (:inherit header-line)))
  "Face for host-name in header line."
  :group 'x-header-line-format)

(defface x-header-line-format-narrowing
  '((t
     (:inherit header-line :slant italic)))
  "Face for git branch in header line."
  :group 'x-header-line-format)


;;; Cache variable lookups to improve speed

(defconst x-header-line-format--cache-duration-seconds 10)

(defun x-header-line-format--make-cache-key ()
  (cons (current-time) default-directory))

(defun x-header-line-format--cache-expired? (key)
  (-let* (((time . key-directory) key)
          (expiry-time (time-add time x-header-line-format--cache-duration-seconds)))

    (or (time-less-p expiry-time (current-time))
        (not (equal default-directory key-directory)))))

;; Cache the git branch.

(defvar-local x-header-line-format--branch nil
  "A cons of (cache-key . branch-name) or nil")

(defun x-header-line-format--update-branch ()
  (let ((key (x-header-line-format--make-cache-key))
        (branch (magit-get-current-branch)))
    (setq x-header-line-format--branch (cons key branch))
    branch))

(defun x-header-line-format--current-branch ()
  (require 'magit)
  (-if-let ((key . branch) x-header-line-format--branch)
      (cond
       ((x-header-line-format--cache-expired? key)
        (x-header-line-format--update-branch))
       (t
        branch))
    (x-header-line-format--update-branch)))

;; Cache the projectile project.
;;
;; Projectile maintains its own cache of project info, but it still does file IO
;; as part of its checks.

(defvar-local x-header-line-format--project nil
  "A cons of (cache-key . project-name) or nil")

(defun x-header-line-format--update-project ()
  (let ((key (x-header-line-format--make-cache-key))
        (project (projectile-project-p)))
    (setq x-header-line-format--project (cons key project))
    project))

(defun x-header-line-format--current-project ()
  (-if-let ((key . project) x-header-line-format--project)
      (cond
       ((x-header-line-format--cache-expired? key)
        (x-header-line-format--update-project))
       (t
        project))
    (x-header-line-format--update-project)))


;;; Helper for testing if window selected.

(defvar x-header-line-format--window-for-redisplay nil
  "The window currently being redisplayed.")

(defun x-header-line-format--set-window-for-redisplay (_)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq x-header-line-format--window-for-redisplay (selected-window))))

(add-function :before pre-redisplay-function #'x-header-line-format--set-window-for-redisplay)

(defun x-header-line-format--window-selected? ()
  (eq x-header-line-format--window-for-redisplay (get-buffer-window)))


;;; Construction functions

(defun x-header-line-format--access-mode-info ()
  (let ((str (concat
              (if (and (buffer-file-name) (file-remote-p (buffer-file-name))) "@" "")
              (if buffer-read-only "%" "")
              (if (buffer-modified-p) "*" ""))))
    (propertize (s-pad-right 2 " " str) 'face 'x-header-line-format-nonemphased-element)))

(defun x-header-line-format--narrowing-info ()
  (if (buffer-narrowed-p)
      (propertize " (Narrowed) " 'face 'x-header-line-format-narrowing)
    ""))

(defun x-header-line-format--nonemphasised (str)
  (propertize str 'face 'x-header-line-format-nonemphased-element))

(defun x-header-line-format--project-info ()
  (let* ((project (x-header-line-format--current-project))
         (project (when project (directory-file-name project)))
         (project-root-name (when project (file-name-nondirectory project)))
         (branch (when project (x-header-line-format--current-branch)))
         (subdir (when project (s-chop-prefix project (directory-file-name (file-truename default-directory))))))
    (cond
     ((and project branch)
      (concat (x-header-line-format--nonemphasised " (in ")
              (propertize project-root-name 'face 'x-header-line-format-project-name)
              (x-header-line-format--nonemphasised subdir)
              (x-header-line-format--nonemphasised " on ")
              (propertize branch 'face 'x-header-line-format-branch-name)
              (x-header-line-format--nonemphasised ") ")))
     (project
      (concat (x-header-line-format--nonemphasised " (in ")
              (propertize project-root-name 'face 'x-header-line-format-project-name)
              (x-header-line-format--nonemphasised ") ")))
     (t
      ""))))

(defun x-header-line-format--host-info ()
  (concat
   (x-header-line-format--nonemphasised " (at ")
   (propertize (and (boundp 'tramp-current-host) tramp-current-host) 'face 'x-header-line-format-host-name)
   (x-header-line-format--nonemphasised ") ")))

(defun x-header-line-format--context-info ()
  (cond
   ((not (x-header-line-format--window-selected?))
    "")
   ((file-remote-p default-directory)
    "")
   (t
    (x-header-line-format--project-info))))

(defun x-header-line-format--buffer-name ()
  (if (x-header-line-format--window-selected?)
      (buffer-name)
    (propertize (buffer-name) 'face 'x-header-line-format-nonemphased-element)))

(defun x-header-line-format--line-info ()
  (let ((str "%2l:%2c"))
    (if (x-header-line-format--window-selected?)
        str
      (propertize str 'face 'x-header-line-format-nonemphased-element))))

(defconst x-header-line-format
  '(
    ;; Print error on low memory
    "%e"
    " "

    ;; Emacsclient info
    mode-line-client

    ;; Current line, padded
    (:eval (x-header-line-format--line-info))
    "  "
    (:propertize "%6p " face x-header-line-format-nonemphased-element)

    ;; Modification indicator.
    (:eval (x-header-line-format--access-mode-info))

    ;; Buffer name, with braces on recursive edit
    "  %[" (:eval (x-header-line-format--buffer-name)) "%] "

    (:eval (x-header-line-format--narrowing-info))
    (:eval (x-header-line-format--context-info))

    " "

    ;; Global mode string, etc.
    (:eval (if (x-header-line-format--window-selected?) mode-line-misc-info ""))))

(provide 'x-header-line-format)

;;; x-header-line-format.el ends here
