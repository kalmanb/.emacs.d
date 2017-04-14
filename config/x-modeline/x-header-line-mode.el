;;; x-header-line-mode.el --- Minor mode for toggling the header line.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst x-header-line-function #'ignore
  "0-argument function returning the header line format string.")

;;;###autoload
(define-minor-mode x-header-line-mode
  "Minor mode to show or hide the header line."
  nil nil nil
  (if x-header-line-mode
      (setq header-line-format (funcall x-header-line-function))
    (setq header-line-format nil)))

;;;###autoload
(defun x-header-line-mode-on ()
  "Explicitly enable `x-header-line-mode'."
  (interactive)
  (x-header-line-mode +1))

;;;###autoload
(define-globalized-minor-mode x-header-line-global-mode x-header-line-mode
  x-header-line-mode-on)

(provide 'x-header-line-mode)

;;; x-header-line-mode.el ends here
