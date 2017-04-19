;;; x-dark-theme.el --- Dark colour theme. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme x-dark)

(require 'x-theme-common)

(apply #'custom-theme-set-faces 'x-dark (x-theme-common-make-theme "#ccc" "#111"))

(provide-theme 'x-dark)

;;; x-dark-theme.el ends here
