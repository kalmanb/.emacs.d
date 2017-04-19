;;; x-light-theme.el --- Light colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme x-light)

(require 'x-theme-common)

(apply #'custom-theme-set-faces 'x-light (x-theme-common-make-theme "black" "white"))

(provide-theme 'x-light)

;;; x-light-theme.el ends here
