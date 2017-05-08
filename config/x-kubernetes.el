;;; x-kubernetes.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Kalman Bekesi

;; Author: Kalman Bekesi <kalmanbekesi@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package kubernetes
  :ensure t  ;; Don't use this, will download deps from elpa
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(provide 'x-kubernetes)

;;; x-kubernetes.el ends here
