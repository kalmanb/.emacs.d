;;; x-ivy-commands.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Kalman Bekesi

;; Author: Kalman Bekesi <kalmanb@Kalmans-MacBook-Pro.local>

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

(require 'counsel)
(require 'swiper)
(require 'subr-x)

(autoload 'projectile-project-root "projectile")

(defun x-swiper-region-or-symbol ()
  "Run `swiper' with the selected region or the symbol around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper input)))

(defun x-counsel-project-region-or-symbol ()
  "Search project for region or symbol at point."
  (interactive)
  (if-let ((sym (symbol-at-point)))
      (counsel-rg (symbol-name sym) (projectile-project-root))
    (counsel-rg nil (projectile-project-root))))

(defun x-counsel-region-or-symbol ()
  "Search initial directory for region or symbol at point."
  (interactive)
  (let ((init-dir (read-directory-name "Start from directory: ")))
    (if-let ((sym (symbol-at-point)))
        (counsel-rg (symbol-name sym) init-dir)
      (counsel-rg nil init-dir))))


(provide 'x-ivy-commands)

;;; x-ivy-commands.el ends here
