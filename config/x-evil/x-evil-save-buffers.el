;;; x-evil-save-buffers.el --- <enter description here>  -*- lexical-binding: t; -*-

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

(defun x-evil-save-buffers-and-nope ()
  (interactive)
  (save-current-buffer)
  (message "Nope"))

(provide 'x-evil-save-buffers)

;;; x-evil-save-buffers.el ends here
