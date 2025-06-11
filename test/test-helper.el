;;; test-helper.el --- DEFLATE test initialization   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: deflate, compression, algorithm, zlib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'f)

(defvar package-test-path
  (f-dirname (f-this-file)))

(defvar package-code-path
  (f-parent package-test-path))

(defun load-deflate ()
  "Load the package before testing it."
  (require 'deflate (f-expand "deflate.el" package-code-path)))

;; enable code coverage
(when (require 'undercover nil t)
  (undercover "deflate.el"))

(load-deflate)

(provide 'test-helper)
;;; test-helper.el ends here
