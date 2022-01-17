;;; pretty-speedbar.el --- Make speedbar pretty -*- lexical-binding: t -*-
;; Copyright (C) 2022 Kristle Chester, all rights reserved.
;; Author: Kristle Chester <kcyarn7@gmail.com>
;; Maintainer: Kristle Chester <kcyarn7@gmail.com>
;; Created: 2019-11-18
;; Version: 0.2
;; Last-Updated: 2022-01-13
;; URL: https://github.com/kcyarn/pretty-speedbar
;; Package-Requires: ((emacs "27.1"))
;; Keywords: file, speedbar, sr-speedbar.el
;; Compatibility:
;; License: GPL-3.0-or-later
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Customize interface definitions.

;;; Code:

(require 'speedbar)
(require 'svg)

(defgroup pretty-speedbar nil
  "Group for pretty-speedbar."
  :group 'pretty-speedbar
  :prefix "pretty-speedbar-")

(defcustom pretty-speedbar-font "Font Awesome 5 Free Solid"
  "Set the default icon font To Font Awesome 5 Free Solid obtainable as an otf from github."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-icon-size 20
  "Set default size in pixels and applies to both width and height."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-icon-fill "#C8C8C8"
  "Set default non-folder fill color as hex."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-icon-stroke "#333333"
  "Set default non-folder stroke color as hex."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-icon-folder-fill "#888888"
  "Set default folder fill color as hex."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-icon-folder-stroke "#333333"
  "Set default folder stroke color as hex."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-check-fill "#FF0000"
  "Set default non-folder fill color as hex."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-check-stroke "#333333"
  "Set default non-folder stroke color as hex."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-icons-dir
  (expand-file-name (concat user-emacs-directory "pretty-speedbar-icons/"))
  "Store pretty-speedbar-icons in the pretty-speedbar-icons folder located in the user's default Emacs directory."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-checkmark nil
  "Turn the checkmark for files check out of a VC on or off.
Defaults to off."
  :group 'pretty-speedbar)

;;; Match the icon names to the unicode, but keep this configurable by the user.

(defcustom pretty-speedbar-lock '("pretty-lock" "\uf023" nil)
  "Lock icon from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-chevron-down '("pretty-chevron-down" "\uf13a" nil)
  "Chevron-circle-down from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-chevron-right '("pretty-chevron-right" "\uf138" nil)
  "Chevron-circle-right from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-tag '("pretty-tag" "\uf02b" nil)
  "Tag from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-tags '("pretty-tags" "\uf02c" nil)
  "Tags (plural) from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-info '("pretty-info" "\uf05a" nil)
  "Info-circle from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-mail '("pretty-mail" "\uf0e0" nil)
  "Envelope from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-book '("pretty-book" "\uf02d" nil)
  "Book from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-box-closed '("pretty-box-closed" "\uf466" nil)
  "Box from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-box-open '("pretty-box-open" "\uf49e" nil)
  "Box-open from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-folder '("pretty-folder" "\uf07b" t)
  "Folder from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-folder-open '("pretty-folder-open" "\uf07c" t)
  "Folder-open from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-page '("pretty-page" "\uf15c" nil)
  "File-alt from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-plus '("pretty-plus" "\uf0fe" nil)
  "Plus-square from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-minus '("pretty-minus" "\uf146" nil)
  "Minus-square from FontAwesome."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-check '("pretty-check" "\uf00c")
  "Check from FontAwesome."
  :group 'pretty-speedbar)

(defun pretty-speedbar-blank-svg ()
  "Create a blank image to replace the checkmarks."
  (let ((this-svg (svg-create 1 1 :viewBox "0 0 512 512")))
    (with-temp-file (expand-file-name "blank.svg" pretty-speedbar-icons-dir)
    (set-buffer-multibyte nil)
    (svg-print this-svg))))

(defun pretty-speedbar-check-svg ()
  "Create a checkmark smaller than the normal icons."
  (setq this-name (car pretty-speedbar-check))
  (setq this-icon (nth 1 pretty-speedbar-check))
  (setq this-size (* 0.5 pretty-speedbar-icon-size))

  (let ((this-svg (svg-create this-size this-size :viewBox "0 0 512 512")))
  (svg-text this-svg this-icon
	    :font-size "470px" ;;; changed from 507
	    :stroke pretty-speedbar-check-stroke
	    :fill pretty-speedbar-check-fill
	    :font-family  (format "\'%s\'" pretty-speedbar-font)
	    :x "50%"
	    :y "450" ;;; changed from 90%
	   ;; :dominant-baseline "middle"
	    :text-anchor "middle"
	    :rendering "optimizeLegibility"
	    :stroke-width 5)
    (with-temp-file (expand-file-name (concat this-name ".svg") pretty-speedbar-icons-dir)
    (set-buffer-multibyte nil)
    (svg-print this-svg))))

(defun pretty-speedbar-svg (this-list)
  "Function to create individual icon svgs with THIS-LIST being the individual icon's variable."

  (setq this-name (car this-list))
  (setq this-icon (nth 1 this-list))
  (setq is-folder (nth 2 this-list))

  (if is-folder
    (setq this-pretty-icon-fill pretty-speedbar-icon-folder-fill)
    (setq this-pretty-icon-fill pretty-speedbar-icon-fill))

    (if is-folder
      (setq this-pretty-icon-stroke pretty-speedbar-icon-folder-stroke)
      (setq this-pretty-icon-stroke pretty-speedbar-icon-stroke)) 
  
 (let ((this-svg (svg-create pretty-speedbar-icon-size  pretty-speedbar-icon-size :viewBox "0 0 512 512")))
  (svg-text this-svg this-icon
	    :font-size "470px" ;;; changed from 507
	    :stroke this-pretty-icon-stroke
	    :fill  this-pretty-icon-fill
	    :font-family  (format "\'%s\'" pretty-speedbar-font)
	    :x "50%"
	    :y "450" ;;; changed from 90%
	   ;; :dominant-baseline "middle"
	    :text-anchor "middle"
	    :rendering "optimizeLegibility"
	    :stroke-width 5)
    (with-temp-file (expand-file-name (concat this-name ".svg") pretty-speedbar-icons-dir)
    (set-buffer-multibyte nil)
    (svg-print this-svg))
  ;;; Although using these from :data instead of :file is theoretically possible, it lags badly. Save the files and the headache.
  ))

;;; complete generator combines all variables with pretty-test-icon function

(defun pretty-speedbar-make-dir ()
  "Create the pretty-speedbar-icons directory."
  (unless (file-exists-p pretty-speedbar-icons-dir)
    (make-directory pretty-speedbar-icons-dir t)))

(defun pretty-speedbar-generate()
  "Generate the icon svg images."
  (interactive)
  (pretty-speedbar-make-dir)
  (pretty-speedbar-blank-svg)
  (pretty-speedbar-check-svg)

  (pretty-speedbar-svg pretty-speedbar-lock)
  (pretty-speedbar-svg pretty-speedbar-chevron-down)
  (pretty-speedbar-svg pretty-speedbar-chevron-right)
  (pretty-speedbar-svg pretty-speedbar-tag)
  (pretty-speedbar-svg pretty-speedbar-tags)
  (pretty-speedbar-svg pretty-speedbar-info)
  (pretty-speedbar-svg pretty-speedbar-mail)
  (pretty-speedbar-svg pretty-speedbar-book)
  (pretty-speedbar-svg pretty-speedbar-box-closed)
  (pretty-speedbar-svg pretty-speedbar-box-open)
  (pretty-speedbar-svg pretty-speedbar-folder)
  (pretty-speedbar-svg pretty-speedbar-folder-open)
  (pretty-speedbar-svg pretty-speedbar-page)
  (pretty-speedbar-svg pretty-speedbar-plus)
  (pretty-speedbar-svg pretty-speedbar-minus))

;;; should be toggled by the setq
(defmacro pretty-speedbar-blank ()
  "A blank svg image."
  `(defezimage pretty-blank ((:type svg :file ,(expand-file-name "blank.svg" pretty-speedbar-icons-dir) :ascent center)) "A blank svg image."))
(pretty-speedbar-blank)

(defmacro pretty-speedbar-ezimage (this-var)
  "Macro for defezimage using the defcustom icon settings list passed as THIS-VAR."
  (setq this-name (car this-var))
  (setq this-icon (nth 1 this-var))
  `(defezimage ,this-name ((:type svg :file ,(expand-file-name (concat this-name ".svg") pretty-speedbar-icons-dir) :ascent center)) "Placeholder documentation string."))

;;;###autoload



(provide 'pretty-speedbar)
;;; pretty-speedbar.el ends here
