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

(defcustom pretty-speedbar-signs-fill "#FF0000"
  "Set default fill color for plus and minus signs added to select icons."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-icons-dir
  (expand-file-name (concat user-emacs-directory "pretty-speedbar-icons/"))
  "Store pretty-speedbar-icons in the pretty-speedbar-icons folder located in the user's default Emacs directory."
  :type '(string)
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-vc-enable t
  "Turn the checkmark for files checked out of a VC on or off.
Defaults to off."
  :group 'pretty-speedbar)

;;; Set the icon unicode and if it's folder.

(defcustom pretty-speedbar-lock '("\uf023" nil)
  "Lock icon from FontAwesome used by pretty-lock."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-chevron-down '("\uf13a" nil)
  "Chevron-circle-down from FontAwesome used by pretty-chevron-down."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-chevron-right '("\uf138" nil)
  "Chevron-circle-right from FontAwesome used by pretty-chevron-right."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-tag '("\uf02b" nil)
  "Tag from FontAwesome used by pretty-tag."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-tags '("\uf02c" nil)
  "Tags (plural) from FontAwesome used by pretty-tags."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-info '("\uf05a" nil)
  "Info-circle from FontAwesome used by pretty-info."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-mail '("\uf0e0" nil)
  "Envelope from FontAwesome used by pretty-mail."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-book '("\uf02d" nil)
  "Book from FontAwesome used by pretty-book."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-box-closed '("\uf466" nil)
  "Box from FontAwesome used by pretty-box-closed."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-box-open '("\uf49e" nil)
  "Box-open from FontAwesome used by pretty-box-open."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-folder '("\uf07b" t)
  "Folder from FontAwesome used by pretty-folder."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-folder-open '("\uf07c" t)
  "Folder-open from FontAwesome used by pretty-folder-open."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-page '("\uf15c" nil)
  "File-alt from FontAwesome used by pretty-page."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-blank-page '("\uf15b" nil)
  "File from FontAwesome used by pretty-page-plus and pretty-page-minus."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-plus '("\uf0fe" nil)
  "Plus-square from FontAwesome used by pretty-plus."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-minus '("\uf146" nil)
  "Minus-square from FontAwesome used by pretty-minus."
  :group 'pretty-speedbar)

(defcustom pretty-speedbar-check '("\uf00c")
  "Check from FontAwesome used by pretty-check."
  :group 'pretty-speedbar)

(defun pretty-speedbar-blank-svg ()
  "Create a blank image to replace the checkmarks."
  (let ((this-svg (svg-create 1 1 :viewBox "0 0 512 512")))
    (with-temp-file (expand-file-name "blank.svg" pretty-speedbar-icons-dir)
    (set-buffer-multibyte nil)
    (svg-print this-svg))))

(defun pretty-speedbar-check-svg ()
  "Create a checkmark smaller than the normal icons."
  (setq this-name "pretty-check")
  (setq this-icon (car pretty-speedbar-check))
  (setq this-size (* 0.7 pretty-speedbar-icon-size))

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

(defun pretty-speedbar-svg (this-name this-list &optional this-sign)
  "Function to create individual icon svgs with THIS-LIST being the individual icon's variable."
  (setq this-icon (car this-list))
  (setq is-folder (nth 1 this-list))

  (setq pretty-speedbar-icon-width (/ pretty-speedbar-icon-size 0.8643))

  (if is-folder
    (setq this-pretty-icon-fill pretty-speedbar-icon-folder-fill)
    (setq this-pretty-icon-fill pretty-speedbar-icon-fill))

    (if is-folder
      (setq this-pretty-icon-stroke pretty-speedbar-icon-folder-stroke)
      (setq this-pretty-icon-stroke pretty-speedbar-icon-stroke)) 
  
 (let ((this-svg (svg-create pretty-speedbar-icon-width  pretty-speedbar-icon-size :viewBox "0 0 512 512")))
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
  (if (equal this-sign "plus")
      (progn
        (svg-rectangle this-svg 224.62 270 64 204.8
		       :rx 12.8
		       :ry 12.8
		       :fill  pretty-speedbar-signs-fill
		       :fill-rule "evenodd"
		       :rendering "optimizeLegibility")
        (svg-rectangle this-svg -398 157.2 64 204.8
		       :transform "rotate(-90)"
		       :rx 12.8
		       :ry 12.8
		       :fill  pretty-speedbar-signs-fill
		       :fill-rule "evenodd"
		        :rendering "optimizeLegibility")))
  (if (equal this-sign "minus")
      (svg-rectangle this-svg -398 157.2 64 204.8
		 :transform "rotate(-90)"
		 :rx 12.8
		 :ry 12.8
		 :fill  pretty-speedbar-signs-fill
		 :fill-rule "evenodd"
		  :rendering "optimizeLegibility"))
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

  (pretty-speedbar-svg "pretty-lock" pretty-speedbar-lock)
  (pretty-speedbar-svg "pretty-chevron-down" pretty-speedbar-chevron-down)
  (pretty-speedbar-svg "pretty-chevron-right" pretty-speedbar-chevron-right)
  (pretty-speedbar-svg "pretty-tag" pretty-speedbar-tag)
  (pretty-speedbar-svg "pretty-tags" pretty-speedbar-tags)
  (pretty-speedbar-svg "pretty-info" pretty-speedbar-info)
  (pretty-speedbar-svg "pretty-mail" pretty-speedbar-mail)
  (pretty-speedbar-svg "pretty-book" pretty-speedbar-book)
  (pretty-speedbar-svg "pretty-box-closed" pretty-speedbar-box-closed "plus")
  (pretty-speedbar-svg "pretty-box-open" pretty-speedbar-box-open "minus")
  (pretty-speedbar-svg "pretty-page-plus" pretty-speedbar-blank-page "plus")
  (pretty-speedbar-svg "pretty-page-minus" pretty-speedbar-blank-page "minus")
  (pretty-speedbar-svg "pretty-folder" pretty-speedbar-folder)
  (pretty-speedbar-svg "pretty-folder-open" pretty-speedbar-folder-open)
  (pretty-speedbar-svg "pretty-page" pretty-speedbar-page)
  (pretty-speedbar-svg "pretty-plus" pretty-speedbar-plus)
  (pretty-speedbar-svg "pretty-minus" pretty-speedbar-minus))

;;; should be toggled by the setq
(defmacro pretty-speedbar-vc-icon (this-file)
  "A blank svg image."
  `(defezimage pretty-check ((:type svg :file ,(expand-file-name (format "%s.svg" this-file) pretty-speedbar-icons-dir) :ascent center)) "Replacement for ezimage-checkout, which is described as files checked out of a vc with toggle."))

(defun pretty-speedbar-toggle-vc ()
  "Function to toggle the VC checkmarks on and off."

  (if pretty-speedbar-vc-enable
      (pretty-speedbar-vc-icon "pretty-check")
      (pretty-speedbar-vc-icon "blank")))

  (pretty-speedbar-toggle-vc)

;;; manually using pretty-page works. Now change it to variables.

(defmacro pretty-speedbar-ezimage (this-name)
  "Macro for defezimage using the defcustom icon settings list passed as THIS-LIST."
  `(defezimage ,(intern (format "%s" this-name)) ((:type svg :file ,(expand-file-name (format "%s.svg" this-name) pretty-speedbar-icons-dir) :ascent center)) "Documentation string replace.")
  )

;; passing the name manually works. Pulling it from the list does not.
(pretty-speedbar-ezimage "pretty-lock")
(put 'pretty-lock 'function-documentation "Replacement for ezimage-lock, which is described as a read only or private.")
(pretty-speedbar-ezimage "pretty-chevron-down")
(pretty-speedbar-ezimage "pretty-chevron-right")
(pretty-speedbar-ezimage "pretty-tag")
(pretty-speedbar-ezimage "pretty-tags")
(pretty-speedbar-ezimage "pretty-info")
(pretty-speedbar-ezimage "pretty-lock")
(pretty-speedbar-ezimage "pretty-mail")
(pretty-speedbar-ezimage "pretty-book")
(pretty-speedbar-ezimage "pretty-box-closed")
(pretty-speedbar-ezimage "pretty-box-open")
(pretty-speedbar-ezimage "pretty-folder")
(pretty-speedbar-ezimage "pretty-folder-open")

(pretty-speedbar-ezimage "pretty-page")
(put 'pretty-page 'function-documentation "Replacement for ezimage-page, which is described as file with nothing interesting in it.")

(pretty-speedbar-ezimage "pretty-page-plus")
(pretty-speedbar-ezimage "pretty-page-minus")

(pretty-speedbar-ezimage "pretty-plus")
(pretty-speedbar-ezimage "pretty-minus")

;;;###autoload
(setq speedbar-expand-image-button-alist
      '(("<+>" . pretty-folder)
    ("<->" . pretty-folder-open)
    ("< >" . pretty-folder)
    ("[+]" . pretty-page-plus)
    ("[-]" . pretty-page-minus)
    ("[?]" . pretty-page)
    ("[ ]" . pretty-page)
    ("{+}" . pretty-box-closed)
    ("{-}" . pretty-box-open)
    ("<M>" . pretty-mail)
    ("<d>" . pretty-book)
    ("<i>" . ezimage-info-tag)
    (" =>" . ezimage-tag)
    (" +>" . ezimage-tag-gt)
    (" ->" . ezimage-tag-v)
    (">"   . ezimage-tag)
    ("@"   . ezimage-tag-type)
    ("  @" . ezimage-tag-type)
    ("*"   . pretty-check)
    ("#"   . ezimage-object)
    ("!"   . ezimage-object-out-of-date)
    ("//"  . ezimage-label)
    ("%"   . ezimage-lock)
    ))


(provide 'pretty-speedbar)
;;; pretty-speedbar.el ends here
