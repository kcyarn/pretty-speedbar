;;; pretty-speedbar.el --- Make speedbar pretty -*- lexical-binding: t -*-
;; Copyright: 2022 Kristle Chester
;; Author: Kristle Chester
;; Created: 2019-11-18
;; Version: 0.2
;; Last-Updated: 2022-01-13
;; URL: ADD!
;; Package-Requires: ((emacs "27.1"))
;; Keywords: speedbar, pretty speedbar, sr-speedbar
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

(require 'f)
(require 'speedbar)
(require 'svg)

(defgroup pretty-speedbar nil
  "Group for pretty-speedbar."
  :group 'pretty-speedbar
<<<<<<< Updated upstream
)
=======
  :prefix "pretty-speedbar-")

(defcustom pretty-speedbar-font "Font Awesome 5 Free Solid"
  "Set the default icon font To Font Awesome 5 Free Solid obtainable as an otf from github."
  :type '(string)
  :group 'pretty-speedbar)
>>>>>>> Stashed changes

(defcustom pretty-speedbar-icon-size "20"
  "Set default size in pixels and applies to both width and height."
  :type '(string)
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

(defcustom pretty-speedbar-icons-dir
  (expand-file-name (concat user-emacs-directory "pretty-speedbar-icons/"))
  "Store pretty-speedbar-icons in the pretty-speedbar-icons folder located in the user's default Emacs directory."
  :group 'pretty-speedbar)

<<<<<<< Updated upstream
(defun pretty-speedbar-make-dir ()
  "Create the pretty-speedbar-icons directory."
  (unless (file-exists-p pretty-speedbar-icons-dir)
    (make-directory pretty-speedbar-icons-dir t)))

;;; blank to remove the uc checkmarks

(defcustom pretty-speedbar-blank-icon
  (format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\" width=\"1\" height=\"1\"><!-- Font Awesome Free 5.15.4 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free (Icons: CC BY 4.0, Fonts: SIL OFL 1.1, Code: MIT License) --></svg>")
  "Blank icon to overwrite the uc checkmarks."
  :group 'pretty-speedbar
)
=======
(defcustom pretty-speedbar-checkmark nil
  "Turn the checkmark for files check out of a VC on or off.
Defaults to off."
  :group 'pretty-speedbar)
>>>>>>> Stashed changes

;;; Match the icon names to the unicode, but keep this configurable by the user.

(defcustom test-icon '("this-test-icon" "\uf15c" nil)
  "Test icon follows the defezimage name, unicode symbol, and is folder pattern."
  :group 'pretty-speedbar
  )

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

<<<<<<< Updated upstream
  (setq pretty-speedbar-lock-icon
    (format "%sM400 224h-24v-72C376 68.2 307.8 0 224 0S72 68.2 72 152v72H48c-26.5 0-48 21.5-48 48v192c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V272c0-26.5-21.5-48-48-48zm-104 0H152v-72c0-39.7 32.3-72 72-72s72 32.3 72 72v72z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))
  
)

(defun pretty-speedbar-write-icon (this_icon this_icon_file)
  "Argument THIS_ICON is the svg text as a string, which this function writes to THIS_ICON_FILE."
  (let ((this_file_path (expand-file-name this_icon_file pretty-speedbar-icons-dir)))
    (f-write-text this_icon 'utf-8 this_file_path)
  )
)
=======
(defun pretty-speedbar-svg (this-list)
  "Function to create individual icon svgs with THIS-LIST being the individual icon's variable."
 
  ;; each named icon gets a defcustom list that's editable by the icon. (setq pretty-folder-icon '(pretty-folder \uf07b t))

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
  ;;; Although using these from :data instead of :file is theoretically possible, it lags badly. Save the files.
  ))

;;; complete generator combines all variables with pretty-test-icon function

(defun pretty-speedbar-make-dir ()
  "Create the pretty-speedbar-icons directory."
  (unless (file-exists-p pretty-speedbar-icons-dir)
    (make-directory pretty-speedbar-icons-dir t)))
>>>>>>> Stashed changes

(defun pretty-speedbar-generate()
  (interactive)
  (pretty-speedbar-make-dir)
<<<<<<< Updated upstream
  ;;; Function that sets svg using updated variables.
  (pretty-speedbar-folder-run)
  ;; Update svg files with latest variables.
  (pretty-speedbar-write-icon pretty-speedbar-blank-icon "blank.svg")
  (pretty-speedbar-write-icon pretty-speedbar-folder-icon "folder.svg")
  (pretty-speedbar-write-icon pretty-speedbar-folder-open-icon "folder-open.svg")
  (pretty-speedbar-write-icon pretty-speedbar-page-plus-icon "plus.svg")
  (pretty-speedbar-write-icon pretty-speedbar-page-minus-icon "minus.svg")
  (pretty-speedbar-write-icon pretty-speedbar-page-icon "page.svg")
  (pretty-speedbar-write-icon pretty-speedbar-box-open-icon "box-open.svg")
  (pretty-speedbar-write-icon pretty-speedbar-box-closed-icon "box-closed.svg")
  (pretty-speedbar-write-icon pretty-speedbar-book-icon "book.svg")
  (pretty-speedbar-write-icon pretty-speedbar-mail-icon "mail.svg")
  (pretty-speedbar-write-icon pretty-speedbar-info-icon "info.svg")
  (pretty-speedbar-write-icon pretty-speedbar-tag-icon "tag.svg")
  (pretty-speedbar-write-icon pretty-speedbar-tags-icon "tags.svg")
  (pretty-speedbar-write-icon pretty-speedbar-chevron-right-icon "chevron-right.svg")
  (pretty-speedbar-write-icon pretty-speedbar-chevron-down-icon "chevron-down.svg")
  (pretty-speedbar-write-icon pretty-speedbar-lock-icon "lock.svg")
)

(defmacro pretty-speedbar-lock ()
  "Lock image from FontAwesome."
  `(defezimage pretty-lock ((:type svg :file ,(expand-file-name "lock.svg" pretty-speedbar-icons-dir) :ascent center)) "Lock image from FontAwesome.")
  )
(pretty-speedbar-lock)

(defmacro pretty-speedbar-chevron-down ()
  "Round chevron down from Fontawesome."
  `(defezimage pretty-chevron-down ((:type svg :file ,(expand-file-name "chevron-down.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used to replace tag-v.")
  )
(pretty-speedbar-chevron-down)

(defmacro pretty-speedbar-chevron-right ()
  "Round right chevron image from Fontawesome."
  `(defezimage pretty-chevron-right ((:type svg :file ,(expand-file-name "chevron-right.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used to replace greater than tag.")
  )
(pretty-speedbar-chevron-right)

(defmacro pretty-speedbar-tag ()
  "Tag from Fontawesome."
  `(defezimage pretty-tag ((:type svg :file ,(expand-file-name "tag.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for basic tag.")
  )
(pretty-speedbar-tag)

(defmacro pretty-speedbar-tags ()
  "Tags (plural) from Fontawesome."
  `(defezimage pretty-tags ((:type svg :file ,(expand-file-name "tags.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for basic tags.")
  )
(pretty-speedbar-tags)

(defmacro pretty-speedbar-info ()
  "Round i from Fontawesome."
  `(defezimage pretty-info ((:type svg :file ,(expand-file-name "info.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for info tag and data type tag.")
  )
(pretty-speedbar-info)

(defmacro pretty-speedbar-mail ()
  "Envelope icon from Fontawesome."
  `(defezimage pretty-mail ((:type svg :file ,(expand-file-name "mail.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for mail.")
  )
(pretty-speedbar-mail)

(defmacro pretty-speedbar-book ()
  "Book from Fontawesome."
  `(defezimage pretty-book ((:type svg :file ,(expand-file-name "book.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for documentation available.")
  )
(pretty-speedbar-book)

(defmacro pretty-speedbar-box-closed ()
  "Box from Fontawesome."
  `(defezimage pretty-box-closed ((:type svg :file ,(expand-file-name "box-closed.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for box-closed.")
  )
(pretty-speedbar-box-closed)

(defmacro pretty-speedbar-box-open ()
  "Box open from Fontawesome."
  `(defezimage pretty-box-open ((:type svg :file ,(expand-file-name "box-open.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for box-open.")
  )
(pretty-speedbar-box-open)
=======
  (pretty-speedbar-svg test-icon)
  )

;;; blank to remove the uc checkmarks

(defcustom pretty-speedbar-blank-icon
  (format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\" width=\"1\" height=\"1\"></svg>")
  "Blank icon to overwrite the uc checkmarks."
  :group 'pretty-speedbar)
>>>>>>> Stashed changes


;;; should be toggled by the setq
(defmacro pretty-speedbar-blank ()
  "A blank svg image."
  `(defezimage pretty-blank ((:type svg :file ,(expand-file-name "blank.svg" pretty-speedbar-icons-dir) :ascent center)) "A blank svg image.")
  )
(pretty-speedbar-blank)

<<<<<<< Updated upstream
(defmacro pretty-speedbar-folder ()
  "Folder from Fontawesome."
  `(defezimage pretty-folder ((:type svg :file ,(expand-file-name "folder.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for closed directories .")
  )
(pretty-speedbar-folder)

(defmacro pretty-speedbar-folder-open ()
  "Folder open from Fontawesome."
  `(defezimage pretty-folder-open ((:type svg :file ,(expand-file-name "folder-open.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for folder-open.")
  )
(pretty-speedbar-folder-open)

(defmacro pretty-speedbar-page ()
  "File-alt from Fontawesome."
  `(defezimage pretty-page ((:type svg :file ,(expand-file-name "page.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for page.")
  )
(pretty-speedbar-page)

(defmacro pretty-speedbar-plus ()
  "Plus in a square from Fontawesome."
  `(defezimage pretty-plus ((:type svg :file ,(expand-file-name "plus.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for plus.")
  )
(pretty-speedbar-plus)

(defmacro pretty-speedbar-minus ()
  "Minus in a square from Fontawesome."
  `(defezimage pretty-minus ((:type svg :file ,(expand-file-name "minus.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for minus.")
  )
(pretty-speedbar-minus)

=======
>>>>>>> Stashed changes
;;;###autoload

(setq speedbar-expand-image-button-alist
    '(
    ("<+>" . pretty-folder)
    ("<->" . pretty-folder-open)
    ("< >" . pretty-folder)
    ("[+]" . pretty-plus)
    ("[-]" . pretty-minus)
    ("[?]" . pretty-page)
    ("[ ]" . pretty-page)
    ("{+}" . pretty-plus)
    ("{-}" . pretty-minus)
    ("<M>" . pretty-mail)
    ("<d>" . pretty-book)
    ("<i>" . pretty-info)
    (" =>" . pretty-tag)
    (" +>" . pretty-chevron-right)
    (" ->" . pretty-chevron-down)
    (">"   . pretty-tag)
    ("@"   . pretty-info)
    ("  @" . pretty-info)
    ("*"   . pretty-blank)
    ("%"   . pretty-lock)
    ))

(provide 'pretty-speedbar)
;;; pretty-speedbar.el ends here
