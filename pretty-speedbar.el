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

(require 'f)
(require 'speedbar)

(defgroup pretty-speedbar nil
  "Group for pretty-speedbar."
  :group 'pretty-speedbar)

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
  :type '(string)
  :group 'pretty-speedbar)

(defun pretty-speedbar-make-dir ()
  "Create the pretty-speedbar-icons directory."
  (unless (file-exists-p pretty-speedbar-icons-dir)
    (make-directory pretty-speedbar-icons-dir t)))

;;; blank to remove the uc checkmarks

(defcustom pretty-speedbar-blank-icon
  (format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\" width=\"1\" height=\"1\"></svg>")
  "Blank icon to overwrite the uc checkmarks."
  :group 'pretty-speedbar)

(defun pretty-speedbar-folder-run ()
  "Build all SVGs as strings."
  (setq pretty-speedbar-svg-head (format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\" width=\"%s\" height=\"%s\"><!-- Font Awesome Free 5.15.4 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free (Icons: CC BY 4.0, Fonts: SIL OFL 1.1, Code: MIT License) --><path d=\"" pretty-speedbar-icon-size pretty-speedbar-icon-size))

  (setq pretty-speedbar-svg-foot (format "\" style=\"fill:%s;stroke:%s;stroke-width:5;stroke-miterlimit:4;\"/></svg>" pretty-speedbar-icon-fill pretty-speedbar-icon-stroke))

  (setq pretty-speedbar-svg-foot-folder (format "\" style=\"fill:%s;stroke:%s;stroke-width:5;stroke-miterlimit:4;\"/></svg>" pretty-speedbar-icon-folder-fill pretty-speedbar-icon-folder-stroke))

  ;;; Insert the path value from the invidiual Font Awesome svg. Look for <path d="[path value here]". And add %s to the beginning and the end. />

  (setq pretty-speedbar-folder-icon (format "%sM464 128H272l-64-64H48C21.49 64 0 85.49 0 112v288c0 26.51 21.49 48 48 48h416c26.51 0 48-21.49 48-48V176c0-26.51-21.49-48-48-48z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot-folder))

  (setq pretty-speedbar-folder-open-icon
    (format "%sM572.694 292.093L500.27 416.248A63.997 63.997 0 0 1 444.989 448H45.025c-18.523 0-30.064-20.093-20.731-36.093l72.424-124.155A64 64 0 0 1 152 256h399.964c18.523 0 30.064 20.093 20.73 36.093zM152 224h328v-48c0-26.51-21.49-48-48-48H272l-64-64H48C21.49 64 0 85.49 0 112v278.046l69.077-118.418C86.214 242.25 117.989 224 152 224z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot-folder))

  (setq pretty-speedbar-page-plus-icon
    (format "%sM400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zm-32 252c0 6.6-5.4 12-12 12h-92v92c0 6.6-5.4 12-12 12h-56c-6.6 0-12-5.4-12-12v-92H92c-6.6 0-12-5.4-12-12v-56c0-6.6 5.4-12 12-12h92v-92c0-6.6 5.4-12 12-12h56c6.6 0 12 5.4 12 12v92h92c6.6 0 12 5.4 12 12v56z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-page-minus-icon
    (format "%sM400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zM92 296c-6.6 0-12-5.4-12-12v-56c0-6.6 5.4-12 12-12h264c6.6 0 12 5.4 12 12v56c0 6.6-5.4 12-12 12H92z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-page-icon
    (format "%sM224 136V0H24C10.7 0 0 10.7 0 24v464c0 13.3 10.7 24 24 24h336c13.3 0 24-10.7 24-24V160H248c-13.2 0-24-10.8-24-24zm64 236c0 6.6-5.4 12-12 12H108c-6.6 0-12-5.4-12-12v-8c0-6.6 5.4-12 12-12h168c6.6 0 12 5.4 12 12v8zm0-64c0 6.6-5.4 12-12 12H108c-6.6 0-12-5.4-12-12v-8c0-6.6 5.4-12 12-12h168c6.6 0 12 5.4 12 12v8zm0-72v8c0 6.6-5.4 12-12 12H108c-6.6 0-12-5.4-12-12v-8c0-6.6 5.4-12 12-12h168c6.6 0 12 5.4 12 12zm96-114.1v6.1H256V0h6.1c6.4 0 12.5 2.5 17 7l97.9 98c4.5 4.5 7 10.6 7 16.9z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-box-open-icon
    (format "%sM425.7 256c-16.9 0-32.8-9-41.4-23.4L320 126l-64.2 106.6c-8.7 14.5-24.6 23.5-41.5 23.5-4.5 0-9-.6-13.3-1.9L64 215v178c0 14.7 10 27.5 24.2 31l216.2 54.1c10.2 2.5 20.9 2.5 31 0L551.8 424c14.2-3.6 24.2-16.4 24.2-31V215l-137 39.1c-4.3 1.3-8.8 1.9-13.3 1.9zm212.6-112.2L586.8 41c-3.1-6.2-9.8-9.8-16.7-8.9L320 64l91.7 152.1c3.8 6.3 11.4 9.3 18.5 7.3l197.9-56.5c9.9-2.9 14.7-13.9 10.2-23.1zM53.2 41L1.7 143.8c-4.6 9.2.3 20.2 10.1 23l197.9 56.5c7.1 2 14.7-1 18.5-7.3L320 64 69.8 32.1c-6.9-.8-13.5 2.7-16.6 8.9z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-box-closed-icon
    (format "%sM509.5 184.6L458.9 32.8C452.4 13.2 434.1 0 413.4 0H272v192h238.7c-.4-2.5-.4-5-1.2-7.4zM240 0H98.6c-20.7 0-39 13.2-45.5 32.8L2.5 184.6c-.8 2.4-.8 4.9-1.2 7.4H240V0zM0 224v240c0 26.5 21.5 48 48 48h416c26.5 0 48-21.5 48-48V224H0z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-book-icon
    (format "%sM448 360V24c0-13.3-10.7-24-24-24H96C43 0 0 43 0 96v320c0 53 43 96 96 96h328c13.3 0 24-10.7 24-24v-16c0-7.5-3.5-14.3-8.9-18.7-4.2-15.4-4.2-59.3 0-74.7 5.4-4.3 8.9-11.1 8.9-18.6zM128 134c0-3.3 2.7-6 6-6h212c3.3 0 6 2.7 6 6v20c0 3.3-2.7 6-6 6H134c-3.3 0-6-2.7-6-6v-20zm0 64c0-3.3 2.7-6 6-6h212c3.3 0 6 2.7 6 6v20c0 3.3-2.7 6-6 6H134c-3.3 0-6-2.7-6-6v-20zm253.4 250H96c-17.7 0-32-14.3-32-32 0-17.6 14.4-32 32-32h285.4c-1.9 17.1-1.9 46.9 0 64z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-mail-icon
    (format "%sM502.3 190.8c3.9-3.1 9.7-.2 9.7 4.7V400c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V195.6c0-5 5.7-7.8 9.7-4.7 22.4 17.4 52.1 39.5 154.1 113.6 21.1 15.4 56.7 47.8 92.2 47.6 35.7.3 72-32.8 92.3-47.6 102-74.1 131.6-96.3 154-113.7zM256 320c23.2.4 56.6-29.2 73.4-41.4 132.7-96.3 142.8-104.7 173.4-128.7 5.8-4.5 9.2-11.5 9.2-18.9v-19c0-26.5-21.5-48-48-48H48C21.5 64 0 85.5 0 112v19c0 7.4 3.4 14.3 9.2 18.9 30.6 23.9 40.7 32.4 173.4 128.7 16.8 12.2 50.2 41.8 73.4 41.4z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-info-icon
    (format "%sM256 8C119.043 8 8 119.083 8 256c0 136.997 111.043 248 248 248s248-111.003 248-248C504 119.083 392.957 8 256 8zm0 110c23.196 0 42 18.804 42 42s-18.804 42-42 42-42-18.804-42-42 18.804-42 42-42zm56 254c0 6.627-5.373 12-12 12h-88c-6.627 0-12-5.373-12-12v-24c0-6.627 5.373-12 12-12h12v-64h-12c-6.627 0-12-5.373-12-12v-24c0-6.627 5.373-12 12-12h64c6.627 0 12 5.373 12 12v100h12c6.627 0 12 5.373 12 12v24z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-tag-icon
    (format "%sM0 252.118V48C0 21.49 21.49 0 48 0h204.118a48 48 0 0 1 33.941 14.059l211.882 211.882c18.745 18.745 18.745 49.137 0 67.882L293.823 497.941c-18.745 18.745-49.137 18.745-67.882 0L14.059 286.059A48 48 0 0 1 0 252.118zM112 64c-26.51 0-48 21.49-48 48s21.49 48 48 48 48-21.49 48-48-21.49-48-48-48z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-tags-icon
    (format "%sM497.941 225.941L286.059 14.059A48 48 0 0 0 252.118 0H48C21.49 0 0 21.49 0 48v204.118a48 48 0 0 0 14.059 33.941l211.882 211.882c18.744 18.745 49.136 18.746 67.882 0l204.118-204.118c18.745-18.745 18.745-49.137 0-67.882zM112 160c-26.51 0-48-21.49-48-48s21.49-48 48-48 48 21.49 48 48-21.49 48-48 48zm513.941 133.823L421.823 497.941c-18.745 18.745-49.137 18.745-67.882 0l-.36-.36L527.64 323.522c16.999-16.999 26.36-39.6 26.36-63.64s-9.362-46.641-26.36-63.64L331.397 0h48.721a48 48 0 0 1 33.941 14.059l211.882 211.882c18.745 18.745 18.745 49.137 0 67.882z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-chevron-right-icon
    (format "%sM256 8c137 0 248 111 248 248S393 504 256 504 8 393 8 256 119 8 256 8zm113.9 231L234.4 103.5c-9.4-9.4-24.6-9.4-33.9 0l-17 17c-9.4 9.4-9.4 24.6 0 33.9L285.1 256 183.5 357.6c-9.4 9.4-9.4 24.6 0 33.9l17 17c9.4 9.4 24.6 9.4 33.9 0L369.9 273c9.4-9.4 9.4-24.6 0-34z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-chevron-down-icon
    (format "%sM504 256c0 137-111 248-248 248S8 393 8 256 119 8 256 8s248 111 248 248zM273 369.9l135.5-135.5c9.4-9.4 9.4-24.6 0-33.9l-17-17c-9.4-9.4-24.6-9.4-33.9 0L256 285.1 154.4 183.5c-9.4-9.4-24.6-9.4-33.9 0l-17 17c-9.4 9.4-9.4 24.6 0 33.9L239 369.9c9.4 9.4 24.6 9.4 34 0z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot))

  (setq pretty-speedbar-lock-icon
    (format "%sM400 224h-24v-72C376 68.2 307.8 0 224 0S72 68.2 72 152v72H48c-26.5 0-48 21.5-48 48v192c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V272c0-26.5-21.5-48-48-48zm-104 0H152v-72c0-39.7 32.3-72 72-72s72 32.3 72 72v72z%s" pretty-speedbar-svg-head pretty-speedbar-svg-foot)))

(defun pretty-speedbar-write-icon (this_icon this_icon_file)
  "Argument THIS_ICON is the svg text as a string, which this function writes to THIS_ICON_FILE."
  (let ((this_file_path (expand-file-name this_icon_file pretty-speedbar-icons-dir)))
    (f-write-text this_icon 'utf-8 this_file_path)))

(defun pretty-speedbar-generate ()
  "Generate the icon svg files."
  (interactive)
  (pretty-speedbar-make-dir)
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
  (pretty-speedbar-write-icon pretty-speedbar-lock-icon "lock.svg"))

(defmacro pretty-speedbar-lock ()
  "Lock image from FontAwesome."
  `(defezimage pretty-lock ((:type svg :file ,(expand-file-name "lock.svg" pretty-speedbar-icons-dir) :ascent center)) "Lock image from FontAwesome."))
(pretty-speedbar-lock)

(defmacro pretty-speedbar-chevron-down ()
  "Round chevron down from Fontawesome."
  `(defezimage pretty-chevron-down ((:type svg :file ,(expand-file-name "chevron-down.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used to replace tag-v."))
(pretty-speedbar-chevron-down)

(defmacro pretty-speedbar-chevron-right ()
  "Round right chevron image from Fontawesome."
  `(defezimage pretty-chevron-right ((:type svg :file ,(expand-file-name "chevron-right.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used to replace greater than tag."))
(pretty-speedbar-chevron-right)

(defmacro pretty-speedbar-tag ()
  "Tag from Fontawesome."
  `(defezimage pretty-tag ((:type svg :file ,(expand-file-name "tag.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for basic tag."))
(pretty-speedbar-tag)

(defmacro pretty-speedbar-tags ()
  "Tags (plural) from Fontawesome."
  `(defezimage pretty-tags ((:type svg :file ,(expand-file-name "tags.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for basic tags."))
(pretty-speedbar-tags)

(defmacro pretty-speedbar-info ()
  "Round i from Fontawesome."
  `(defezimage pretty-info ((:type svg :file ,(expand-file-name "info.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for info tag and data type tag."))
(pretty-speedbar-info)

(defmacro pretty-speedbar-mail ()
  "Envelope icon from Fontawesome."
  `(defezimage pretty-mail ((:type svg :file ,(expand-file-name "mail.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for mail."))
(pretty-speedbar-mail)

(defmacro pretty-speedbar-book ()
  "Book from Fontawesome."
  `(defezimage pretty-book ((:type svg :file ,(expand-file-name "book.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for documentation available."))
(pretty-speedbar-book)

(defmacro pretty-speedbar-box-closed ()
  "Box from Fontawesome."
  `(defezimage pretty-box-closed ((:type svg :file ,(expand-file-name "box-closed.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for box-closed."))
(pretty-speedbar-box-closed)

(defmacro pretty-speedbar-box-open ()
  "Box open from Fontawesome."
  `(defezimage pretty-box-open ((:type svg :file ,(expand-file-name "box-open.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for box-open."))
(pretty-speedbar-box-open)

(defmacro pretty-speedbar-blank ()
  "A blank svg image."
  `(defezimage pretty-blank ((:type svg :file ,(expand-file-name "blank.svg" pretty-speedbar-icons-dir) :ascent center)) "A blank svg image."))
(pretty-speedbar-blank)

(defmacro pretty-speedbar-folder ()
  "Folder from Fontawesome."
  `(defezimage pretty-folder ((:type svg :file ,(expand-file-name "folder.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for closed directories ."))
(pretty-speedbar-folder)

(defmacro pretty-speedbar-folder-open ()
  "Folder open from Fontawesome."
  `(defezimage pretty-folder-open ((:type svg :file ,(expand-file-name "folder-open.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for folder-open."))
(pretty-speedbar-folder-open)

(defmacro pretty-speedbar-page ()
  "File-alt from Fontawesome."
  `(defezimage pretty-page ((:type svg :file ,(expand-file-name "page.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for page."))
(pretty-speedbar-page)

(defmacro pretty-speedbar-plus ()
  "Plus in a square from Fontawesome."
  `(defezimage pretty-plus ((:type svg :file ,(expand-file-name "plus.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for plus."))
(pretty-speedbar-plus)

(defmacro pretty-speedbar-minus ()
  "Minus in a square from Fontawesome."
  `(defezimage pretty-minus ((:type svg :file ,(expand-file-name "minus.svg" pretty-speedbar-icons-dir) :ascent center)) "Image used for minus."))
(pretty-speedbar-minus)

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
    ("%"   . pretty-lock)))

(provide 'pretty-speedbar)
;;; pretty-speedbar.el ends here
