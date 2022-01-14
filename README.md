# Pretty Speedbar for Emacs

![Pretty speedbar customized for a dark theme](pretty_speedbar_sample_dark.png)

Why turn off the ugly Emacs Speedbar icons when you can replace them with pretty SVG icons customized to match your theme?

## Installation

Pretty speedbar requires [f](https://github.com/rejeep/f.el), which installs via [Melpa](https://melpa.org/).

Download or clone `pretty-speedbar` and place it in the same directory as your init file or wherever you keep extensions.

Add the following to your init file.

```
;;;;;; Begin pretty-speedbar
(load "pretty-speedbar")

(setq pretty-speedbar-icon-size "20") ;; Icon width and height in pixels.
(setq pretty-speedbar-icon-fill "#FFFFFF") ;; Fill color for all non-folder icons.
(setq pretty-speedbar-icon-stroke "#DCDCDC") ;; Stroke color for all non-folder icons.
(setq pretty-speedbar-icon-folder-fill "#D9B3FF") ;; Fill color for all folder icons.
(setq pretty-speedbar-icon-folder-stroke "#CC00CC") ;; Stroke color for all folder icons.
```

Customize the values as desired. The above works well with the doom-one theme I'm currently addicted to.

If you want to take this a step further, customize your speedbar faces in your init file to match your icons.

```
;; Show unknown files and fix the indentation.
(setq speedbar-show-unknown-files t)
(setq speedbar-indentation-width 3)

;;; Customize text color
(custom-set-faces
'(speedbar-button-face ((t (:foreground "gray80"))))
'(speedbar-directory-face ((t (:foreground "gray60"))))
'(speedbar-file-face ((t (:foreground "gray80"))))
'(speedbar-highlight-face ((t (:background "slate blue" :foreground "gray98"))))
'(speedbar-selected-face ((t (:foreground "gray98" :underline t))))
'(speedbar-tag-face ((t (:foreground "gray80"))))
)
```

There is no need to set `(setq speedbar-use-images t)` explicitly. If you've previously used speedbar, please search your init file for `speedbar-use-images` and delete this setting if it's nil. (I  don't set it in my init file at all. The default setting works fine for my purposes.)

After saving your init file, it's time to create the custom icons.

1. Open Emacs.
2. `M-x pretty-speedbar-generate` (If you're unfamiliar with Emacs, M-x is shorthand for Alt + x .)

Now, restart Emacs and start using your pretty speedbar or sr-speedbar, which is my preference.

## Troubleshooting

1. Open one of the generated SVG icons in Emacs. Do you see an image? If not, your Emacs may not have SVG support enabled.
2. Copy the absolute path to pretty-speedbar-icons/page.SVG, edit the following, and paste it into your init file below `(load "pretty-speedbar")`.

```
(defezimage pretty-page ((:type svg :file [Absolute Path to page.svg] :ascent center)) "Image used for page.")
```

If you still don't see a page image, open a GitHub issue. Please let me know your Emacs version.

## FAQ

* Where can I find the original icons?
  * This uses solid SVGs from [FontAwesome](https://github.com/FortAwesome/Font-Awesome). Since Speedbar's ezimage macro requires static images, these files will be saved into the folder in your init directory.
* Does this download icon files?
  * No. This does not and will not download files.
* Can I create my own images?
  * Certainly! Create the *pretty-speedbar-icons* directory in the same folder as your init file. Then add your SVG files manually. You can find the required filenames in the .el file. Don't forget to resize the height and width, not the viewbox, and add fill and stroke colors.
* Will this work if I don't run the generate command?
  * If you create your own images manually, yes. Otherwise, your speedbar will show folders as <+>.
* Why didn't you use the built-in SVG.el?
  * Because resizing icons without editing the paths required height and width alongside a viewbox. It doesn't currently offer a viewbox option. If that changes, I'll revisit this.
* How do I start speedbar?
  * Open a file in Emacs. Then M-x speedbar. For sr-speedbar, which opens in a frame instead of a separate window, install it from Melpa and run M-x sr-speedbar-open.
