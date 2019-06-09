;; Run this file to create a theme for kitty based on the current Xresources colors.

(require 'cl-lib)
(require 's)

(let ((theme-file "theme.conf")
      (resource-alist (mapcar
                       (lambda (line)
                         (cl-destructuring-bind (key value) (s-split ":" line t)
                           (cons (s-trim key) (s-trim value))))
                       (s-lines (s-trim (shell-command-to-string "xrdb -query"))))))
  (cl-flet ((get-x-resource (key) (alist-get key resource-alist nil nil #'string-equal)))
    (with-temp-buffer
      (insert (format "# automatically generated from Xresources

# cursor
cursor            %s
cursor_text_color background

# colors
foreground           %s
background           %s
selection_foreground %s
selection_background %s
# black
color0  %s
color8  %s
# red
color1  %s
color9  %s
# green
color2  %s
color10 %s
# yellow
color3  %s
color11 %s
# blue
color4  %s
color12 %s
# magenta
color5  %s
color13 %s
# cyan
color6  %s
color14 %s
# white
color7  %s
color15 %s
"
                      (get-x-resource "*cursorColor")
                      (get-x-resource "*foreground")
                      (get-x-resource "*background")
                      (get-x-resource "*background")
                      (get-x-resource "*foreground")
                      (get-x-resource "*color0")
                      (get-x-resource "*color8")
                      (get-x-resource "*color1")
                      (get-x-resource "*color9")
                      (get-x-resource "*color2")
                      (get-x-resource "*color10")
                      (get-x-resource "*color3")
                      (get-x-resource "*color11")
                      (get-x-resource "*color4")
                      (get-x-resource "*color12")
                      (get-x-resource "*color5")
                      (get-x-resource "*color13")
                      (get-x-resource "*color6")
                      (get-x-resource "*color14")
                      (get-x-resource "*color7")
                      (get-x-resource "*color15")))
      (write-region (point-min) (point-max) theme-file))))
