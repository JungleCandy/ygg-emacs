### [Emacs](https://www.gnu.org/software/emacs/)

#### Activating theme

1. Add the Emacs theme variant file to `~/.emacs.d/themes`.
2. Then load your chosen variant by adding the following to your `init.el`:

   ```bash
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
   (load-theme 'variant t)
   ```
