;;;; -*- no-byte-compile: t -*-
;;;; -*- lexical-binding: t -*-
;;;; blog.el -- Convenience functions for writing blog posts
;;;; Be sure to update this if the blog location or platform changes

(defun blog-directory ()
  "Return the top level directory for the blog"
  "~/Sites/JekyllBlog/")

(defun blog-post-directory ()
  "Return the directory for regular posts"
  (concat (blog-directory) "_posts/"))

(defun blog-post-date ()
  "Returns the formatted date for file name and posts"
  (format-time-string "%Y-%m-%d"))

(defun blog-post-date-time ()
  "Returns the formatted date and time for blog posts"
  (format-time-string "%Y-%m-%d %H:%M"))

(defun blog-post-file-name (title)
  "Return a file name with the date prepended to the TITLE of the post"
  (concat
   (blog-post-date)
   "-"
   (replace-regexp-in-string " " "-" (downcase title))
   ".markdown"))

(defun blog-new-post (title)
  "Create a new blog post with the given TITLE in the correct directory"
  (interactive "sTitle: ")
  (let ((file-name (blog-post-file-name title)))
    (set-buffer (get-buffer-create file-name))
    (markdown-mode)
    (insert (format
             "---\nlayout: post\ndate: %s\ntitle: %s\nintroduction: \ncategory:\n- \ntags:\n- \n---\n\n" (blog-post-date-time) title))
    (write-file (expand-file-name file-name (blog-post-directory)))
    (switch-to-buffer file-name)))

