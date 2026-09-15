;;; vui-html.el --- Render HTML using vui.el framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; URL: https://github.com/ginqi7/vui-html
;; Keywords: hypermedia
;; Package-Requires: ((emacs "29.1") (vui "0.1") (ekp "0.1") (vui+ "0.0.1"))
;; Version: 0.0.1

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
;; Provide rendering of HTML using the vui.el framework.

;;; Code:

(require 'vui-components)
(require 'vui)
(require 'vui+)
(require 'seq)
(require 'ekp)

(defgroup vui-html ()
  "Render HTML using vui.el framework."
  :group 'vui-html
  :prefix "vui-html-")

(defcustom vui-html-buffer-name "*vui-html*"
  "Default buffer name for rendering HTML."
  :type 'string
  :group 'vui-html)

(defcustom vui-html-max-width 80
  "Maximum character width for rendering HTML content."
  :type 'int
  :group 'vui-html)

;;; Utilities

(defun vui-html--parse-html (html-string)
  "Parse HTML-STRING using Emacs libxml parser.
Returns a parse tree suitable for translation."
  (with-temp-buffer
    (insert html-string)
    (libxml-parse-html-region (point-min) (point-max))))

(defun vui-html--node-tag (node)
  "Get the tag name from a parsed HTML NODE.
Returns the symbol (e.g., div, p) if NODE is a cons cell, nil otherwise."
  (when (consp node)
    (car node)))

(defun vui-html--node-attrs (node)
  "Get the attribute alist from a parsed HTML NODE.
Returns an association list of attributes if NODE is a cons cell, nil otherwise."
  (when (consp node)
    (cadr node)))

(defun vui-html--node-children (node)
  "Get the children of NODE as a list."
  (when (consp node)
    (cddr node)))

;;; HTML to VUI Translation

(defun vui-html--translate-children (children)
  "Translate list of CHILDREN nodes to vui components.
Filters out nil results."
  (cl-remove-if (lambda (item) (and (stringp item) (string-blank-p item)))
                (seq-filter #'identity (mapcar #'vui-html--translate-node children))))

(defun vui-html--compose-children (children &optional compose-func)
  "Compose a list of CHILDREN into a single vui component.
If CHILDREN has only one element, returns it directly.
Otherwise, applies COMPOSE-FUNC (default: `vui-vstack') to combine them."
  (if (= 1 (length children))
      (car children)
    (apply (or compose-func #'vui-vstack) children)))

(defun vui-html--translate-node (node)
  "Translate single HTML NODE to vui component."
  (let* ((tag (vui-html--node-tag node))
         (attrs (vui-html--node-attrs node))
         (children (vui-html--node-children node))
         (translated-children (vui-html--translate-children children)))
    (if tag
        (vui-html--translate-element tag attrs translated-children)
      (string-trim node))))

(defun vui-html--translate-element (tag attrs children)
  "Translate HTML element with TAG, ATTRS and CHILDREN to vui component."
  (cond
   ;; Block-level elements
   ((eq tag 'html) (vui-html--compose-children children))
   ((eq tag 'body) (vui-html--compose-children children))
   ((eq tag 'div) (vui-html--compose-children children))
   ((eq tag 'p) (vui+-kp-text (vui-html--compose-children children #'vui-hstack) vui-html-max-width))
   ((eq tag 'span) (vui-html--compose-children children))
   ((eq tag 'br) (vui-newline))
   ((eq tag 'hr) (vui-text (make-string vui-html-max-width ?─)))

   ;; Headings
   ((memq tag '(h1 h2 h3 h4 h5 h6 h7 h8))
    (let* ((level (string-to-number (substring (symbol-name tag) 1))))
      (vui-heading (vui-html--compose-children children) :level level)))

   ;; Text formatting
   ((memq tag '(b strong)) (vui-strong (vui-html--compose-children children)))
   ((eq tag 'em) (vui-text (vui-html--compose-children children)))
   ((eq tag 'code) (vui-code (vui-html--compose-children children)))

   ((eq tag 'pre)
    (vui+-source-code (vui-html--compose-children children) "emacs-lisp"))

   ((eq tag 'blockquote) (vui-html--compose-children children))
   ;; Lists
   ((memq tag '(ul))
    (vui-list children (lambda (item) (vui-hstack "•" item))))

   ((memq tag '(ol))
    (vui-list children
              (lambda (item)
                (vui-hstack
                 (format "%s." (1+ (cl-position item children))) item))))

   ((eq tag 'li)
    (vui+-kp-text (vui-html--compose-children children #'vui-hstack) vui-html-max-width))

   ;; Links
   ((eq tag 'a)
    (let ((href (alist-get 'href attrs)))
      (print (vui-html--compose-children children))
      `(vui-button ,(vui-html--compose-children children)
        :on-click (lambda () (browse-url ,href)))))

   ;; Unknown elements - treat as container
   (t
    (apply #'vui-vstack children))))

;;; Main API

(defun vui-html (html)
  "Parse HTML string and return a vui component tree.
HTML is a string of HTML markup.  Returns a vui component suitable
for rendering via `vui-mount'."
  (let* ((parse-tree (vui-html--parse-html html))
         (vui-node (vui-html--translate-node parse-tree)))
    vui-node))

(vui-defcomponent vui-html (node) :render node)

(defun vui-html-mount (&optional html)
  "Render HTML string in BUFFER (default: `vui-html-buffer-name').
Returns the buffer if successful, nil otherwise."
  (interactive)
  (unless html
    (setq html (buffer-string)))
  (vui-mount
   (vui-component 'vui-html :node (vui-html html))
   vui-html-buffer-name))

(provide 'vui-html)
;;; vui-html.el ends here
