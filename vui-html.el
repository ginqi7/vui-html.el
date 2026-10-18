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
;;
;; The translation works in two passes:
;;
;; - Block-level content is translated into vui vnodes.  Consecutive
;;   inline nodes are merged into a single propertized string, so that
;;   HTML whitespace collapses the way a browser would collapse it.
;; - Inline content is translated into a propertized string, layering
;;   faces for nested markup (`<b>', `<i>', `<code>', ...).

;;; Code:

(require 'vui-components)
(require 'vui)
(require 'vui+)
(require 'seq)
(require 'subr-x)
(require 'ekp)

(defgroup vui-html nil
  "Render HTML using vui.el framework."
  :group 'text
  :prefix "vui-html-")

(defcustom vui-html-buffer-name "*vui-html*"
  "Default buffer name for rendering HTML."
  :type 'string
  :group 'vui-html)

(defcustom vui-html-max-width 80
  "Maximum character width for rendering HTML content."
  :type 'integer
  :group 'vui-html)

(defcustom vui-html-table-border :unicode
  "Border style used when rendering `<table>' elements.
The value is passed to `vui-table' as its :border argument."
  :type '(choice (const :tag "Unicode" :unicode)
                 (const :tag "ASCII" :ascii)
                 (const :tag "None" nil))
  :group 'vui-html)

(defcustom vui-html-pre-language nil
  "Default language for `<pre>' blocks without an explicit language.
When nil, `vui+-source-code' tries to detect the language itself."
  :type '(choice (const :tag "Auto-detect" nil) string)
  :group 'vui-html)

(defcustom vui-html-browse-url #'browse-url
  ""
  :type 'function
  :group 'vui-html)

;;; Tag classification

(defconst vui-html--ignored-tags
  '(base col colgroup comment datalist head link map area meta noscript
    optgroup option param script source style template title track)
  "HTML tags that produce no renderable content.")

(defconst vui-html--inline-tags
  '(a abbr b bdi bdo big br cite code data del dfn em font i ins kbd label
    mark q s samp small span strike strong sub sup time tt u var wbr)
  "HTML tags that participate in inline text flow.")

(defconst vui-html--inline-or-ignored-tags
  (append vui-html--inline-tags vui-html--ignored-tags)
  "Inline tags plus ignored tags, which do not break an inline run.")

;;; Utilities

(defun vui-html--parse-html (html-string)
  "Parse HTML-STRING using Emacs libxml parser.
Returns a parse tree suitable for translation."
  (with-temp-buffer
    (insert html-string)
    (libxml-parse-html-region (point-min) (point-max))))

(defun vui-html--attr-p (attrs name)
  "Return non-nil when boolean attribute NAME is present in ATTRS."
  (and (assq name attrs) t))

(defun vui-html--normalize-text (text)
  "Collapse runs of whitespace in TEXT to a single space."
  (replace-regexp-in-string "[ \t\r\n\f]+" " " text))

(defun vui-html--collapse-whitespace (string)
  "Collapse redundant whitespace in STRING, preserving newlines.
Leading and trailing whitespace is removed."
  (let ((result (replace-regexp-in-string "[ \t]+" " " string)))
    (setq result (replace-regexp-in-string "[ \t]*\n[ \t]*" "\n" result))
    (string-trim result)))

(defun vui-html--raw-text (nodes)
  "Concatenate NODES into a string, preserving whitespace."
  (mapconcat
   (lambda (node)
     (cond
      ((stringp node) node)
      ((eq (car-safe node) 'br) "\n")
      ((memq (car-safe node) vui-html--ignored-tags) "")
      (t (vui-html--raw-text (cddr node)))))
   nodes ""))

;;; Faces

(defun vui-html--face-list (face)
  "Return FACE as a list of face specifications."
  (cond
   ((null face) nil)
   ;; An anonymous face spec such as (:underline t) is itself a list,
   ;; but must be treated as one element of the face list.
   ((and (listp face) (keywordp (car face))) (list face))
   ((listp face) face)
   (t (list face))))

(defun vui-html--merge-faces (parent child)
  "Merge CHILD faces into PARENT faces, dropping duplicates."
  (seq-uniq (append (vui-html--face-list parent)
                    (vui-html--face-list child))))

(defun vui-html--tag-face (tag attrs)
  "Return the face specification contributed by TAG with ATTRS."
  (pcase tag
    ((or 'b 'strong) '(vui-strong))
    ((or 'i 'em 'cite 'dfn 'var) '(vui-italic))
    ((or 'u 'ins) '((:underline t)))
    ((or 's 'strike 'del) '((:strike-through t)))
    ('mark '((:inherit highlight)))
    ('small '((:height 0.8)))
    ('big '((:height 1.2)))
    ((or 'code 'kbd 'samp 'tt) '(vui-code))
    ((or 'sub 'sup) '((:height 0.8)))
    ('font (when-let* ((color (alist-get 'color attrs)))
             (list (list :foreground color))))
    (_ nil)))

(defun vui-html--propertize (string face)
  "Return STRING propertized with FACE, when non-nil."
  (if (or (null face) (string-empty-p string))
      string
    (propertize string 'face face)))

(defun vui-html--add-text-properties (string props)
  "Return a copy of STRING with text PROPS added.
Existing properties, including faces, are preserved."
  (if (string-empty-p string)
      string
    (let ((result (copy-sequence string)))
      (add-text-properties 0 (length result) props result)
      result)))

;;; Inline rendering

(defun vui-html--link-keymap (url)
  "Return a keymap that opens URL with `vui-html-browse-url'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") (lambda () (interactive) (funcall vui-html-browse-url url)))
    (define-key map [mouse-1] (lambda () (interactive) (funcall vui-html-browse-url url)))
    (define-key map [mouse-2] (lambda () (interactive) (funcall vui-html-browse-url url)))
    map))

(defun vui-html--render-inline-nodes (nodes &optional face)
  "Render NODES as inline text, applying FACE to text nodes."
  (mapconcat (lambda (node) (vui-html--render-inline node face)) nodes ""))

(defun vui-html--render-inline-string (nodes &optional face)
  "Render NODES as a single collapsed inline string.
FACE is applied to text nodes."
  (vui-html--collapse-whitespace (vui-html--render-inline-nodes nodes face)))

(defun vui-html--render-inline-image (attrs face)
  "Render an `<img>' with ATTRS as inline text, applying FACE."
  (let ((alt (alist-get 'alt attrs))
        (src (alist-get 'src attrs)))
    (vui-html--propertize
     (format "[image: %s]" (or alt src "unknown"))
     face)))

(defun vui-html--render-inline-anchor (attrs children face)
  "Render an `<a>' with ATTRS and CHILDREN as inline text.
Nested text inherits FACE; when a href is present, a link face,
tooltip and activation keymap are added."
  (let* ((href (alist-get 'href attrs))
         (title (alist-get 'title attrs))
         (child-face (if href (vui-html--merge-faces face '(link)) face))
         (text (vui-html--render-inline-nodes children child-face)))
    (vui-html--add-target-id
     (if href
         (vui-html--add-text-properties
          text (list 'help-echo (or title href)
                     'mouse-face 'highlight
                     'shr-url href
                     'keymap (vui-html--link-keymap href)))
       text)
     (alist-get 'id attrs))))

(defun vui-html--render-inline-control (tag attrs children face)
  "Render form control TAG with ATTRS and CHILDREN as inline text.
FACE is applied to the resulting placeholder."
  (pcase tag
    ('input
     (let ((type (downcase (or (alist-get 'type attrs) "text"))))
       (vui-html--propertize
        (pcase type
          ((or "hidden" "image") "")
          ((or "checkbox" "radio")
           (if (vui-html--attr-p attrs 'checked) "☑" "☐"))
          ((or "submit" "button" "reset")
           (format "[%s]" (or (alist-get 'value attrs) "Submit")))
          (_ (or (alist-get 'value attrs)
                 (alist-get 'placeholder attrs)
                 "----------")))
        face)))
    ('button
     (vui-html--propertize
      (format "[%s]" (vui-html--render-inline-string children))
      face))
    ('select
     (let* ((option (seq-find (lambda (child) (eq (car-safe child) 'option))
                              children))
            (label (if option
                       (vui-html--render-inline-string (cddr option))
                     "")))
       (vui-html--propertize (format "[%s ▾]" label) face)))
    ('textarea
     (vui-html--propertize (vui-html--raw-text children) face))))

(defun vui-html--render-inline-media (tag attrs face)
  "Render media element TAG with ATTRS as inline text, applying FACE."
  (let ((title (or (alist-get 'title attrs)
                   (alist-get 'alt attrs)
                   (alist-get 'src attrs))))
    (vui-html--propertize
     (format "[%s%s]" tag (if title (format ": %s" title) ""))
     face)))

(defconst vui-html--target-id-marker "​"
  "Zero-width character used to anchor an ID on an otherwise empty element.")

(defun vui-html--add-target-id (result id)
  "Attach `shr-target-id' text property ID to RESULT.
RESULT is a string or a vnode.  Container vnodes are descended
into so the property lands on the first rendered text.  Empty
elements receive a zero-width marker, since an empty string cannot
carry text properties."
  (if (not id)
      result
    (cond
     ((stringp result)
      (if (string-empty-p result)
          (propertize vui-html--target-id-marker 'shr-target-id id)
        (vui-html--add-text-properties result (list 'shr-target-id id))))
     ((vui-vnode-text-p result)
      (let ((content (vui-vnode-text-content result)))
        (vui-vnode-text--create
         :content (if (and (stringp content) (string-empty-p content))
                      vui-html--target-id-marker
                    content)
         :face (vui-vnode-text-face result)
         :key (vui-vnode-text-key result)
         :properties (append (vui-vnode-text-properties result)
                             (list 'shr-target-id id)))))
     ((vui-vnode-button-p result)
      (setf (vui-vnode-button-label result)
            (vui-html--add-target-id (vui-vnode-button-label result) id))
      result)
     ((vui-vnode-box-p result)
      (setf (vui-vnode-box-child result)
            (vui-html--add-target-id (vui-vnode-box-child result) id))
      result)
     ((vui-vnode-region-p result)
      (when-let* ((children (vui-vnode-region-children result)))
        (setf (vui-vnode-region-children result)
              (cons (vui-html--add-target-id (car children) id)
                    (cdr children))))
      result)
     ((vui-vnode-fragment-p result)
      (when-let* ((children (vui-vnode-fragment-children result)))
        (setf (vui-vnode-fragment-children result)
              (cons (vui-html--add-target-id (car children) id)
                    (cdr children))))
      result)
     ((vui-vnode-vstack-p result)
      (when-let* ((children (vui-vnode-vstack-children result)))
        (setf (vui-vnode-vstack-children result)
              (cons (vui-html--add-target-id (car children) id)
                    (cdr children))))
      result)
     ((vui-vnode-hstack-p result)
      (when-let* ((children (vui-vnode-hstack-children result)))
        (setf (vui-vnode-hstack-children result)
              (cons (vui-html--add-target-id (car children) id)
                    (cdr children))))
      result)
     (t result))))

(defun vui-html--render-inline (node &optional face)
  "Render NODE as propertized inline text, layering FACE beneath markup faces."
  (cond
   ((stringp node)
    (vui-html--propertize (vui-html--normalize-text node) face))
   ((not (consp node)) "")
   (t
    (let* ((tag (car node))
           (attrs (cadr node))
           (children (cddr node))
           (child-face (vui-html--merge-faces
                        face (vui-html--tag-face tag attrs))))
      (pcase tag
        ('br "\n")
        ((or 'wbr 'comment) "")
        ((or 'audio 'video 'iframe 'embed 'object)
         (vui-html--render-inline-media tag attrs child-face))
        ('img (vui-html--render-inline-image attrs child-face))
        ('a (vui-html--render-inline-anchor attrs children face))
        ((or 'input 'button 'select 'textarea)
         (vui-html--render-inline-control tag attrs children child-face))
        ('q (concat "“" (vui-html--render-inline-nodes children child-face) "”"))
        ((or 'script 'style 'head 'title 'meta 'link 'base 'template 'noscript
             'source 'track 'param 'col 'colgroup 'datalist 'map 'area
             'option 'optgroup)
         "")
        (_
         (let ((result (vui-html--render-inline-nodes children child-face))
               (title (alist-get 'title attrs)))
           (if (and title (memq tag '(abbr dfn time)))
               (setq result (vui-html--add-text-properties
                             result (list 'help-echo title))))
           (vui-html--add-target-id result (alist-get 'id attrs)))))))))

;;; Block rendering

(defun vui-html--inline-node-p (node)
  "Return non-nil when NODE belongs to inline text flow."
  (or (stringp node)
      (and (consp node) (memq (car node) vui-html--inline-or-ignored-tags))))

(defun vui-html--compose-block (nodes &rest props)
  "Compose NODES into a single block vnode.
A single node is returned as-is unless PROPS is non-nil."
  (if (and (null props) (= 1 (length nodes)))
      (car nodes)
    (apply #'vui-vstack (append props nodes))))

(defun vui-html--sole-anchor (children)
  "Return the single non-blank anchor node in CHILDREN, if there is one.
Standalone anchors are rendered as buttons rather than as inline text."
  (let ((nodes (seq-remove (lambda (child)
                             (and (stringp child)
                                  (string-empty-p (string-trim child))))
                           children)))
    (when (and (= 1 (length nodes))
               (eq (car-safe (car nodes)) 'a))
      (car nodes))))

(defun vui-html--translate-block-children (children &optional width indent)
  "Translate CHILDREN into a list of block-level vnodes.
Consecutive inline nodes are merged into one `vui-text' vnode.
When WIDTH is non-nil, merged text is wrapped to WIDTH columns and
continuation lines are indented by INDENT."
  (if-let* ((anchor (vui-html--sole-anchor children)))
      (let ((node (vui-html--translate-node anchor)))
        (if node (list node) nil))
    (vui-html--translate-block-children-1 children width indent)))

(defun vui-html--translate-block-children-1 (children &optional width indent)
  "Translate CHILDREN, merging consecutive inline nodes into text.
WIDTH and INDENT are passed to `vui-html--wrap-text' when WIDTH is
non-nil."
  (let ((result nil)
        (inline "")
        (break-p nil))
    (cl-labels
        ((flush ()
           (let ((text (vui-html--collapse-whitespace inline)))
             (cond
              ((not (string-empty-p text))
               (push (if width
                         (vui-html--wrap-text (vui-text text) width (or indent 0))
                       (vui-text text))
                     result))
              (break-p (push (vui-newline) result))))
           (setq inline "" break-p nil)))
      (dolist (child children)
        (if (vui-html--inline-node-p child)
            (progn
              (when (eq (car-safe child) 'br)
                (setq break-p t))
              (setq inline (concat inline (vui-html--render-inline child))))
          (flush)
          (let ((node (vui-html--translate-node child)))
            (when node (push node result)))))
      (flush))
    (nreverse result)))

(defun vui-html--translate-container (children)
  "Translate CHILDREN as a block container.
Inline text is wrapped to `vui-html-max-width', so that documents that
use <div> instead of <p> for body text honour the configured width."
  (vui-html--compose-block
   (vui-html--translate-block-children children vui-html-max-width)))

(defun vui-html--indent-text-lines (text indent)
  "Prefix every line of TEXT except the first with INDENT spaces."
  (if (or (<= indent 0) (not (string-match-p "\n" text)))
      text
    (replace-regexp-in-string
     "\n" (concat "\n" (make-string indent ?\s)) text t t)))

(defun vui-html--wrap-text (node width indent)
  "Wrap text NODE to WIDTH columns, indenting continuation lines by INDENT.
Return NODE unchanged when it is not a text vnode."
  (if (not (vui-vnode-text-p node))
      node
    (let* ((wrapped (vui+-kp-text node width))
           (content (vui-vnode-text-content wrapped)))
      (when (stringp content)
        (setf (vui-vnode-text-content wrapped)
              (vui-html--indent-text-lines content indent)))
      wrapped)))

(defun vui-html--translate-list (children &optional ordered start)
  "Translate list CHILDREN into a `vui-list'.
ORDERED selects numbered markers; START is the first number."
  (let* ((items (seq-filter (lambda (child) (eq (car-safe child) 'li))
                            children))
         (number (or start 1)))
    (vui-list
     items
     (lambda (item)
       (let* ((marker (if ordered
                          (prog1 (format "%d." number)
                            (setq number (1+ number)))
                        "•"))
              ;; The marker is followed by one space, and continuation
              ;; lines are aligned under the item's text.
              (indent (1+ (length marker)))
              (width (max 1 (- vui-html-max-width indent)))
              (nodes (vui-html--translate-block-children
                      (cddr item) width indent)))
         (if (cdr nodes)
             ;; Inline content followed by nested blocks (usually a
             ;; nested list): keep the marker on the first line and
             ;; indent the remaining blocks beneath it.
             (vui-vstack
              (vui-hstack marker (car nodes))
              (apply #'vui-vstack :indent 2 (cdr nodes)))
           (vui-hstack marker (or (car nodes) (vui-vstack)))))))))

(defun vui-html--translate-dl (children)
  "Translate definition list CHILDREN into vnodes."
  (let (nodes)
    (dolist (child children)
      (pcase (car-safe child)
        ('dt
         (push (vui-strong (vui-html--render-inline-string (cddr child)))
               nodes))
        ('dd
         (push (vui-html--compose-block
                (vui-html--translate-block-children
                 (cddr child) (max 1 (- vui-html-max-width 2)) 2)
                :indent 2)
               nodes))))
    (vui-html--compose-block (nreverse nodes))))

(defun vui-html--table-rows (children)
  "Collect all `<tr>' nodes from table CHILDREN."
  (seq-mapcat
   (lambda (child)
     (pcase (car-safe child)
       ('tr (list child))
       ((or 'thead 'tbody 'tfoot) (vui-html--table-rows (cddr child)))
       (_ nil)))
   children))

(defun vui-html--table-cells (row)
  "Return the `<th>' and `<td>' cells of ROW."
  (seq-filter (lambda (child) (memq (car-safe child) '(th td)))
              (cddr row)))

(defun vui-html--table-cell-content (cell)
  "Translate table CELL contents."
  (let ((children (cddr cell)))
    (if (cl-every #'vui-html--inline-node-p children)
        (vui-html--render-inline-string children)
      (vui-html--compose-block
       (vui-html--translate-block-children children)))))

(defun vui-html--translate-table (children)
  "Translate table CHILDREN into a `vui-table' vnode."
  (let* ((caption (seq-find (lambda (child) (eq (car-safe child) 'caption))
                            children))
         (rows (seq-filter (lambda (row) (vui-html--table-cells row))
                           (vui-html--table-rows children))))
    (if (null rows)
        (vui-vstack)
      (let* ((header-cells (vui-html--table-cells (car rows)))
             (header-p (and header-cells
                            (cl-every (lambda (cell) (eq (car cell) 'th))
                                      header-cells)))
             (data-rows (if header-p (cdr rows) rows))
             (ncols (cl-loop for row in rows
                             maximize (length (vui-html--table-cells row))))
             (columns
              (cl-loop
               for i below ncols
               collect
               (let ((cell (nth i (and header-p header-cells))))
                 (if cell
                     (list :header (vui-html--render-inline-string (cddr cell)))
                   (list)))))
             (table
              (vui-table
               :columns columns
               :rows (mapcar
                      (lambda (row)
                        (mapcar #'vui-html--table-cell-content
                                (vui-html--table-cells row)))
                      data-rows)
               :border vui-html-table-border)))
        (if caption
            (vui-vstack
             (vui-muted (vui-html--render-inline-string (cddr caption)))
             table)
          table)))))

(defun vui-html--language-from-class (class)
  "Extract a language name from CLASS, e.g. \"language-python\"."
  (when (stringp class)
    (when (string-match
           "\\(?:^\\|\\s-\\)\\(?:language\\|lang\\)-\\([[:alnum:]_+#.-]+\\)"
           class)
      (match-string 1 class))))

(defun vui-html--pre-language (attrs children)
  "Determine the language of a `<pre>' with ATTRS and CHILDREN."
  (or (vui-html--language-from-class (alist-get 'class attrs))
      (seq-some
       (lambda (child)
         (when (eq (car-safe child) 'code)
           (let ((code-attrs (cadr child)))
             (or (vui-html--language-from-class
                  (alist-get 'class code-attrs))
                 (alist-get 'data-lang code-attrs)))))
       children)
      (alist-get 'lang attrs)
      vui-html-pre-language))

(defun vui-html--translate-input (attrs)
  "Translate an `<input>' with ATTRS into a form component."
  (let* ((type (downcase (or (alist-get 'type attrs) "text")))
         (value (alist-get 'value attrs))
         (placeholder (alist-get 'placeholder attrs))
         (name (or (alist-get 'name attrs) (alist-get 'id attrs)))
         (size-attr (alist-get 'size attrs))
         (size (if (and (stringp size-attr)
                        (string-match-p "\\`[0-9]+\\'" size-attr))
                   (string-to-number size-attr)
                 30)))
    (pcase type
      ((or "hidden" "image") nil)
      ((or "checkbox" "radio")
       (vui-checkbox :checked (vui-html--attr-p attrs 'checked)
                     :label (or (alist-get 'aria-label attrs) name)))
      ((or "submit" "reset" "button")
       (vui-button (or value "Submit")))
      ("password"
       (vui-field :value (or value "") :size (min size 32)
                  :placeholder placeholder :secret t))
      (_
       (vui-field :value (or value "") :size size
                  :placeholder placeholder)))))

(defun vui-html--translate-select (children attrs)
  "Translate a `<select>' with CHILDREN and ATTRS."
  (let (options selected)
    (dolist (child children)
      (let ((candidates (if (eq (car-safe child) 'optgroup)
                            (cddr child)
                          (list child))))
        (dolist (option candidates)
          (when (eq (car-safe option) 'option)
            (let* ((option-attrs (cadr option))
                   (label (vui-html--render-inline-string (cddr option)))
                   (value (or (alist-get 'value option-attrs) label)))
              (push (cons value label) options)
              (when (vui-html--attr-p option-attrs 'selected)
                (setq selected value)))))))
    (setq options (nreverse options))
    (vui-select
     :value (or (alist-get 'value attrs) selected (car-safe (car options)))
     :options options)))

(defun vui-html--translate-details (attrs children)
  "Translate `<details>' with ATTRS and CHILDREN into a collapsible."
  (let* ((summary (seq-find (lambda (child) (eq (car-safe child) 'summary))
                            children))
         (body (seq-remove (lambda (child) (eq (car-safe child) 'summary))
                           children))
         (title (if summary
                    (vui-html--render-inline-string (cddr summary))
                  "Details")))
    (apply #'vui-collapsible
           (append (list :title title
                         :initially-expanded (vui-html--attr-p attrs 'open))
                   (vui-html--translate-block-children body)))))

;;; HTML to VUI Translation

(defun vui-html--translate-node (node)
  "Translate block-level parsed HTML NODE to a vui component or nil."
  (let ((tag (car node)))
    (unless (memq tag vui-html--ignored-tags)
      (vui-html--translate-element tag (cadr node) (cddr node)))))

(defun vui-html--translate-element (tag attrs children)
  "Translate HTML element with TAG, ATTRS and CHILDREN to a vui component."
  (vui-html--add-target-id
   (cond
    ;; Document structure and generic block containers
    ((memq tag '(html body top div section article main header footer nav
                      aside address figure center hgroup search))
     (vui-html--translate-container children))

   ;; Paragraphs and headings
    ((eq tag 'p)
     (vui+-kp-text (vui-html--render-inline-string children)
                   vui-html-max-width))
    ((memq tag '(h1 h2 h3 h4 h5 h6 h7 h8))
     (let ((level (string-to-number (substring (symbol-name tag) 1))))
       (vui-heading (vui-html--render-inline-string children)
                    :level level)))

   ;; Line and block separators
    ((eq tag 'hr) (vui-text (make-string vui-html-max-width ?─)))

   ;; Quoted and preformatted blocks
    ((eq tag 'blockquote)
     (apply #'vui-vstack
            (append (list :indent 2 :face 'vui-muted)
                    (vui-html--translate-block-children
                     children (max 1 (- vui-html-max-width 2)) 2))))
    ((eq tag 'pre)
     (let* ((code (vui-html--raw-text children))
            ;; Browsers drop a single newline directly after <pre>.
            (code (if (string-prefix-p "\n" code)
                      (substring code 1)
                    code)))
       (vui+-source-code code (vui-html--pre-language attrs children))))

   ;; Lists
    ((eq tag 'ul)
     (vui-html--translate-list children))
    ((eq tag 'menu)
     (vui-html--translate-list children))
    ((eq tag 'ol)
     (let ((start (alist-get 'start attrs)))
       (vui-html--translate-list
        children t (and (stringp start)
                        (string-match-p "\\`[0-9]+\\'" start)
                        (string-to-number start)))))
    ((eq tag 'li)
     (vui-html--compose-block
      (vui-html--translate-block-children children)))
    ((eq tag 'dl) (vui-html--translate-dl children))
    ((eq tag 'dt)
     (vui-strong (vui-html--render-inline-string children)))
    ((eq tag 'dd)
     (vui-html--compose-block
      (vui-html--translate-block-children
       children (max 1 (- vui-html-max-width 2)) 2)
      :indent 2))

   ;; Tables
    ((eq tag 'table) (vui-html--translate-table children))
    ((or (eq tag 'thead) (eq tag 'tbody) (eq tag 'tfoot)
         (eq tag 'tr) (eq tag 'th) (eq tag 'td))
     (vui-html--translate-container children))
    ((eq tag 'caption)
     (vui-muted (vui-html--render-inline-string children)))

   ;; Links and media
    ((eq tag 'a)
     (let ((href (alist-get 'href attrs))
           (label (vui-html--render-inline-string children)))
       (if href
           (vui-button (vui-html--add-text-properties
                        (if (string-empty-p label) href label)
                        (list 'shr-url href))
             :on-click (lambda () (funcall vui-html-browse-url href)))
         (vui-text label))))
    ((eq tag 'img)
     (let ((src (alist-get 'src attrs))
           (alt (or (alist-get 'alt attrs)
                    (alist-get 'title attrs))))
       (if src
           (vui-button (vui-html--add-text-properties
                        (format "[image: %s]" (or alt src))
                        (list 'shr-url src))
             :no-decoration t
             :on-click (lambda () (funcall vui-html-browse-url src)))
         (vui-text (format "[image: %s]" (or alt "unknown"))))))
    ((memq tag '(audio video iframe embed object))
     (let ((src (or (alist-get 'src attrs) (alist-get 'data attrs)))
           (title (or (alist-get 'title attrs) (alist-get 'alt attrs))))
       (vui-button (vui-html--add-text-properties
                    (format "[%s%s]" tag (if title (format ": %s" title) ""))
                    (when src (list 'shr-url src)))
         :no-decoration t
         :on-click (when src (lambda () (funcall vui-html-browse-url src))))))

   ;; Forms
    ((eq tag 'form) (vui-html--translate-container children))
    ((eq tag 'fieldset) (vui-html--translate-container children))
    ((eq tag 'legend)
     (vui-strong (vui-html--render-inline-string children)))
    ((eq tag 'input) (vui-html--translate-input attrs))
    ((eq tag 'textarea)
     (vui-field :value (vui-html--raw-text children) :size 40))
    ((eq tag 'select) (vui-html--translate-select children attrs))
    ((eq tag 'button)
     (vui-button (vui-html--render-inline-string children)))
    ((eq tag 'output)
     (vui-text (vui-html--render-inline-string children)))
    ((memq tag '(progress meter))
     (vui-text (format "%s %s" tag (or (alist-get 'value attrs) ""))))

   ;; Disclosure and captions
    ((eq tag 'details) (vui-html--translate-details attrs children))
    ((eq tag 'summary)
     (vui-strong (vui-html--render-inline-string children)))
    ((eq tag 'figcaption)
     (vui-muted (vui-html--render-inline-string children)))
    ((eq tag 'dialog) (vui-html--translate-container children))

   ;; Unknown elements - treat as a block container
    (t (vui-html--translate-container children)))
   (alist-get 'id attrs)))

;;; Main API

(defun vui-html (html)
  "Parse HTML string and return a vui component tree.
HTML is a string of HTML markup.  Returns a vui component suitable
for rendering via `vui-mount'."
  (vui-html--translate-node (vui-html--parse-html html)))

(vui-defcomponent vui-html (node) :render node)

(defun vui-html-mount (&optional html)
  "Render HTML string in `vui-html-buffer-name'.
When HTML is nil, use the contents of the current buffer.
Returns the buffer if successful, nil otherwise."
  (interactive)
  (unless html
    (setq html (buffer-string)))
  (vui-mount
   (vui-component 'vui-html :node (vui-html html))
   vui-html-buffer-name))

(provide 'vui-html)
;;; vui-html.el ends here
