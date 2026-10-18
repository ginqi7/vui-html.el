;;; vui-html-test.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

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
;; Tests for the HTML to vui translation.

;;; Code:

(require 'buttercup)
(require 'vui-html)

(defun vui-html-test--render (html)
  "Render HTML using vui and return the buffer text without properties."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (vui-render (vui-html html) (current-buffer))
      (substring-no-properties (buffer-string)))))

(defun vui-html-test--content (html)
  "Translate HTML and return the text content of the resulting vnode."
  (let ((node (vui-html html)))
    (cond
     ((vui-vnode-text-p node) (vui-vnode-text-content node))
     ((stringp node) node)
     (t nil))))

(describe "vui-html.el"
          (it "loads successfully"
              (expect (featurep 'vui-html) :to-be-truthy)))

;;; Existing behaviour

(describe "vui-html basic translation"
  (it "parse <p>hello</p>"
    (expect (vui-html "<p>hello</p>") :to-equal (vui-text "hello")))

  (it "parse <p>hello</p><p>world</p>"
    (expect (vui-html "<p>hello</p><p>world</p>") :to-equal
            (vui-vstack (vui-text "hello")
                        (vui-text "world"))))

  (it "parse <p>hello<b>world</b></p>"
    (expect (vui-html "<p>hello <b>world</b></p>") :to-equal
            (vui-text #("hello world" 6 11 (face vui-strong)))))

  (it "parse <p>hello <b>world</b></p>\n <br> \n <p>hello <b>world</b></p>"
    (expect (vui-html "<p>hello <b>world</b></p>\n <br> \n <p>hello <b>world</b></p>") :to-equal
            (vui-vstack #s(vui-vnode-text nil #("hello world" 6 11 (face vui-strong)) nil nil)
                        (vui-newline)
                        #s(vui-vnode-text nil #("hello world" 6 11 (face vui-strong)) nil nil))))

  (it "parse <h1> HTML VUI </h1>"
    (expect (vui-html "<h1> HTML VUI </h1>") :to-equal
            (vui-heading-1 "HTML VUI")))

  (it "renders a standalone <a> as a clickable button"
    (let ((node (vui-html "<a href=\"http://qiqijin.com\">Qiqi</a>")))
      (expect (vui-vnode-button-p node) :to-be-truthy)
      (expect (vui-vnode-button-label node) :to-equal "Qiqi")
      (spy-on 'browse-url)
      (funcall (vui-vnode-button-on-click node))
      (expect 'browse-url :to-have-been-called-with "http://qiqijin.com"))))

;;; Inline formatting

(describe "vui-html inline formatting"
  (it "renders <em> and <i> with the italic face"
    (expect (vui-html-test--content "<p><em>x</em></p>") :to-equal
            (propertize "x" 'face '(vui-italic)))
    (expect (vui-html-test--content "<p><i>x</i></p>") :to-equal
            (propertize "x" 'face '(vui-italic))))

  (it "renders <u> and <ins> with an underline face"
    (expect (vui-html-test--content "<p><u>x</u></p>") :to-equal
            (propertize "x" 'face '((:underline t))))
    (expect (vui-html-test--content "<p><ins>x</ins></p>") :to-equal
            (propertize "x" 'face '((:underline t)))))

  (it "renders <s>, <strike> and <del> with a strike-through face"
    (dolist (tag '("s" "strike" "del"))
      (expect (vui-html-test--content (format "<p><%s>x</%s></p>" tag tag))
              :to-equal (propertize "x" 'face '((:strike-through t))))))

  (it "renders <mark>, <small> and <big>"
    (expect (vui-html-test--content "<p><mark>x</mark></p>") :to-equal
            (propertize "x" 'face '((:inherit highlight))))
    (expect (vui-html-test--content "<p><small>x</small></p>") :to-equal
            (propertize "x" 'face '((:height 0.8))))
    (expect (vui-html-test--content "<p><big>x</big></p>") :to-equal
            (propertize "x" 'face '((:height 1.2)))))

  (it "renders <code>, <kbd>, <samp> and <tt> with the code face"
    (dolist (tag '("code" "kbd" "samp" "tt"))
      (expect (vui-html-test--content (format "<p><%s>x</%s></p>" tag tag))
              :to-equal (propertize "x" 'face '(vui-code)))))

  (it "composes nested faces"
    (expect (vui-html-test--content "<p><b>bold <i>both</i></b></p>") :to-equal
            #("bold both" 0 4 (face (vui-strong)) 5 9 (face (vui-strong vui-italic)))))

  (it "renders <q> with quotation marks"
    (expect (vui-html-test--content "<p><q>quoted</q></p>") :to-equal "“quoted”"))

  (it "adds a tooltip from the <abbr> title attribute"
    (let ((content (vui-html-test--content
                    "<p><abbr title=\"HyperText Markup Language\">HTML</abbr></p>")))
      (expect content :to-equal "HTML")
      (expect (get-text-property 0 'help-echo content)
              :to-equal "HyperText Markup Language")))

  (it "renders <sub> and <sup> as smaller text"
    (dolist (tag '("sub" "sup"))
      (expect (vui-html-test--content (format "<p>H<%s>2</%s>O</p>" tag tag))
              :to-equal (concat "H" (propertize "2" 'face '((:height 0.8))) "O"))))

  (it "preserves inline whitespace without inserting extra spaces"
    (let ((content (vui-html-test--content "<p>a<b> b </b>c   d</p>")))
      (expect (substring-no-properties content) :to-equal "a b c d")
      (expect (get-text-property 2 'face content) :to-equal '(vui-strong))))

  (it "turns <br> into a newline inside a paragraph"
    (expect (vui-html-test--content "<p>a<br>b</p>") :to-equal "a\nb"))

  (it "decodes HTML entities"
    (expect (vui-html-test--content "<p>&amp; &lt; &gt; &#65;</p>")
            :to-equal "& < > A")))

;;; Headings

(describe "vui-html headings"
  (it "renders h1 through h6 with the matching heading face"
    (dolist (level (number-sequence 1 6))
      (let ((node (vui-html (format "<h%d>Head</h%d>" level level))))
        (expect (vui-vnode-text-content node) :to-equal "Head")
        (expect (vui-vnode-text-face node)
                :to-equal (intern (format "vui-heading-%d" level))))))

  (it "keeps inline formatting inside a heading"
    (let ((content (vui-vnode-text-content
                    (vui-html "<h2>A <code>b</code></h2>"))))
      (expect (substring-no-properties content) :to-equal "A b")
      (expect (get-text-property 2 'face content) :to-equal '(vui-code)))))

;;; Block containers

(describe "vui-html block containers"
  (it "treats <section>, <article>, <main> and friends as containers"
    (dolist (tag '("section" "article" "main" "header" "footer" "nav" "aside"))
      (expect (vui-html (format "<%s><p>a</p><p>b</p></%s>" tag tag))
              :to-equal (vui-vstack (vui-text "a") (vui-text "b")))))

  (it "treats unknown tags as containers"
    (expect (vui-html "<unknown>hello</unknown>")
            :to-equal (vui-text "hello")))

  (it "wraps text inside a generic container to `vui-html-max-width'"
    (let* ((text (mapconcat (lambda (_) "这是一个很长的中文句子")
                            (number-sequence 1 20) ""))
           (lines (split-string
                   (vui-html-test--render (format "<div><span>%s</span></div>" text))
                   "\n")))
      (expect (> (length lines) 1) :to-be-truthy)
      (dolist (line lines)
        (expect (<= (length line) vui-html-max-width) :to-be-truthy))))

  (it "ignores comments and <script>/<style> content"
    (expect (vui-html "<!-- a comment --><p>hello</p>")
            :to-equal (vui-text "hello"))
    (expect (vui-html "<script>bad()</script><style>.a{}</style><p>ok</p>")
            :to-equal (vui-text "ok")))

  (it "ignores document metadata"
    (expect (vui-html "<head><title>T</title><meta charset=\"utf-8\"></head><p>x</p>")
            :to-equal (vui-text "x")))

  (it "renders a complete document"
    (expect (vui-html-test--render
             "<!doctype html><html><head><title>T</title></head><body><h1>Hi</h1><p>Body</p></body></html>")
            :to-equal "Hi\nBody")))

;;; Lists

(describe "vui-html lists"
  (it "renders an unordered list"
    (expect (vui-html-test--render "<ul><li>one</li><li>two</li></ul>")
            :to-equal "• one\n• two"))

  (it "renders an ordered list"
    (expect (vui-html-test--render "<ol><li>a</li><li>b</li></ol>")
            :to-equal "1. a\n2. b"))

  (it "honours the start attribute of an ordered list"
    (expect (vui-html-test--render "<ol start=\"3\"><li>a</li><li>b</li></ol>")
            :to-equal "3. a\n4. b"))

  (it "indents nested lists"
    (expect (vui-html-test--render
             "<ul><li>one</li><li>two<ul><li>nested</li></ul></li></ul>")
            :to-equal "• one\n• two\n  • nested"))

  (it "renders a definition list"
    (expect (vui-html-test--render
             "<dl><dt>Term</dt><dd>Definition</dd></dl>")
            :to-equal "Term\n  Definition"))

  (it "renders inline formatting inside a list item"
    (expect (vui-html-test--render "<ul><li>a <b>b</b></li></ul>")
            :to-equal "• a b")))

;;; Tables

(describe "vui-html tables"
  (it "translates a table with a header row"
    (let ((node (vui-html
                 "<table><tr><th>A</th><th>B</th></tr><tr><td>1</td><td>2</td></tr></table>")))
      (expect (vui-vnode-table-p node) :to-be-truthy)
      (expect (vui-vnode-table-columns node)
              :to-equal '((:header "A") (:header "B")))
      (expect (vui-vnode-table-rows node) :to-equal '(("1" "2")))
      (expect (vui-vnode-table-border node) :to-equal :unicode)))

  (it "renders the table with borders"
    (let ((rendered (vui-html-test--render
                     "<table><tr><th>A</th><th>B</th></tr><tr><td>1</td><td>2</td></tr></table>")))
      (expect (string-match-p "│ A │ B │" rendered) :to-be-truthy)
      (expect (string-match-p "│ 1 │ 2 │" rendered) :to-be-truthy)))

  (it "supports thead/tbody and caption"
    (let ((node (vui-html
                 "<table><caption>Cap</caption><thead><tr><th>A</th></tr></thead><tbody><tr><td>1</td></tr></tbody></table>")))
      (expect (vui-vnode-vstack-p node) :to-be-truthy)
      (expect (vui-vnode-text-content (car (vui-vnode-vstack-children node)))
              :to-equal "Cap")
      (let ((table (cadr (vui-vnode-vstack-children node))))
        (expect (vui-vnode-table-columns table) :to-equal '((:header "A")))
        (expect (vui-vnode-table-rows table) :to-equal '(("1"))))))

  (it "renders a table without a header"
    (let ((node (vui-html "<table><tr><td>1</td><td>2</td></tr></table>")))
      (expect (vui-vnode-table-columns node) :to-equal '(nil nil))
      (expect (vui-vnode-table-rows node) :to-equal '(("1" "2")))))

  (it "keeps inline formatting in table cells"
    (let ((node (vui-html "<table><tr><td><b>x</b></td></tr></table>")))
      (expect (vui-vnode-table-rows node) :to-equal
              (list (list (propertize "x" 'face '(vui-strong))))))))

;;; Preformatted text

(describe "vui-html preformatted text"
  (it "renders <pre> content"
    (expect (vui-html "<pre>code here</pre>") :to-equal (vui-text "code here")))

  (it "drops the first newline after <pre>"
    (expect (vui-html-test--content "<pre>\nline1\nline2</pre>")
            :to-equal "line1\nline2"))

  (it "detects the language from a code element class"
    (expect (vui-html--pre-language
             nil '((code ((class . "language-python")) "print(1)")))
            :to-equal "python"))

  (it "detects the language from the pre class"
    (expect (vui-html--pre-language
             '((class . "lang-rust")) '("fn main() {}"))
            :to-equal "rust"))

  (it "renders a highlighted code block"
    (let ((node (vui-html
                 "<pre><code class=\"language-emacs-lisp\">(+ 1 2)</code></pre>")))
      (expect (substring-no-properties (vui-vnode-text-content node))
              :to-equal "(+ 1 2)"))))

;;; Quotes

(describe "vui-html blockquote"
  (it "indents and mutes quoted content"
    (let ((node (vui-html "<blockquote>quoted <b>text</b></blockquote>")))
      (expect (vui-vnode-vstack-p node) :to-be-truthy)
      (expect (vui-vnode-vstack-indent node) :to-equal 2)
      (expect (vui-vnode-vstack-face node) :to-equal 'vui-muted)
      (expect (substring-no-properties
               (vui-html-test--render "<blockquote>quoted <b>text</b></blockquote>"))
              :to-equal "  quoted text"))))

;;; Links and media

(describe "vui-html links"
  (it "renders inline links as propertized text"
    (let* ((content (vui-html-test--content
                     "<p>visit <a href=\"http://x.com\" title=\"X\">X</a> now</p>"))
           (position (string-match "X" content)))
      (expect (substring-no-properties content) :to-equal "visit X now")
      (expect (get-text-property position 'face content) :to-equal '(link))
      (expect (get-text-property position 'help-echo content) :to-equal "X")
      (expect (get-text-property position 'keymap content) :to-be-truthy)))

  (it "renders an anchor without href as plain text"
    (expect (vui-html "<p>a <a>plain</a> b</p>")
            :to-equal (vui-text "a plain b"))))

(describe "vui-html media"
  (it "renders an image as a button with its alt text"
    (let ((node (vui-html "<img src=\"a.png\" alt=\"A\">")))
      (expect (vui-vnode-button-p node) :to-be-truthy)
      (expect (vui-vnode-button-label node) :to-equal "[image: A]")
      (expect (vui-vnode-button-no-decoration node) :to-be-truthy)))

  (it "renders an image inline inside a paragraph"
    (expect (vui-html-test--content "<p>hello <img src=\"a.png\" alt=\"A\"> world</p>")
            :to-equal "hello [image: A] world"))

  (it "renders an image without src as text"
    (expect (vui-html "<img alt=\"A\">")
            :to-equal (vui-text "[image: A]")))

  (it "renders media elements as labelled buttons"
    (let ((node (vui-html "<iframe src=\"http://x\" title=\"Frame\"></iframe>")))
      (expect (vui-vnode-button-p node) :to-be-truthy)
      (expect (vui-vnode-button-label node) :to-equal "[iframe: Frame]"))))

;;; Forms

(describe "vui-html form elements"
  (it "renders a text input as a field"
    (expect (vui-html "<input type=\"text\" value=\"v\" placeholder=\"p\">")
            :to-equal (vui-field :value "v" :size 30 :placeholder "p")))

  (it "honours the size attribute"
    (expect (vui-html "<input type=\"text\" size=\"12\">")
            :to-equal (vui-field :size 12)))

  (it "renders a checked checkbox"
    (expect (vui-html "<input type=\"checkbox\" checked name=\"n\">")
            :to-equal (vui-checkbox :checked t :label "n")))

  (it "renders a submit input as a button"
    (expect (vui-html "<input type=\"submit\" value=\"Send\">")
            :to-equal (vui-button "Send")))

  (it "renders a hidden input as nothing"
    (let ((node (vui-html "<input type=\"hidden\" value=\"x\">")))
      (expect (vui-vnode-vstack-p node) :to-be-truthy)
      (expect (vui-vnode-vstack-children node) :not :to-be-truthy)
      (expect (vui-html-test--render "<input type=\"hidden\" value=\"x\">")
              :to-equal "")))

  (it "renders a textarea as a field"
    (expect (vui-html "<textarea>hello</textarea>")
            :to-equal (vui-field :value "hello" :size 40)))

  (it "renders a select with its options and selected value"
    (expect (vui-html
             "<select><option value=\"a\">A</option><option value=\"b\" selected>B</option></select>")
            :to-equal (vui-select :value "b" :options '(("a" . "A") ("b" . "B")))))

  (it "renders a button with its label"
    (expect (vui-html "<button>Go</button>")
            :to-equal (vui-button "Go")))

  (it "renders progress and meter values"
    (expect (vui-html "<progress value=\"0.5\"></progress>")
            :to-equal (vui-text "progress 0.5"))))

;;; Disclosure

(describe "vui-html details"
  (it "renders <details> as a collapsible section"
    (let ((node (vui-html
                 "<details><summary>More</summary><p>hidden</p></details>")))
      (expect (vui-vnode-component-p node) :to-be-truthy)
      (expect (vui-vnode-component-type node)
              :to-equal 'vui-collapsible--internal)
      (expect (plist-get (vui-vnode-component-props node) :title)
              :to-equal "More")
      (expect (vui-vnode-component-children node)
              :to-equal (list (vui-text "hidden")))))

  (it "starts collapsed and hides the body"
    (let ((rendered (vui-html-test--render
                     "<details><summary>More</summary><p>hidden</p></details>")))
      (expect (string-match-p "More" rendered) :to-be-truthy)
      (expect (string-match-p "hidden" rendered) :not :to-be-truthy))))

;;; Miscellaneous blocks

(describe "vui-html block elements"
  (it "renders <hr> as a rule"
    (expect (vui-html-test--content "<hr>")
            :to-equal (make-string vui-html-max-width ?─)))

  (it "renders <figure> with a caption"
    (expect (substring-no-properties
             (vui-html-test--render
              "<figure><img src=\"a.png\" alt=\"A\"><figcaption>cap</figcaption></figure>"))
            :to-equal "[image: A]\ncap"))

  (it "renders <dialog> as a container"
    (expect (vui-html "<dialog><p>hello</p></dialog>")
            :to-equal (vui-text "hello"))))

;;; Mounting

(describe "vui-html-mount"
  (it "renders into `vui-html-buffer-name'"
    (unwind-protect
        (progn
          (vui-html-mount "<p>mounted</p>")
          (expect (with-current-buffer vui-html-buffer-name
                    (substring-no-properties (buffer-string)))
                  :to-equal "mounted"))
      (when (get-buffer vui-html-buffer-name)
        (kill-buffer vui-html-buffer-name))))

  (it "applies `vui-html-font' to the rendered page"
    (let ((vui-html-font "TestFont"))
      (unwind-protect
          (progn
            (vui-html-mount "<p>mounted <code>code</code></p>")
            (with-current-buffer vui-html-buffer-name
              (expect (assq 'default face-remapping-alist)
                      :to-equal '(default (:family "TestFont") default))))
        (when (get-buffer vui-html-buffer-name)
          (kill-buffer vui-html-buffer-name)))))

  (it "applies family and size from a `vui-html-font' plist"
    (let ((vui-html-font '(:family "TestFont" :size 16)))
      (unwind-protect
          (progn
            (vui-html-mount "<p>mounted</p>")
            (with-current-buffer vui-html-buffer-name
              (expect (assq 'default face-remapping-alist)
                      :to-equal
                      '(default (:family "TestFont" :height 160) default))))
        (when (get-buffer vui-html-buffer-name)
          (kill-buffer vui-html-buffer-name)))))

  (it "keeps the font remapping after a re-render"
    (let ((vui-html-font "TestFont"))
      (unwind-protect
          (progn
            (vui-html-mount "<p>mounted</p>")
            (with-current-buffer vui-html-buffer-name
              (vui-rerender vui--root-instance)
              (expect (assq 'default face-remapping-alist)
                      :to-equal '(default (:family "TestFont") default))))
        (when (get-buffer vui-html-buffer-name)
          (kill-buffer vui-html-buffer-name))))))

;;; With-default-font

(describe "with-default-font"
  (it "sets the default face family for the dynamic extent of BODY"
    (let (inside)
      (with-default-font "TestFont"
        (setq inside (face-attribute 'default :family)))
      (expect inside :to-equal "TestFont")
      (expect (face-attribute 'default :family) :not :to-equal "TestFont")))

  (it "restores the default face when BODY signals"
    (let ((original (face-attribute 'default :family)))
      (ignore-errors
        (with-default-font "TestFont"
          (error "boom")))
      (expect (face-attribute 'default :family) :to-equal original)))

  (it "accepts a plist of face attributes"
    (with-default-font '(:family "TestFont" :weight bold)
      (expect (face-attribute 'default :family) :to-equal "TestFont")
      (expect (face-attribute 'default :weight) :to-equal 'bold)))

  (it "converts :size to an absolute :height"
    (with-default-font '(:family "TestFont" :size 16)
      (expect (face-attribute 'default :height) :to-equal 160))))

(describe "vui-html--font-attributes"
  (it "accepts a family name"
    (expect (vui-html--font-attributes "TestFont")
            :to-equal '(:family "TestFont")))

  (it "converts :size in a plist to :height"
    (expect (vui-html--font-attributes '(:family "TestFont" :size 12.5 :weight bold))
            :to-equal '(:family "TestFont" :height 125 :weight bold)))

  (it "keeps an absolute or relative :height as is"
    (expect (vui-html--font-attributes '(:height 1.2))
            :to-equal '(:height 1.2))))

(provide 'vui-html-test)
;;; vui-html-test.el ends here
