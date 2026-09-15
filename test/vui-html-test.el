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

;;

;;; Code:

(require 'buttercup)
(require 'vui-html)

(describe "vui-html.el"
          (it "loads successfully"
              (expect (featurep 'vui-html) :to-be-truthy)))

(describe "vui-html"
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
                                  #s(vui-vnode-text nil #("hello world" 6 11 (face vui-strong)) nil nil)))))

(describe "vui-html"
          (it "parse <h1> HTML VUI </h1>"
              (expect (vui-html "<h1> HTML VUI </h1>") :to-equal
                      (vui-heading-1 "HTML VUI"))))

(describe "vui-html"
          (it "parse <a href=\"http://qiqijin.com\">Qiqi</a>"
              (expect (vui-html "<a href=\"http://qiqijin.com\">Qiqi</a>") :to-equal
                      `(vui-button "Qiqi" :on-click (lambda () (browse-url "http://qiqijin.com"))))))

(provide 'vui-html-test)
;;; vui-html-test.el ends here
