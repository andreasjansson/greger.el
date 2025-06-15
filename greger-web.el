;;; greger-web.el --- Web content handling for greger -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Functions for downloading web content and extracting text from URLs.
;; Originally part of greger-parser.el, moved here for better organization.

;;; Code:

(require 'url)
(require 'dom)
(require 'eww)

(defun greger-web-text-from-url (url &optional use-highest-readability)
  "Retrieve the text content from URL.
If USE-HIGHEST-READABILITY is non-nil, use eww's readability heuristics."
  (with-current-buffer
      (url-retrieve-synchronously url t nil 10.0)
    ;; Skip HTTP headers - they end with a double newline
    (goto-char (point-min))
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (delete-region (point-min) (point)))

    ;; Parse the HTML content
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (when use-highest-readability
        (setq dom (eww-highest-readability dom))
        (eww-score-readability dom))
      (greger-web-dom-texts-inline-aware dom))))

(defun greger-web-dom-texts-inline-aware (node &optional block-separator inline-separator)
  "Extract text from the DOM NODE, aware of inline and block elements.
BLOCK-SEPARATOR separates block elements.
INLINE-SEPARATOR separates inline elements."
  (let ((block-separator (or block-separator "\n"))
        (inline-separator (or inline-separator " ")))
    (mapconcat
     (lambda (elem)
       (cond
        ((stringp elem)
         (when (> (length (string-trim elem)) 0)
           elem))
        ((memq (dom-tag elem) '(head meta script style details footer)) "")
        ((memq (dom-tag elem) '(p div h1 h2 h3 h4 h5 h6 pre br hr ul ol li))
         (concat (greger-web-dom-texts-inline-aware elem block-separator inline-separator)
                 block-separator))
        (t
         (greger-web-dom-texts-inline-aware elem block-separator inline-separator))))
     (dom-children node)
     inline-separator)))

(defun greger-web-is-web-url-p (path)
  "Return non-nil if PATH is a web URL (starts with http:// or https://)."
  (or (string-prefix-p "http://" path)
      (string-prefix-p "https://" path)))

(defun greger-web-download-page (url &optional extract-text use-highest-readability)
  "Download webpage from URL and optionally extract text.
If EXTRACT-TEXT is non-nil (default t), extract and return text content.
If EXTRACT-TEXT is nil, return raw HTML.
If USE-HIGHEST-READABILITY is non-nil, use eww's aggressive readability setting."
  (if extract-text
      (greger-web-text-from-url url use-highest-readability)
    ;; Return raw HTML
    (with-current-buffer
        (url-retrieve-synchronously url t nil 10.0)
      ;; Skip HTTP headers - they end with a double newline
      (goto-char (point-min))
      (when (re-search-forward "\r?\n\r?\n" nil t)
        (delete-region (point-min) (point)))
      (buffer-string))))

(provide 'greger-web)

;;; greger-web.el ends here
