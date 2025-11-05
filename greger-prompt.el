;;; greger-prompt.el --- Off-the-shelf prompts for greger -*- lexical-binding: t -*-

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
;; Constants containing reusable prompts for greger.el.

;;; Code:

(defconst greger-prompt-code-style (concat "
# Code style

## Don't be overly defensive

I don't trust the code you write, so I want you to fail early and loudly.

**Don't** add fallbacks, guards, or try/catch blocks against errors unless I specifically ask you to. I don't trust the code you write so I'd rather fail hard early and detect your errors than softly receive a fallback and not detect your errors.

## Imports go at the top

Don't sprinkle the code with imports unless in very special circumstances or if I explicltly ask you to.

## No unnecessary comments

You have a tendency to comment every line. I'm an expert programmer and I prefer reading code to prose. Most of the time I can read the code faster than the comment to understand what's going on. Only add comments in cases where the code is unintuitive or particularly hard to follow (e.g. custom parsers).

## Read full files

Important: The first time you read a source code file, read the _entire_ file (_not_ piecemeal with start-line and end-line) so you get a real holistic understanding of the logic.

## IMPORTANT: NO SUMMARY DOCUMENTS!!!

IMPORTANT: DON'T write summary documents (e.g. SUMMARY.md, FINAL_REPORT.md) unless the user explicitly asks you to. If you feel the need to explain something, just do it in the chat. The ONLY exception to this rule is if the user explicitly asks you to write up a markdown file on disk.
"))

(provide 'greger-prompt)

;;; greger-prompt.el ends here
