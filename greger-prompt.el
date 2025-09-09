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

Don't add fallbacks, guards, or try/catch blocks against errors unless I specifically ask you to.

## Imports go at the top

Don't sprinkle the code with imports unless in very special circumstances or if I explicltly ask you to.

## No unnecessary comments

You have a tendency to comment every line. I'm an expert programmer and I prefer reading code to prose. Most of the time I can read the code faster than the comment to understand what's going on. Only add comments in cases where the code is unintuitive or particularly hard to follow (e.g. custom parsers).

## Read full files

The first time you read a source code file, read the full file so you get a real holistic understanding of the logic.

## Today's date

Today's date is " (format-time-string "%B %d, %Y") ".
"))

(provide 'greger-prompt)

;;; greger-prompt.el ends here
