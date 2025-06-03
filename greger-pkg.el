;;; greger-pkg.el --- Package definition for greger -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson
;; SPDX-License-Identifier: MIT

;;; Code:

(define-package "greger" "0.1.0"
  "Chat with language models"
  '((emacs "28.1")
    (markdown-mode "2.3"))
  :keywords '("ai" "chat" "language-models" "tools")
  :url "https://github.com/andreasjansson/greger.el"
  :maintainer '("Andreas Jansson" . "andreas@jansson.me.uk")
  :authors '(("Andreas Jansson" . "andreas@jansson.me.uk")))
