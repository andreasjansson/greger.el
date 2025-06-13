;;; greger-ui.el --- Claude client for greger -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

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
;; Greger UI components for folding, etc.

;;; Code:

(require 'treesit)

(defvar greger-ui-citation-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'greger-ui--toggle-citation-fold)
    (define-key map (kbd "TAB") #'greger-ui--toggle-citation-fold)
    map)
  "Keymap for citation text to handle mouse clicks.")

(defvar greger-ui-url-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'greger-ui--open-url-at-point)
    (define-key map (kbd "RET") #'greger-ui--open-url-at-point)
    map))

(defvar greger-ui-tool-content-head-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'greger-ui--toggle-tool-content-head-fold)
    (define-key map (kbd "TAB") #'greger-ui--toggle-tool-content-head-fold)
    map))

(defvar greger-ui-tool-content-tail-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'greger-ui--toggle-tool-content-tail-fold)
    (define-key map (kbd "TAB") #'greger-ui--toggle-tool-content-tail-fold)
    map))

;; Folding

(defun greger-ui--citation-entry-folding-fn (node _override _start _end)
  "Font-lock function to hide citation entries within assistant blocks.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (parent (treesit-node-parent node))
         (first-text (treesit-search-subtree parent "^text$" nil nil 1))
         (last-text (treesit-search-subtree parent "^text$" t nil 1))
         (text-start (treesit-node-start first-text))
         (text-end (- (treesit-node-end last-text) 2))
         (uncle (treesit-node-prev-sibling parent))
         (aunt (treesit-node-next-sibling parent))
         (should-fold (not (get-text-property text-start 'greger-ui-citation-expanded)))
         (invisible-start text-end)
         (invisible-end (- node-end 2)))

    (put-text-property text-start text-end 'face 'greger-citation-face)
    (put-text-property text-start text-end 'greger-ui-expandable-citation-entry t)
    (put-text-property text-start text-end 'mouse-face 'highlight)
    (put-text-property text-start text-end 'keymap greger-ui-citation-keymap)
    (put-text-property text-start text-end 'invisible-start invisible-start)
    (put-text-property text-start text-end 'invisible-end invisible-end)

    (when (and uncle (equal (treesit-node-type uncle) "assistant"))
      (let* ((uncle-last-citation-entry (treesit-search-subtree uncle "^citation_entry$" t nil 1)))

        (if uncle-last-citation-entry
            ;; uncle always invisible
            (put-text-property (treesit-node-end uncle-last-citation-entry) node-start 'invisible should-fold)

          (let* ((uncle-last-child (treesit-node-child uncle -1))
                 (uncle-last-child-end (treesit-node-end uncle-last-child)))
            (put-text-property (- uncle-last-child-end 2) text-start 'invisible t)))

        ))

    (when (and aunt (equal (treesit-node-type aunt) "assistant"))
      (let* ((aunt-header (treesit-search-subtree aunt "assistant_header"))
             (aunt-header-end (treesit-node-end aunt-header)))
        (setq invisible-end (+ aunt-header-end 2))))

    (put-text-property (+ invisible-start 0) invisible-end 'invisible should-fold)
    ))

(defun greger-ui--tool-content-head-folding-fn (node _override _start _end)
  "Font-lock function to make tool_content_head TAB-able for tail visibility.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (parent (treesit-node-parent node)))
    
    (when parent
      ;; Find the corresponding tail safely
      (let ((tail-node (treesit-search-subtree parent "^tool_content_tail$" nil nil 1)))
        (when tail-node
          (let* ((tail-start (treesit-node-start tail-node))
                 (tail-end (treesit-node-end tail-node))
                 (is-tail-visible (get-text-property tail-start 'greger-ui-tool-content-expanded))
                 (line-count (max 1 (count-lines tail-start tail-end))))
            
            ;; Mark the head as foldable and store tail info
            (put-text-property node-start node-end 'greger-ui-foldable-tool-content t)
            (put-text-property node-start node-end 'greger-ui-tool-tail-start tail-start)
            (put-text-property node-start node-end 'greger-ui-tool-tail-end tail-end)
            (put-text-property node-start node-end 'keymap greger-ui-tool-content-head-keymap)
            
            ;; Clean up old overlays first
            (let ((old-overlay (get-text-property node-start 'greger-ui-fold-overlay)))
              (when (and old-overlay (overlayp old-overlay))
                (delete-overlay old-overlay)
                (remove-text-properties node-start node-end '(greger-ui-fold-overlay nil))))
            
            ;; Add overlay with fold indicator when tail is hidden
            (unless is-tail-visible
              (let ((overlay (make-overlay (-  node-end 2) (1- node-end))))
                (overlay-put overlay 'after-string 
                             (propertize (format "\n[+%d lines, TAB to expand]" line-count)
                                         'face '(:foreground "gray" :height 0.8 :slant italic)))
                (overlay-put overlay 'greger-ui-fold-overlay t)
                (overlay-put overlay 'evaporate t)
                ;; Store overlay reference for cleanup
                (put-text-property node-start node-end 'greger-ui-fold-overlay overlay)))))))))

(defun greger-ui--tool-content-tail-folding-fn (node _override _start _end)
  "Font-lock function to make tool_content_tail invisible by default.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (is-visible (get-text-property node-start 'greger-ui-tool-content-expanded)))
    
    ;; Apply invisibility (default is invisible unless expanded)
    (put-text-property node-start node-end 'invisible (not is-visible))
    (put-text-property node-start node-end 'keymap greger-ui-tool-content-tail-keymap)))

;; Links

(defun greger-ui--url-link-fn (node _override _start _end)
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (url-start (+ node-start 3)))
    (put-text-property url-start node-end 'face 'greger-link-face)
    (put-text-property url-start node-end 'mouse-face 'highlight)
    (put-text-property url-start node-end 'keymap greger-ui-url-link-keymap)))

(defun greger-ui--open-url-at-point ()
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (text (treesit-node-text node t))

         ;; remove leading `## `
         (url (substring text 3)))
    (browse-url url)))

"Make URLs clickable by adding mouse highlighting and keybindings to NODE.
Processes URL nodes from tree-sitter to add interactive properties,
ignoring OVERRIDE, START, and END parameters."
  "Open URL at point in default web browser.
Finds the URL node under cursor and launches it, stripping the markdown
URL prefix before opening."
  ;; Code blocks

(defun greger-ui--copy-code ()
  (interactive)
  (let ((node (treesit-node-at (point))))
    (if (string= (treesit-node-type node) "code_block_content")
        (let ((code (treesit-node-text node t)))
              (kill-new code)
              (message "Copied code:\n%s" (greger-ui--truncate-with-ellipsis code 40)))
      (message "Not inside a code block"))))

(defun greger-ui--truncate-with-ellipsis (str max-width)
  "Truncate STR to MAX-WIDTH characters, adding an ellipsis if necessary."
  (let ((len (length str)))
    (if (<= len max-width)
        str
      (concat (substring str 0 (- max-width 3)) "..."))))

"Copy code block content at point to kill ring.
Finds code block under cursor and copies the content for pasting elsewhere,
showing a truncated preview of what was copied."
  ;; TAB toggles

(defun greger-ui--toggle-citation-fold ()
  "Toggle folding of citation or tool content at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (node-start (treesit-node-start node))
         (invisible-start (get-text-property node-start 'invisible-start))
         (invisible-end (get-text-property node-start 'invisible-end))
         (is-expanded (get-text-property node-start 'greger-ui-citation-expanded)))

    ;; TODO: remove debug
    (message (format "node: %s" node))

    (put-text-property node-start (1+ node-start) 'greger-ui-citation-expanded (not is-expanded))
    (font-lock-flush invisible-start invisible-end)))

(defun greger-ui--toggle-tool-content-head-fold ()
  "Toggle folding of citation or tool content at point."
  (interactive)
  (let* ((tail-start (get-text-property (point) 'greger-ui-tool-tail-start))
         (tail-end (get-text-property (point) 'greger-ui-tool-tail-end)))
    (when (and tail-start tail-end)
      (let ((is-tail-visible (get-text-property tail-start 'greger-ui-tool-content-expanded)))
        (put-text-property tail-start (min (1+ tail-start) tail-end) 'greger-ui-tool-content-expanded (not is-tail-visible))
        ;; Also need to flush both head and tail for overlay updates
        (font-lock-flush (point) tail-end)))))

(defun greger-ui--toggle-tool-content-tail-fold ()
  "Toggle folding of citation or tool content at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (tail-start (treesit-node-start node))
         (tail-end (treesit-node-end node))
         (is-tail-visible (get-text-property tail-start 'greger-ui-tool-content-expanded)))

    (when is-tail-visible
      (put-text-property tail-start (1+ tail-start) 'greger-ui-tool-content-expanded nil)
      ;; Find the corresponding head to flush it too for overlay updates
      (let* ((parent (treesit-node-parent node))
             (head-node (treesit-search-subtree parent "^tool_content_head$" nil nil 1)))
        (when head-node
          (font-lock-flush (treesit-node-start head-node) tail-end))))))


(provide 'greger-ui)
