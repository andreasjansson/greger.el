;;; greger-ui.el --- Claude client for greger -*- lexical-binding: t -*-

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
;; Greger UI components for folding, etc.

;;; Code:

(require 'treesit)
(require 'diff)
(require 'diff-mode)

(require 'greger-parser)

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

(defvar greger-ui-folding-mode t)

(defun greger-ui-toggle-folding ()
  "Toggle greger-ui-folding-mode and re-fontify the buffer."
  (interactive)
  (setq greger-ui-folding-mode (not greger-ui-folding-mode))
  (font-lock-flush (point-min) (point-max))
  (message "Greger UI folding mode: %s" (if greger-ui-folding-mode "enabled" "disabled")))

;; Folding and hiding

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
         (should-fold (and greger-ui-folding-mode
                          (not (get-text-property text-start 'greger-ui-citation-expanded))))
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
            ;; uncle always invisible when folding mode is on
            (put-text-property (treesit-node-end uncle-last-citation-entry) node-start 'invisible 
                              (and greger-ui-folding-mode should-fold))

          (let* ((uncle-last-child (treesit-node-child uncle -1))
                 (uncle-last-child-end (treesit-node-end uncle-last-child)))
            (put-text-property (- uncle-last-child-end 2) text-start 'invisible greger-ui-folding-mode)))))

    (when (and aunt (equal (treesit-node-type aunt) "assistant"))
      (let* ((aunt-header (treesit-search-subtree aunt "assistant_header"))
             (aunt-header-end (treesit-node-end aunt-header)))
        (setq invisible-end (+ aunt-header-end 2))))

    (put-text-property (+ invisible-start 0) invisible-end 'invisible should-fold)))

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
                 (is-tail-visible (or (get-text-property tail-start 'greger-ui-tool-content-expanded)
                                      (not greger-ui-folding-mode)))
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
NODE is the matched tree-sitter node"
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (is-visible (get-text-property node-start 'greger-ui-tool-content-expanded)))

    ;; Apply invisibility (default is invisible unless expanded, but respect global folding mode)
    (put-text-property node-start node-end 'invisible 
                       (and greger-ui-folding-mode (not is-visible)))
    (put-text-property node-start node-end 'keymap greger-ui-tool-content-tail-keymap)))

(defun greger-ui--thinking-signature-hiding-fn (node _override _start _end)
  "Hide thinking signature.  NODE is the matched tree-sitter node."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (invisible-end (+ node-end 2)))
    (put-text-property node-start invisible-end 'invisible greger-ui-folding-mode)))

(defun greger-ui--make-tool-tag-invisible (node _override _start _end)
  "Make tool tag NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (1+ (treesit-node-end node))))
    (put-text-property node-start node-end 'invisible greger-ui-folding-mode)))

(defun greger-ui--make-tool-result-id-invisible (node _override _start _end)
  "Make id NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (put-text-property node-start node-end 'invisible greger-ui-folding-mode)))

(defun greger-ui--make-tool-use-id-invisible (node _override _start _end)
  "Make id NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (1- (treesit-node-end node))))
    (put-text-property node-start node-end 'invisible greger-ui-folding-mode)))

;; Links

(defun greger-ui--url-link-fn (node _override _start _end)
  "Apply URL link properties to NODE for interactive behavior."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (url-start (+ node-start 3)))
    (put-text-property url-start node-end 'face 'greger-link-face)
    (put-text-property url-start node-end 'mouse-face 'highlight)
    (put-text-property url-start node-end 'keymap greger-ui-url-link-keymap)))

(defun greger-ui--open-url-at-point ()
  "Open URL at point in default web browser."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (text (treesit-node-text node t))

         ;; remove leading `## `
         (url (substring text 3)))
    (browse-url url)))

;; Code blocks

(defun greger-ui-copy-code ()
  "Copy code block content at point to kill ring."
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

;; TAB toggles

(defun greger-ui--toggle-citation-fold ()
  "Toggle folding of citation or tool content at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (node-start (treesit-node-start node))
         (invisible-start (get-text-property node-start 'invisible-start))
         (invisible-end (get-text-property node-start 'invisible-end))
         (is-expanded (get-text-property node-start 'greger-ui-citation-expanded)))

    (put-text-property node-start (1+ node-start) 'greger-ui-citation-expanded (not is-expanded))
    (font-lock-flush invisible-start invisible-end)))

(defun greger-ui--toggle-tool-content-head-fold ()
  "Toggle folding of citation or tool content at point."
  (interactive)
  (let* ((tail-start (get-text-property (point) 'greger-ui-tool-tail-start))
         (tail-end (get-text-property (point) 'greger-ui-tool-tail-end)))
    (when (and tail-start tail-end)
      (let ((is-tail-visible (get-text-property tail-start 'greger-ui-tool-content-expanded))
            (inhibit-read-only t))
        (put-text-property tail-start (min (1+ tail-start) tail-end) 'greger-ui-tool-content-expanded (not is-tail-visible))
        ;; Also need to flush both head and tail for overlay updates
        (font-lock-flush (point) tail-end)))))

(defun greger-ui--toggle-tool-content-tail-fold ()
  "Toggle folding of citation or tool content at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (tail-start (treesit-node-start node))
         (tail-end (treesit-node-end node))
         (is-tail-visible (get-text-property tail-start 'greger-ui-tool-content-expanded))
         (parent (treesit-node-parent node))
         (head-node (treesit-search-subtree parent "^tool_content_head$" nil nil 1))
         (inhibit-read-only t))

    (if is-tail-visible
        (progn
          (put-text-property tail-start (1+ tail-start) 'greger-ui-tool-content-expanded nil)
          (font-lock-flush (treesit-node-start head-node) tail-end))

      (put-text-property tail-start (1+ tail-start) 'greger-ui-tool-content-expanded t)
      (font-lock-flush (treesit-node-start head-node) tail-end))))

;; Str-replace diff content replacement functionality

(defun greger-ui--get-str-replace-fontified-overlay (tool-use-node)
  "Get cached overlay for str-replace transformation between START and END."
  (let* ((node-start (treesit-node-start tool-use-node))
         (overlays (overlays-at node-start)))
    (seq-find (lambda (ov) (overlay-get ov 'greger-ui-str-replace-fontified)) overlays)))

(defun greger-ui--create-str-replace-fontified-overlay (tool-use-node)
  "Create a cached overlay for str-replace transformation between START and END with CACHE-KEY."
  ;; Clean up any old overlay first
  (when-let ((old-overlay (greger-ui--get-str-replace-fontified-overlay tool-use-node)))
    (delete-overlay old-overlay))
  
  ;; Create new overlay
  (let* ((node-start (treesit-node-start tool-use-node))
         (overlay (make-overlay node-start (1+ node-start))))
    (overlay-put overlay 'greger-ui-str-replace-fontified t)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun greger-ui--str-replace-diff-transform-fn (tool-use-node _override start end)
  "Font-lock function to transform str-replace original/new content into diff content.
NODE is the matched tree-sitter node for tool_use block.
Expensive operations are deferred to idle time to avoid blocking scrolling."
  (when-let* ((tool-use-name (greger-parser--extract-tool-use-name tool-use-node))
              ((string= tool-use-name "str-replace")))

    (unless (greger-ui--get-str-replace-fontified-overlay tool-use-node)
      (let* ((params (greger-parser--extract-tool-use-params tool-use-node))
             (has-diff (alist-get 'diff params))
             (has-original-new (and (alist-get 'original-content params)
                                    (alist-get 'new-content params))))
        (cond
         ;; Case 1: File already has diff parameter - defer syntax highlighting
         (has-diff
                                        ;(greger-ui--schedule-diff-operation 'highlight node start end)
          (greger-ui--apply-diff-syntax-highlighting tool-use-node start end)
          )
         ;; Case 2: File has original-content/new-content - defer diff transformation
         (has-original-new
                                        ;(greger-ui--schedule-diff-operation 'transform node start end)
          (greger-ui--apply-str-replace-diff-content tool-use-node start end)
          )
         ;; Case 3: Neither - do nothing
         (t nil))))))

(defun greger-ui--apply-str-replace-diff-content (tool-use-node _start _end)
  "Apply diff overlay to str-replace NODE content between START and END."
  (let* ((params (greger-parser--extract-tool-use-params tool-use-node))
         (original-content (alist-get 'original-content params))
         (new-content (alist-get 'new-content params))
         (path (alist-get 'path params))
         (param-nodes (greger-parser--extract-tool-use-param-nodes tool-use-node))
         (original-content-node (alist-get 'original-content param-nodes))
         (new-content-node (alist-get 'new-content param-nodes))
         (replace-start (treesit-node-start original-content-node))
         (replace-end (treesit-node-end new-content-node))
         (tool-use-id (greger-parser--extract-tool-use-id tool-use-node))
         (diff-content (greger-ui--generate-diff-content original-content new-content path))
         (wrapped-diff (greger-parser--wrapped-tool-param "diff" tool-use-id diff-content t))
         (inhibit-read-only t))

    (greger-ui--create-str-replace-fontified-overlay tool-use-node)

    ;; Replace buffer content with diff
    (goto-char replace-start)
    (delete-region replace-start replace-end)
    (insert wrapped-diff)))

(defun greger-ui--apply-diff-syntax-highlighting (tool-use-node _start _end)
  "Apply syntax highlighting to existing diff content in str-replace tool_use."
  (let* ((params (greger-parser--extract-tool-use-params tool-use-node))
         (raw-diff-content (alist-get 'diff params))
         (path (alist-get 'path params))
         (undiff-result (greger-parser-undiff-strings raw-diff-content))
         (original-content (car undiff-result))
         (new-content (cdr undiff-result))
         (param-nodes (greger-parser--extract-tool-use-param-nodes tool-use-node))
         (diff-node (alist-get 'diff param-nodes))
         (replace-start (treesit-node-start diff-node))
         (replace-end (treesit-node-end diff-node))
         (tool-use-id (greger-parser--extract-tool-use-id tool-use-node))
         (diff-content (greger-ui--generate-diff-content original-content new-content path))
         (wrapped-diff (greger-parser--wrapped-tool-param "diff" tool-use-id diff-content t))
         (inhibit-read-only t))

    (greger-ui--create-str-replace-fontified-overlay tool-use-node)

    ;; Replace buffer content with diff
    (goto-char replace-start)
    (delete-region replace-start replace-end)
    (insert wrapped-diff)))

(defun greger-ui--generate-diff-content (original new path)
  "Generate diff content from ORIGINAL to NEW for PATH using diff.el."
  (let* ((ext (or (file-name-extension path t) ""))
         (original-file (make-temp-file "greger-original-" nil ext))
         (new-file (make-temp-file "greger-new-" nil ext))
         (diff-buffer (get-buffer-create "*Greger diff*"))
         (diff-refine nil)
         (diff-switches '("-u" "-U" "100000")))

    (unwind-protect
        (progn
          ;; Write content to temp files
          (let ((coding-system-for-write 'raw-text))
            (with-temp-file original-file
              (insert original)))
          (let ((coding-system-for-write 'raw-text))
            (with-temp-file new-file
              (insert new)))

          (diff-no-select original-file new-file nil t diff-buffer)
          (with-current-buffer diff-buffer
            ;; Ensure font-lock is active and force fontification
            (font-lock-ensure (point-min) (point-max))

            ;; Turn off read-only mode and we'll do operations on the
            ;; fontified text
            (setq buffer-read-only nil)

            ;; Convert 'face to 'font-lock-face for tree-sitter compatibility
            (greger-ui--convert-faces-for-tree-sitter)

            ;; Remove headers/footers inserted by the diff command and diff.el
            (greger-ui--remove-diff-headers)
  
            ;; Make diff indicators (space, minus, plus) less prominent
            (greger-ui--apply-diff-deemphasis)

            (buffer-string)))
      
      ;; Cleanup temp files
      (when (file-exists-p original-file) (delete-file original-file))
      (when (file-exists-p new-file) (delete-file new-file))
      (kill-buffer diff-buffer)
      )))

(defun greger-ui--convert-faces-for-tree-sitter ()
  "Convert 'face text properties and overlay faces to 'font-lock-face for tree-sitter."
  (let* ((background-mode (frame-parameter nil 'background-mode))
         (is-dark-theme (eq background-mode 'dark))
         (red-bg (if is-dark-theme "#32171E" "#ffe6e6")) ; Dark red vs light red
         (green-bg (if is-dark-theme "#173714" "#e6ffe6"))) ; Dark green vs light green
    
    ;; First pass: Set background on all characters in diff lines
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line-start-char (char-after line-start)))
          (cond
           ;; Lines starting with - get red background
           ((eq line-start-char ?-)
            (put-text-property (1+ line-start) line-end 'font-lock-face
                               (list :background red-bg)))
           ;; Lines starting with + get green background
           ((eq line-start-char ?+)
            (put-text-property (1+ line-start) line-end 'font-lock-face
                               (list :background green-bg))))
          (forward-line 1))))
    
    ;; Second pass: Convert overlays with 'face property (syntax highlighting overlays)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when-let* ((face (overlay-get overlay 'face))
                  (face-attrs (greger-ui--get-face-attributes face))
                  (start (overlay-start overlay))
                  (end (overlay-end overlay)))
        
        ;; Check if this overlay spans any diff lines and modify background accordingly
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (let* ((line-start (line-beginning-position))
                   (line-end (min (line-end-position) end))
                   (overlay-start-in-line (max start line-start))
                   (overlay-end-in-line (min end (1+ line-end)))
                   (line-start-char (char-after line-start)))

              (cond
               ;; Lines starting with - get red background
               ((eq line-start-char ?-)
                (plist-put face-attrs :background red-bg)
                ;; TODO: remove debug
                (put-text-property overlay-start-in-line overlay-end-in-line
                                   'font-lock-face face-attrs))
               ;; Lines starting with + get green background
               ((eq line-start-char ?+)
                (plist-put face-attrs :background green-bg)
                (put-text-property overlay-start-in-line overlay-end-in-line
                                   'font-lock-face face-attrs))
               ;; Other lines get just the overlay face
               (t
                ;; TODO: remove debug
                (put-text-property overlay-start-in-line overlay-end-in-line
                                   'font-lock-face face))))
            (forward-line 1))))))
  
  ;; Mark as fontified to prevent re-fontification
  (add-text-properties (point-min) (point-max) '(fontified t)))

(defun greger-ui--get-face-attributes (face)
  "Get all non-unspecified attributes of FACE as a plist."
  (let ((attrs '(:family :foundry :width :height :weight :slant
                 :underline :overline :strike-through :box
                 :inverse-video :foreground :background
                 :stipple :font :inherit))
        result)
    (dolist (attr attrs)
      (let ((value (face-attribute face attr nil t)))
        (when (not (eq value 'unspecified))
          (setq result (plist-put result attr value)))))
    result))

(defun greger-ui--remove-diff-headers ()
  "Process diff output, remove headers and apply syntax highlighting."
  (goto-char (point-min))
  (when (looking-at "^diff -u")
    (delete-region (point) (progn (forward-line 1) (point))))
  (when (looking-at "^--- ")
    (delete-region (point) (progn (forward-line 1) (point))))
  (when (looking-at "^\\+\\+\\+ ")
    (delete-region (point) (progn (forward-line 1) (point))))
  
  ;; Remove hunk headers (@@ lines)
  (goto-char (point-min))
  (while (re-search-forward "^@@.*@@\\s-*\n" nil t)
    (replace-match ""))

  (goto-char (point-max))
  (re-search-backward "\nDiff finished" nil t)
  (delete-region (point) (point-max)))

(defun greger-ui--apply-diff-deemphasis ()
  "Apply visual de-emphasis to diff indicators.
Makes indicators small and muted while keeping them readable."
  ;; Make "\ No newline at end of file" messages less prominent
  (goto-char (point-min))
  (while (re-search-forward "^\\\\ No newline at end of file$" nil t)
    (put-text-property (line-beginning-position) (1+ (line-end-position))
                       'font-lock-face '(:height 0.6 :foreground "gray50"))))

;; Tool result syntax highlighting

(defun greger-ui--tool-result-syntax-highlighting (tool-result-node _override _start _end)
  "Apply source language syntax highlighting to TOOL-RESULT-NODE based on corresponding tool_use.
NODE is the matched tree-sitter node, OVERRIDE, START, and END are font-lock parameters."
  (unless (get-char-property (treesit-node-start tool-result-node) 'greger-ui--tool-result-syntax-fontified)
    (greger-ui--fontify-tool-result-content tool-result-node)
    ;; Mark as fontified to prevent re-processing
    (let ((tool-result-start (treesit-node-start tool-result-node))
          (tool-result-end (treesit-node-end tool-result-node)))
      (put-text-property tool-result-start tool-result-end 'greger-ui--tool-result-syntax-fontified t))))

(defun greger-ui--fontify-tool-result-content (tool-result-node)
  "Apply syntax highlighting to TOOL-RESULT-NODE content based on the corresponding tool_use."
  (when-let* ((tool-use-node (greger-ui--find-corresponding-tool-use tool-result-node))
              (tool-name (greger-parser--extract-tool-use-name tool-use-node)))
    
    (message "DEBUG: Found tool %s for result highlighting" tool-name)
    
    ;; Extract content using existing parser function  
    (let* ((content-wrapper-node (treesit-search-subtree tool-result-node "content" nil nil 1))
           (content-text (when content-wrapper-node
                           (greger-parser--extract-tool-result-content content-wrapper-node)))
           ;; For tool_result, the structure is: content -> value -> tool_start_tag + tool_content + tool_end_tag  
           (value-node (when content-wrapper-node
                         (treesit-node-child-by-field-name content-wrapper-node "value")))
           (content-node (when value-node
                           (treesit-search-subtree value-node "tool_content" nil nil 1)))
           (content-start (when content-node (treesit-node-start content-node)))
           (content-end (when content-node (treesit-node-end content-node))))
      
      (message "DEBUG: Content extracted - start: %s, end: %s, text length: %s" 
               content-start content-end (when content-text (length content-text)))
      
      (when (and content-text content-start content-end)
        (cond
         ;; Handle read-file results - detect file extension from tool_use path parameter
         ((string= tool-name "read-file")
          (let* ((tool-use-params (greger-parser--extract-tool-use-params tool-use-node))
                 (file-path (alist-get 'path tool-use-params)))
            (message "DEBUG: File path: %s" file-path)
            (when file-path
              (greger-ui--apply-syntax-highlighting content-start content-end content-text file-path))))
         
         ;; Handle list-directory results - treat as text/plain, no special highlighting
         ((string= tool-name "list-directory")
          ;; Directory listings don't need special syntax highlighting
          nil)
         
         ;; Handle shell-command results - treat as shell output
         ((string= tool-name "shell-command")
          ;; Could potentially highlight as shell output, but for now just leave as-is
          nil)
         
         ;; Handle other tools - no special highlighting for now
         (t nil))))))

(defun greger-ui--find-corresponding-tool-use (tool-result-node)
  "Find the tool_use node that corresponds to TOOL-RESULT-NODE by matching IDs."
  (when-let* ((tool-result-id (greger-parser--extract-tool-result-id tool-result-node))
              (root (treesit-buffer-root-node)))
    
    ;; Search for tool_use nodes with matching ID
    (treesit-search-subtree 
     root
     (lambda (node)
       (and (string= (treesit-node-type node) "tool_use")
            (when-let ((tool-use-id (greger-parser--extract-tool-use-id node)))
              (string= tool-use-id tool-result-id))))
     nil nil 1)))

(defun greger-ui--detect-major-mode-from-path (file-path)
  "Detect the appropriate major mode for FILE-PATH based on file extension."
  (let ((case-fold-search t))
    (cond
     ;; Programming languages
     ((string-match-p "\\.py\\'" file-path) 'python-mode)
     ((string-match-p "\\.js\\'" file-path) 'js-mode)
     ((string-match-p "\\.ts\\'" file-path) 'typescript-mode)
     ((string-match-p "\\.el\\'" file-path) 'emacs-lisp-mode)
     ((string-match-p "\\.\\(c\\|h\\)\\'" file-path) 'c-mode)
     ((string-match-p "\\.\\(cpp\\|cxx\\|cc\\|hpp\\|hxx\\)\\'" file-path) 'c++-mode)
     ((string-match-p "\\.java\\'" file-path) 'java-mode)
     ((string-match-p "\\.go\\'" file-path) 'go-mode)
     ((string-match-p "\\.rs\\'" file-path) 'rust-mode)
     ((string-match-p "\\.rb\\'" file-path) 'ruby-mode)
     ((string-match-p "\\.php\\'" file-path) 'php-mode)
     ((string-match-p "\\.swift\\'" file-path) 'swift-mode)
     ((string-match-p "\\.kt\\'" file-path) 'kotlin-mode)
     ((string-match-p "\\.scala\\'" file-path) 'scala-mode)
     ((string-match-p "\\.clj\\'" file-path) 'clojure-mode)
     ((string-match-p "\\.hs\\'" file-path) 'haskell-mode)
     ((string-match-p "\\.ml\\'" file-path) 'ocaml-mode)
     ((string-match-p "\\.f\\(90\\|95\\|03\\|08\\)?\\'" file-path) 'fortran-mode)
     ((string-match-p "\\.r\\'" file-path) 'r-mode)
     ((string-match-p "\\.jl\\'" file-path) 'julia-mode)
     ((string-match-p "\\.dart\\'" file-path) 'dart-mode)
     
     ;; Web technologies
     ((string-match-p "\\.html?\\'" file-path) 'html-mode)
     ((string-match-p "\\.css\\'" file-path) 'css-mode)
     ((string-match-p "\\.scss\\'" file-path) 'scss-mode)
     ((string-match-p "\\.less\\'" file-path) 'less-css-mode)
     ((string-match-p "\\.vue\\'" file-path) 'vue-mode)
     ((string-match-p "\\.jsx?\\'" file-path) 'js-mode)
     ((string-match-p "\\.tsx?\\'" file-path) 'typescript-mode)
     
     ;; Data formats
     ((string-match-p "\\.json\\'" file-path) 'json-mode)
     ((string-match-p "\\.ya?ml\\'" file-path) 'yaml-mode)
     ((string-match-p "\\.toml\\'" file-path) 'toml-mode)
     ((string-match-p "\\.xml\\'" file-path) 'xml-mode)
     ((string-match-p "\\.csv\\'" file-path) 'csv-mode)
     
     ;; Configuration files
     ((string-match-p "\\.ini\\'" file-path) 'ini-mode)
     ((string-match-p "\\.conf\\'" file-path) 'conf-mode)
     ((string-match-p "Dockerfile\\'" file-path) 'dockerfile-mode)
     ((string-match-p "\\.env\\'" file-path) 'conf-mode)
     
     ;; Shell scripts
     ((string-match-p "\\.\\(sh\\|bash\\|zsh\\|fish\\)\\'" file-path) 'shell-script-mode)
     
     ;; Documentation
     ((string-match-p "\\.md\\'" file-path) 'markdown-mode)
     ((string-match-p "\\.rst\\'" file-path) 'rst-mode)
     ((string-match-p "\\.tex\\'" file-path) 'latex-mode)
     ((string-match-p "\\.org\\'" file-path) 'org-mode)
     
     ;; SQL
     ((string-match-p "\\.sql\\'" file-path) 'sql-mode)
     
     ;; Default to text-mode
     (t 'text-mode))))

(defun greger-ui--apply-syntax-highlighting (start end content major-mode)
  "Apply syntax highlighting for MAJOR-MODE to CONTENT between START and END."
  ;; Skip highlighting for very long content to avoid performance issues
  (when (< (length content) 50000)
    (message "DEBUG: Applying syntax highlighting with mode %s" major-mode)
    ;; Create a temporary buffer to get proper syntax highlighting
    (let ((temp-buffer (generate-new-buffer " *greger-syntax-temp*"))
          (original-buffer (current-buffer)))
      (unwind-protect
          (with-current-buffer temp-buffer
            ;; Insert content and enable the appropriate major mode
            (insert content)
            (message "DEBUG: Inserted %d characters into temp buffer" (length content))
            
            ;; Try to enable the major mode, fall back to text-mode if it fails
            (condition-case err
                (progn
                  (funcall major-mode)
                  (message "DEBUG: Successfully enabled %s" major-mode))
              (error 
               (message "DEBUG: Failed to enable %s: %s, falling back to text-mode" major-mode err)
               (text-mode)))
            
            ;; Enable font-lock and force fontification
            (when (fboundp 'font-lock-mode)
              (font-lock-mode 1)
              (font-lock-ensure (point-min) (point-max))
              (message "DEBUG: Font-lock enabled and ensured"))
            
            ;; Check if there are any face properties in temp buffer
            (let ((faces-found 0))
              (save-excursion
                (goto-char (point-min))
                (while (< (point) (point-max))
                  (when (get-text-property (point) 'face)
                    (setq faces-found (1+ faces-found)))
                  (forward-char 1)))
              (message "DEBUG: Found %d characters with face properties in temp buffer" faces-found))
            
            ;; Copy the font-lock-face properties to the original buffer
            (let ((temp-start (point-min))
                  (temp-end (point-max))
                  (original-pos start)
                  (properties-copied 0))
              
              (while (< temp-start temp-end)
                (let* ((next-change (or (next-single-property-change temp-start 'face (current-buffer) temp-end)
                                        temp-end))
                       (face (get-text-property temp-start 'face))
                       (len (- next-change temp-start)))
                  
                  (when face
                    ;; Apply the face to the corresponding position in the original buffer
                    (with-current-buffer original-buffer
                      (put-text-property original-pos (min (+ original-pos len) end) 'font-lock-face face))
                    (setq properties-copied (1+ properties-copied)))
                  
                  (setq temp-start next-change
                        original-pos (+ original-pos len))))
              (message "DEBUG: Copied %d face properties to original buffer" properties-copied)))
        
        ;; Clean up temp buffer
        (kill-buffer temp-buffer)))))

(provide 'greger-ui)
;;; greger-ui.el ends here
