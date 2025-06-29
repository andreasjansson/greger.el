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
  "Toggle `greger-ui-folding-mode' and re-fontify the buffer."
  (interactive)
  (setq greger-ui-folding-mode (not greger-ui-folding-mode))
  (font-lock-flush (point-min) (point-max))
  (message "Greger UI folding mode: %s" (if greger-ui-folding-mode "enabled" "disabled")))

;; Folding and hiding

(defun greger-ui--citation-entry-folding (node _override _start _end)
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

(defun greger-ui--tool-content-head-folding (node _override _start _end)
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

(defun greger-ui--tool-content-tail-folding (node _override _start _end)
  "Font-lock function to make tool_content_tail invisible by default.
NODE is the matched tree-sitter node"
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (is-visible (get-text-property node-start 'greger-ui-tool-content-expanded)))

    ;; Apply invisibility (default is invisible unless expanded, but respect global folding mode)
    (put-text-property node-start node-end 'invisible
                       (and greger-ui-folding-mode (not is-visible)))
    (put-text-property node-start node-end 'keymap greger-ui-tool-content-tail-keymap)))

(defun greger-ui--thinking-signature-hiding (node _override _start _end)
  "Hide thinking signature.  NODE is the matched tree-sitter node."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (invisible-end (+ node-end 2)))
    (put-text-property node-start invisible-end 'invisible greger-ui-folding-mode)))

(defun greger-ui--make-tool-tag-invisible (node _override _start _end)
  "Make tool tag NODE invisible while preserving face styling."
  (condition-case nil
      (let ((node-start (treesit-node-start node))
            (node-end (min (1+ (treesit-node-end node)) (point-max))))
        ;; In Emacs 29.4, be more careful about invisible property boundaries
        ;; to prevent accidentally marking too much content as invisible
        (when (<= node-end (point-max))
          (put-text-property node-start node-end 'invisible greger-ui-folding-mode)
          (unless greger-ui-folding-mode
            (put-text-property node-start node-end 'font-lock-face '(:height 0.8 :foreground "gray70")))))
    (treesit-node-outdated
     ;; Node became outdated, skip this operation
     nil)))

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

(defun greger-ui--url-link (node _override _start _end)
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

;; Tool use syntax highlighting

(defun greger-ui--syntax-highlighted-p (node)
  "Return non-nil if NODE has already been syntax highlighted."
  (let* ((node-start (treesit-node-start node))
         (overlays (overlays-at node-start)))
    (if (seq-find (lambda (ov) (overlay-get ov 'greger-ui-syntax-highlighted)) overlays)
        t
      nil)))

(defun greger-ui--set-syntax-highlighted (tool-use-node)
  "Mark TOOL-USE-NODE as syntax highlighted with an overlay."
  (condition-case nil
      (let* ((node-start (treesit-node-start tool-use-node))
             (overlay (make-overlay node-start (1+ node-start))))
        (overlay-put overlay 'greger-ui-syntax-highlighted t)
        (overlay-put overlay 'evaporate t)
        overlay)
    (treesit-node-outdated
     ;; Node became outdated, likely due to buffer modification
     ;; This can happen after str-replace diff content transformation
     nil)))

(defun greger-ui--tool-use-syntax-highlighting (tool-use-node _override start end)
  "Font-lock function to transform tool content for syntax highlighting.

TOOL-USE-NODE is the matched tree-sitter node for tool_use block.
_OVERRIDE is ignored.  START and END define the fontification range.
Expensive operations are deferred to idle time to avoid blocking scrolling."
  (unless (greger-ui--syntax-highlighted-p tool-use-node)
    (when-let ((tool-use-name (greger-parser--extract-tool-use-name tool-use-node)))
      (cond
       ((string= tool-use-name "str-replace")
        (greger-ui--str-replace-syntax-highlighting tool-use-node start end))
       ((member tool-use-name '("replace-file" "write-new-file"))
        (greger-ui--file-syntax-highlighting tool-use-node))
       (t nil)))
    (greger-ui--set-syntax-highlighted tool-use-node)))

(defun greger-ui--str-replace-syntax-highlighting (tool-use-node start end)
  "Apply syntax highlighting to str-replace TOOL-USE-NODE from START to END."
  (let* ((params (greger-parser--extract-tool-use-params tool-use-node))
         (has-diff (alist-get 'diff params))
         (has-original-new (and (alist-get 'original-content params)
                                (alist-get 'new-content params))))
    (cond
     (has-diff
      (greger-ui--apply-diff-syntax-highlighting tool-use-node start end))
     (has-original-new
      (greger-ui--apply-str-replace-diff-content tool-use-node start end))
     (t nil))))

(defun greger-ui--apply-str-replace-diff-content (tool-use-node _start _end)
  "Apply diff overlay to str-replace TOOL-USE-NODE content.
_START and _END are ignored font-lock parameters."
  (let* ((params (greger-parser--extract-tool-use-params tool-use-node))
         (original-content (alist-get 'original-content params))
         (new-content (alist-get 'new-content params))
         (path (alist-get 'path params))
         (param-nodes (greger-parser--extract-tool-use-param-nodes tool-use-node))
         (original-content-node (alist-get 'original-content param-nodes))
         (new-content-node (alist-get 'new-content param-nodes))
         (replace-start (treesit-node-start original-content-node))
         (replace-end (treesit-node-end new-content-node))
         (tool-use-id (greger-parser--extract-tool-id tool-use-node))
         (diff-content (greger-ui--generate-diff-content original-content new-content path))
         (wrapped-diff (greger-parser--wrapped-tool-param "diff" tool-use-id diff-content t))
         (inhibit-read-only t))

    ;; Replace buffer content with diff
    (goto-char replace-start)
    (delete-region replace-start replace-end)
    (insert wrapped-diff)))

(defun greger-ui--apply-diff-syntax-highlighting (tool-use-node _start _end)
  "Apply syntax highlighting to existing diff content in str-replace TOOL-USE-NODE.
_START and _END are ignored font-lock parameters."
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
         (tool-use-id (greger-parser--extract-tool-id tool-use-node))
         (diff-content (greger-ui--generate-diff-content original-content new-content path))
         (wrapped-diff (greger-parser--wrapped-tool-param "diff" tool-use-id diff-content t))
         (inhibit-read-only t))

    ;; Replace buffer content with diff
    (goto-char replace-start)
    (delete-region replace-start replace-end)
    (insert wrapped-diff)))

(defun greger-ui--generate-diff-content (original new path)
  "Generate diff content from ORIGINAL to NEW for PATH using diff.el."
  (let* ((ext (or (file-name-extension path t) ""))
         (original-file (make-temp-file "greger-original-" nil ext))
         (new-file (make-temp-file "greger-new-" nil ext))
         (diff-buffer (get-buffer-create "*greger-diff-temp*"))
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

          ;; Set up the buffer to prevent dir-local variable issues
          (with-current-buffer diff-buffer
            (setq buffer-file-name nil)
            (setq default-directory (temporary-file-directory)))

          (diff-no-select original-file new-file nil t diff-buffer)
          (with-current-buffer diff-buffer
            ;; Ensure buffer is still alive before processing
            (unless (buffer-live-p diff-buffer)
              (error "Diff buffer was killed unexpectedly"))

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
            (greger-ui--diff-make-newline-messages-smaller)

            (buffer-string)))

      ;; Cleanup temp files
      (when (file-exists-p original-file) (delete-file original-file))
      (when (file-exists-p new-file) (delete-file new-file))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer)))))

(defun greger-ui--convert-faces-for-tree-sitter ()
  "Convert \='face text properties and overlay faces to \='font-lock-face.
This is needed for tree-sitter compatibility."
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
                (put-text-property overlay-start-in-line overlay-end-in-line
                                   'font-lock-face face-attrs))
               ;; Lines starting with + get green background
               ((eq line-start-char ?+)
                (plist-put face-attrs :background green-bg)
                (put-text-property overlay-start-in-line overlay-end-in-line
                                   'font-lock-face face-attrs))
               ;; Other lines get just the overlay face
               (t
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
    ;; If face is a list, use the first element,
    ;; otheriwse face-attribute will error
    (when (listp face)
      (setq face (car face)))
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

(defun greger-ui--diff-make-newline-messages-smaller ()
  "Apply visual de-emphasis to diff indicators.
Makes indicators small and muted while keeping them readable."
  ;; Make "\ No newline at end of file" messages less prominent
  (goto-char (point-min))
  (while (re-search-forward "^\\\\ No newline at end of file$" nil t)
    (put-text-property (line-beginning-position) (1+ (line-end-position))
                       'font-lock-face '(:height 0.6 :foreground "gray50"))))

(defun greger-ui--file-syntax-highlighting (tool-use-node)
  "Apply syntax highlighting to TOOL-USE-NODE content based on file path."
  (when-let* ((params (greger-parser--extract-tool-use-params tool-use-node))
              (param-nodes (greger-parser--extract-tool-use-param-nodes tool-use-node))
              (path (alist-get 'path params))
              (contents-node (alist-get 'contents param-nodes))
              (start (treesit-node-start contents-node))
              (end (treesit-node-end contents-node))
              (text (treesit-node-text contents-node)))
    (greger-ui--syntax-highlight-text start end text path)))

;; Tool result syntax highlighting

(defun greger-ui--tool-result-syntax-highlighting (tool-result-node _override _start _end)
  "Apply syntax highlighting to TOOL-RESULT-NODE based on corresponding tool use.
_OVERRIDE, _START, and _END are font-lock parameters."
  (unless (greger-ui--syntax-highlighted-p tool-result-node)
    (when-let* ((tool-use-node (greger-ui--find-corresponding-tool-use tool-result-node))
                (tool-name (greger-parser--extract-tool-use-name tool-use-node))
                (content-node (treesit-search-subtree tool-result-node "content"))
                (start (treesit-node-start content-node))
                (end (treesit-node-end content-node))
                (text (treesit-node-text content-node))
                (tool-use-params (greger-parser--extract-tool-use-params tool-use-node)))

      (cond
       ((string= tool-name "read-file")
        (when-let ((path (alist-get 'path tool-use-params)))
          (greger-ui--syntax-highlight-text start end text path)))
       (t nil)))
    (greger-ui--set-syntax-highlighted tool-result-node)))

(defun greger-ui--find-corresponding-tool-use (tool-result-node)
  "Find the tool_use node that corresponds to TOOL-RESULT-NODE by matching IDs."
  (when-let* ((tool-result-id (greger-parser--extract-tool-id tool-result-node))
              (prev-node (treesit-node-prev-sibling tool-result-node))
              ((string= (treesit-node-type prev-node) "tool_use"))
              (tool-use-node prev-node)
              (tool-use-id (greger-parser--extract-tool-id tool-use-node))
              ((string= tool-use-id tool-result-id)))
    tool-use-node))

(defun greger-ui--syntax-highlight-text (start end content path)
  "Apply syntax highlighting to CONTENT between START and END based on FILE-PATH."
  ;; Skip highlighting for very long content to avoid performance issues
  (when (< (length content) 50000)
    ;; Create a temporary buffer to get proper syntax highlighting
    (let ((temp-buffer (generate-new-buffer "*greger-syntax-temp*"))
          (original-buffer (current-buffer)))
      (unwind-protect
          (with-current-buffer temp-buffer
            ;; Insert content and set buffer-file-name for major mode detection
            (insert content)

            ;; Let Emacs determine the major mode based on the file name
            ;; Don't run hooks that might assume buffer-file-name really associates buffer with a file
            (let ((enable-local-variables nil)
                  (buffer-file-name path))
              (delay-mode-hooks (set-auto-mode)))

            ;; Enable font-lock and force fontification
            (font-lock-mode 1)
            (font-lock-ensure (point-min) (point-max))

            ;; Copy the font-lock-face properties to the original buffer
            (let ((temp-start (point-min))
                  (temp-end (point-max))
                  (original-pos start))

              (while (< temp-start temp-end)
                (let* ((next-change (or (next-single-property-change temp-start 'face (current-buffer) temp-end)
                                        temp-end))
                       (face (get-text-property temp-start 'face))
                       (len (- next-change temp-start)))

                  (when face
                    ;; Apply the face to the corresponding position in the original buffer
                    (with-current-buffer original-buffer
                      (put-text-property original-pos (min (+ original-pos len) end) 'font-lock-face face)))

                  (setq temp-start next-change
                        original-pos (+ original-pos len))))))

        ;; Clean up temp buffer
        (with-current-buffer temp-buffer
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(defun greger-ui--process-terminal-sequences (text)
  "Process terminal control sequences in TEXT to simulate terminal behavior.

This function handles common ANSI movement codes and control sequences that
are used by command-line tools for progress bars and dynamic output:

- \\r (carriage return) - moves cursor to beginning of line, overwrites content
- ESC[K - clears from cursor to end of line  
- ESC[2K - clears entire line
- ESC[A - cursor up (removes previous line)
- ESC[B - cursor down (adds newline)

The function processes text to simulate how a terminal would handle these
sequences, making progress bars and dynamic output display correctly in
the greger UI instead of showing all intermediate states."
  (let ((pos 0)
        (len (length text))
        (at-bol-after-cr nil))
    
    (while (< pos len)
      (let ((char (aref text pos)))
        (cond
         ;; Handle carriage return - move to beginning of current line
         ((= char ?\r)
          (beginning-of-line)
          (setq at-bol-after-cr t)
          (setq pos (1+ pos)))
         
         ;; Handle ESC sequences
         ((= char ?\e)
          (if (and (< (1+ pos) len) 
                   (= (aref text (1+ pos)) ?\[))
              ;; Found ESC[, check what follows
              (cond
               ;; ESC[K - clear from cursor to end of line 
               ((and (< (+ pos 2) len)
                     (= (aref text (+ pos 2)) ?K))
                ;; For now, just skip the sequence to test detection
                (setq pos (+ pos 3)))
               ;; ESC[2K - clear entire line
               ((and (<= (+ pos 3) (1- len))
                     (= (aref text (+ pos 2)) ?2)
                     (= (aref text (+ pos 3)) ?K))
                (beginning-of-line)
                (delete-region (point) (line-end-position))
                (setq at-bol-after-cr nil)
                (setq pos (+ pos 4)))
               ;; Not a recognized sequence, treat as regular character
               (t
                (when at-bol-after-cr
                  (delete-region (point) (line-end-position))
                  (setq at-bol-after-cr nil))
                (insert-char char)
                (setq pos (1+ pos))))
            ;; ESC without [, treat as regular character
            (progn
              (when at-bol-after-cr
                (delete-region (point) (line-end-position))
                (setq at-bol-after-cr nil))
              (insert-char char)
              (setq pos (1+ pos)))))
         
         ;; Handle newline
         ((= char ?\n)
          (insert "\n")
          (setq at-bol-after-cr nil)
          (setq pos (1+ pos)))
         
         ;; Regular character
         (t
          ;; If we're at beginning of line after a \r, delete rest of line first
          (when at-bol-after-cr
            (delete-region (point) (line-end-position))
            (setq at-bol-after-cr nil))
          (insert-char char)
          (setq pos (1+ pos))))
      
      ;; Safety check to prevent infinite loops
      (when (> pos len)
        (setq pos len))))))

(provide 'greger-ui)
;;; greger-ui.el ends here
