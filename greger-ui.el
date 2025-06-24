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
            (put-text-property (- uncle-last-child-end 2) text-start 'invisible t)))))

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
NODE is the matched tree-sitter node"
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (is-visible (get-text-property node-start 'greger-ui-tool-content-expanded)))

    ;; Apply invisibility (default is invisible unless expanded)
    (put-text-property node-start node-end 'invisible (not is-visible))
    (put-text-property node-start node-end 'keymap greger-ui-tool-content-tail-keymap)))

(defun greger-ui--thinking-signature-hiding-fn (node _override _start _end)
  "Hide thinking signature.  NODE is the matched tree-sitter node."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (invisible-end (+ node-end 2)))
    (put-text-property node-start invisible-end 'invisible t)))

(defun greger-ui--make-tool-tag-invisible (node _override _start _end)
  "Make tool tag NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (1+ (treesit-node-end node))))
    (put-text-property node-start node-end 'invisible t)))

(defun greger-ui--make-tool-result-id-invisible (node _override _start _end)
  "Make id NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (put-text-property node-start node-end 'invisible t)))

(defun greger-ui--make-tool-use-id-invisible (node _override _start _end)
  "Make id NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (1- (treesit-node-end node))))
    (put-text-property node-start node-end 'invisible t)))

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

(defun greger-ui--get-str-replace-cached-overlay (start end)
  "Get cached overlay for str-replace transformation between START and END."
  (let ((overlays (overlays-in start end)))
    (seq-find (lambda (ov) (overlay-get ov 'greger-ui-str-replace-cached-key)) overlays)))

(defun greger-ui--create-str-replace-cached-overlay (start end cache-key)
  "Create a cached overlay for str-replace transformation between START and END with CACHE-KEY."
  ;; Clean up any old overlay first
  (when-let ((old-overlay (greger-ui--get-str-replace-cached-overlay start end)))
    (delete-overlay old-overlay))
  
  ;; Create new overlay
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'greger-ui-str-replace-cached-key cache-key)
    (overlay-put overlay 'evaporate t)
    overlay))

;; Variable to track pending idle operations
(defvar greger-ui--pending-diff-operations nil
  "List of pending diff operations to be processed during idle time.")

;; Timer for processing pending operations
(defvar greger-ui--idle-timer nil
  "Timer for processing diff operations during idle time.")

;; Customizable delay for idle processing
(defcustom greger-ui-idle-delay 0.5
  "Delay in seconds before processing diff operations during idle time.
Lower values make diff processing more responsive but may impact performance.
Higher values reduce interruptions during active editing."
  :type 'number
  :group 'greger)

(defun greger-ui--schedule-diff-operation (operation-type node start end cache-key)
  "Schedule a diff operation to be processed during idle time.
OPERATION-TYPE is either 'transform or 'highlight.
NODE, START, END, and CACHE-KEY are the operation parameters."
  (let* ((buffer (current-buffer))
         (operation (list operation-type buffer node start end cache-key)))
    
    ;; Add to pending operations (avoid duplicates)
    (unless (member operation greger-ui--pending-diff-operations)
      (push operation greger-ui--pending-diff-operations))
    
    ;; Set up or restart the idle timer
    (when greger-ui--idle-timer
      (cancel-timer greger-ui--idle-timer))
    
    (setq greger-ui--idle-timer
          (run-with-idle-timer greger-ui-idle-delay nil
                               #'greger-ui--process-pending-operations))))

(defun greger-ui--process-pending-operations ()
  "Process all pending diff operations."
  (let ((operations greger-ui--pending-diff-operations))
    (setq greger-ui--pending-diff-operations nil
          greger-ui--idle-timer nil)
    
    ;; Process operations in LIFO order (most recent first)
    (dolist (operation operations)
      (let ((operation-type (nth 0 operation))
            (buffer (nth 1 operation))
            (node (nth 2 operation))
            (start (nth 3 operation))
            (end (nth 4 operation))
            (cache-key (nth 5 operation)))
        
        ;; Only process if buffer is still live and visible
        (when (and (buffer-live-p buffer)
                   (get-buffer-window buffer))
          (with-current-buffer buffer
            (cond
             ((eq operation-type 'transform)
              (greger-ui--apply-str-replace-diff-content node start end cache-key))
             ((eq operation-type 'highlight)
              (greger-ui--apply-diff-syntax-highlighting node start end cache-key)))))))))

(defun greger-ui--cleanup-idle-operations ()
  "Clean up idle operations when buffer is killed or mode is disabled."
  (when greger-ui--idle-timer
    (cancel-timer greger-ui--idle-timer)
    (setq greger-ui--idle-timer nil))
  ;; Remove operations for this buffer
  (setq greger-ui--pending-diff-operations
        (cl-remove-if (lambda (op) (eq (nth 1 op) (current-buffer)))
                      greger-ui--pending-diff-operations)))

(defun greger-ui--str-replace-diff-transform-fn (node _override start end)
  "Font-lock function to transform str-replace original/new content into diff content.
NODE is the matched tree-sitter node for tool_use block.
Expensive operations are deferred to idle time to avoid blocking scrolling."
  (when-let* ((tool-use-name (greger-parser--extract-tool-use-name node))
              ((string= tool-use-name "str-replace")))

    (unless (greger-ui--get-str-replace-cached-overlay start end)
      (when node
       (let* ((params (greger-parser--extract-tool-use-params node))
              (has-diff (alist-get 'diff params))
              (has-original-new (and (alist-get 'original-content params)
                                     (alist-get 'new-content params)))
              (cache-key (greger-ui--compute-str-replace-cache-key node start end)))
         (cond
          ;; Case 1: File already has diff parameter - defer syntax highlighting
          (has-diff
           (greger-ui--schedule-diff-operation 'highlight node start end cache-key))
          ;; Case 2: File has original-content/new-content - defer diff transformation
          (has-original-new
           (greger-ui--schedule-diff-operation 'transform node start end cache-key))
          ;; Case 3: Neither - do nothing
          (t nil)))))))

(defun greger-ui--compute-str-replace-cache-key (_node start end)
  "Compute cache key for str-replace NODE content between START and END."
  (let ((content (buffer-substring-no-properties start end)))
    (md5 content)))

(defun greger-ui--apply-str-replace-diff-content (node start end cache-key)
  "Apply diff overlay to str-replace NODE content between START and END."
  (greger-ui--create-str-replace-cached-overlay start end cache-key)

  (let* ((params (greger-parser--extract-tool-use-params node))
         (original-content (alist-get 'original-content params))
         (new-content (alist-get 'new-content params))
         (path (alist-get 'path params))
         (param-nodes (greger-parser--extract-tool-use-param-nodes node))
         (original-content-node (alist-get 'original-content param-nodes))
         (new-content-node (alist-get 'new-content param-nodes))
         (replace-start (treesit-node-start original-content-node))
         (replace-end (treesit-node-end new-content-node))
         (tool-use-id (greger-parser--extract-tool-use-id node))
         (diff-content (greger-ui--generate-diff-content original-content new-content path))
         (wrapped-diff (greger-parser--wrapped-tool-param "diff" tool-use-id diff-content t))
         (inhibit-read-only t))

    ;; Replace buffer content with diff
    (goto-char replace-start)
    (delete-region replace-start replace-end)
    (insert wrapped-diff)))

(defun greger-ui--apply-diff-syntax-highlighting (node start end cache-key)
  "Apply syntax highlighting to existing diff content in str-replace tool_use."
  (greger-ui--create-str-replace-cached-overlay start end cache-key)

  (let* ((params (greger-parser--extract-tool-use-params node))
         (raw-diff-content (alist-get 'diff params))
         (path (alist-get 'path params))
         (undiff-result (greger-parser-undiff-strings raw-diff-content))
         (original-content (car undiff-result))
         (new-content (cdr undiff-result))
         (param-nodes (greger-parser--extract-tool-use-param-nodes node))
         (diff-node (alist-get 'diff param-nodes))
         (replace-start (treesit-node-start diff-node))
         (replace-end (treesit-node-end diff-node))
         (tool-use-id (greger-parser--extract-tool-use-id node))
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
         (diff-buffer (get-buffer-create "*Greger diff*"))
         (diff-refine nil)
         (diff-switches '("-u" "-U" "100000")))
    (message (format "diff-buffer: %s" diff-buffer))

    (unwind-protect
        (progn
          ;; Write content to temp files
          (with-temp-file original-file
            (insert original))
          (with-temp-file new-file
            (insert new))

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
         (red-bg (if is-dark-theme "#2d1b1b" "#ffe6e6")) ; Dark red vs light red
         (green-bg (if is-dark-theme "#1b2d1b" "#e6ffe6"))) ; Dark green vs light green
    
    ;; Convert overlays with 'face property (syntax highlighting overlays)
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

              ;; TODO: remove debug
              (message (format "line-start-char: %s" line-start-char))
              
              (cond
               ;; Lines starting with - get red background
               ((eq line-start-char ?-)
                (plist-put face-attrs :background red-bg)
                ;; TODO: remove debug
                (message (format "face-attrs: %s" face-attrs))
                (put-text-property overlay-start-in-line overlay-end-in-line
                                   'font-lock-face face-attrs)
                ;; Set background on all characters in the line, including those without overlay attributes
                (put-text-property line-start (1+ line-end) 'font-lock-face 
                                   (list :background red-bg)))
               ;; Lines starting with + get green background
               ((eq line-start-char ?+)
                (plist-put face-attrs :background green-bg)
                (put-text-property overlay-start-in-line overlay-end-in-line
                                   'font-lock-face face-attrs)
                ;; Set background on all characters in the line, including those without overlay attributes
                (put-text-property line-start (1+ line-end) 'font-lock-face 
                                   (list :background green-bg)))
               ;; Other lines get just the overlay face
               (t
                ;; TODO: remove debug
                (message (format "face-attrs: %s" face-attrs))
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

(provide 'greger-ui)
;;; greger-ui.el ends here
