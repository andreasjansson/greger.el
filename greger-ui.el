;;; greger-ui.el --- UI utilities for greger -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This file is part of greger.

;; greger is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; greger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with greger.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides UI utilities for greger, including citation folding
;; functionality that makes assistant responses with citations more readable.

;;; Code:

(require 'cl-lib)
(require 'greger-parser)

(defgroup greger-ui nil
  "UI customization for greger."
  :group 'greger)

(defface greger-cite-tag-face
  '((t (:foreground "#00FFFF" :weight bold)))
  "Face for <cite> tags."
  :group 'greger-ui)

(defface greger-folded-citations-face
  '((t (:foreground "#888888" :slant italic :height 0.8)))
  "Face for folded citation summary text."
  :group 'greger-ui)

(defface greger-tool-tag-face
  '((t (:foreground "#666666" :height 0.8)))
  "Face for tool tags."
  :group 'greger-ui)

;; Constants
(defconst greger-ui-cite-tag-regex "<cite>\\([^<]*\\)</cite>"
  "Regular expression to match cite tags.")

(defconst greger-ui-assistant-header-regex "^## ASSISTANT:$"
  "Regular expression to match assistant headers.")

;; Variables
(defvar-local greger-ui-cite-citation-overlays nil
  "List of overlays used to hide citation blocks.")

(defvar-local greger-ui-bibliography-overlays nil
  "List of overlays used to hide/show the final bibliography section.")

(defvar-local greger-ui-unfolded-cite-positions nil
  "List of cite tag positions that have been manually unfolded.
Each element is (CITE-START . CITE-END) for cite tags that should remain expanded.")

(defvar-local greger-ui-bibliography-manually-shown nil
  "Whether the final bibliography section has been manually expanded.")

;; Tool section variables
(defvar greger-ui-tool-section-max-lines 4
  "Maximum number of lines to show in collapsed tool sections.")

(defvar-local greger-ui-tool-overlays nil
  "List of overlays used for collapsible tool sections.")

(defvar-local greger-ui-unfolded-tool-ids nil
  "List of tool IDs that have been manually unfolded.
These tool IDs should not be auto-folded again.")

;;; Core Functions

(defun greger-ui-point-in-cite-tag-p ()
  "Return t if point is inside a <cite> tag."
  (save-excursion
    (let ((orig-point (point)))
      ;; Look for cite tags on the current line
      (beginning-of-line)
      (catch 'found
        (while (re-search-forward greger-ui-cite-tag-regex (line-end-position) t)
          (when (and (>= orig-point (match-beginning 0))
                     (<= orig-point (match-end 0)))
            (throw 'found t)))
        nil))))

(defun greger-ui-get-cite-tag-at-point ()
  "Get the cite tag at point. Returns (START . END) or nil."
  (save-excursion
    (let ((orig-point (point)))
      (beginning-of-line)
      (catch 'found
        (while (re-search-forward greger-ui-cite-tag-regex (line-end-position) t)
          (when (and (>= orig-point (match-beginning 0))
                     (<= orig-point (match-end 0)))
            (throw 'found (cons (match-beginning 0) (match-end 0)))))
        nil))))

(defun greger-ui-find-all-cite-citation-pairs ()
  "Find all cite tag and citation block pairs in the buffer.
Returns a list of ((CITE-START . CITE-END) . (CITATION-START . CITATION-END))."
  (save-excursion
    (goto-char (point-min))
    (let (pairs)

      ;; Find cite tags and their associated citation blocks
      (while (re-search-forward greger-ui-cite-tag-regex nil t)
        (let ((cite-start (match-beginning 0))
              (cite-end (match-end 0))
              citation-start citation-end)

          ;; Citation block starts right after the cite tag (including newlines)
          (setq citation-start cite-end)

          ;; Look for the next ## CITATIONS: section after this cite tag
          (when (re-search-forward (concat "^" (regexp-quote greger-parser-citations-tag) "$") nil t)

            ;; Find the end of this citation section
            (forward-line 1)
            (if (re-search-forward "^## " nil t)
                (progn
                  ;; Include the next section header and empty lines in the fold
                  (beginning-of-line)
                  (if (looking-at greger-ui-assistant-header-regex)
                      (progn
                        ;; Skip the ASSISTANT header
                        (forward-line 1)
                        ;; Skip empty lines after ASSISTANT header
                        (while (and (not (eobp)) (looking-at "^\\s-*$"))
                          (forward-line 1))
                        (setq citation-end (point)))
                    (setq citation-end (line-beginning-position))))
              (setq citation-end (point-max)))

            ;; Add this pair (include all cite-citation pairs, even if followed by final bibliography)
            (push (cons (cons cite-start cite-end)
                        (cons citation-start citation-end))
                  pairs)

            ;; Go back to continue searching from after the cite tag
            (goto-char cite-end))))

      (nreverse pairs))))

(defun greger-ui-find-citation-for-cite-tag ()
  "Find the citation block associated with the cite tag at point.
Returns (START . END) or nil."
  (let ((cite-tag (greger-ui-get-cite-tag-at-point)))
    (when cite-tag
      (let ((cite-start (car cite-tag))
            (cite-end (cdr cite-tag))
            (pairs (greger-ui-find-all-cite-citation-pairs)))
        ;; Find the pair that matches our cite tag
        (catch 'found
          (dolist (pair pairs)
            (let ((pair-cite-start (car (car pair)))
                  (pair-cite-end (cdr (car pair)))
                  (citation-bounds (cdr pair)))
              (when (and (= cite-start pair-cite-start)
                         (= cite-end pair-cite-end))
                (throw 'found citation-bounds))))
          nil)))))

(defun greger-ui-hide-citation-block (start end)
  "Hide citation block from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'greger-citation t)
    (overlay-put overlay 'evaporate t)
    (push overlay greger-ui-cite-citation-overlays)))

(defun greger-ui-show-citation-block (start end)
  "Show citation block from START to END."
  (remove-overlays start end 'greger-citation t)
  (setq greger-ui-cite-citation-overlays
        (cl-remove-if (lambda (ov)
                        (or (not (overlay-buffer ov))  ; Remove dead overlays
                            (and (overlay-start ov)     ; Check overlay is valid
                                 (overlay-end ov)
                                 (>= (overlay-start ov) start)
                                 (<= (overlay-end ov) end))))
                      greger-ui-cite-citation-overlays)))

(defun greger-ui-citation-block-hidden-p (start end)
  "Return t if citation block from START to END is hidden."
  ;; First clean up dead overlays
  (setq greger-ui-cite-citation-overlays
        (cl-remove-if-not (lambda (ov)
                            (and (overlay-buffer ov)
                                 (overlay-start ov)
                                 (overlay-end ov)))
                          greger-ui-cite-citation-overlays))
  ;; Then check if any overlay covers this region
  (cl-some (lambda (ov)
             (and (overlay-get ov 'greger-citation)
                  (overlay-start ov)  ; Double-check overlay is valid
                  (overlay-end ov)
                  (>= (overlay-start ov) start)
                  (<= (overlay-end ov) end)))
           greger-ui-cite-citation-overlays))

;;; Final Bibliography Functions

(defun greger-ui-find-final-bibliography ()
  "Find the final CITATIONS section that acts as a bibliography.
Returns (START . END) or nil if not found."
  (save-excursion
    (goto-char (point-max))
    ;; Look backwards for the last CITATIONS section
    (when (re-search-backward (concat "^" (regexp-quote greger-parser-citations-tag) "$") nil t)
      (let ((start (line-beginning-position))
            end)
        ;; Check if this is truly the final section (no more cite tags after it)
        (save-excursion
          (goto-char start)
          (unless (re-search-forward greger-ui-cite-tag-regex nil t)
            ;; This is the final bibliography - find where it ends
            (goto-char start)
            (forward-line 1)  ; Skip the header
            ;; Look for the next section header (## ) or end of buffer
            (if (re-search-forward "^## " nil t)
                (progn
                  ;; Go back to preserve one newline before the next section
                  (beginning-of-line)
                  (skip-chars-backward " \t\n")
                  (forward-line 1)
                  (setq end (point)))
              (setq end (point-max)))
            (cons start end)))))))

(defun greger-ui-count-citations-in-section (start end)
  "Count the number of citation entries in a CITATIONS section from START to END."
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (re-search-forward "^### " end t)
        (setq count (1+ count)))
      count)))

(defun greger-ui-hide-final-bibliography (start end)
  "Hide the final bibliography section with a summary."
  (let* ((count (greger-ui-count-citations-in-section start end))
         (header-end (save-excursion (goto-char start) (line-end-position)))
         (content-start (save-excursion (goto-char start) (forward-line 1) (point)))
         (overlay (make-overlay content-start end))
         (summary-overlay (make-overlay header-end content-start)))

    ;; Hide the bibliography content (but not the header)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'greger-bibliography t)
    (overlay-put overlay 'evaporate t)
    (push overlay greger-ui-bibliography-overlays)

    ;; Replace the newline after the header with our summary
    (overlay-put summary-overlay 'display
                 (propertize (format "\n[+%d citations, TAB to expand]\n"
                                   count)
                           'face 'greger-folded-citations-face))
    (overlay-put summary-overlay 'greger-bibliography-summary t)
    (overlay-put summary-overlay 'evaporate t)
    (push summary-overlay greger-ui-bibliography-overlays)))

(defun greger-ui-show-final-bibliography (start end)
  "Show the final bibliography section."
  (let ((header-end (save-excursion (goto-char start) (line-end-position)))
        (content-start (save-excursion (goto-char start) (forward-line 1) (point))))
    ;; Remove both content and summary overlays
    (remove-overlays content-start end 'greger-bibliography t)
    (remove-overlays header-end content-start 'greger-bibliography-summary t)
    (setq greger-ui-bibliography-overlays
          (cl-remove-if (lambda (ov)
                          (or (not (overlay-buffer ov))
                              (and (overlay-start ov)
                                   (overlay-end ov)
                                   (>= (overlay-start ov) header-end)
                                   (<= (overlay-end ov) end))))
                        greger-ui-bibliography-overlays))))

(defun greger-ui-bibliography-hidden-p (start end)
  "Return t if the bibliography section is hidden."
  (let ((content-start (save-excursion (goto-char start) (forward-line 1) (point))))
    (cl-some (lambda (ov)
               (and (overlay-get ov 'greger-bibliography)
                    (overlay-start ov)
                    (overlay-end ov)
                    (>= (overlay-start ov) content-start)
                    (<= (overlay-end ov) end)))
             greger-ui-bibliography-overlays)))

(defun greger-ui-point-in-bibliography-p ()
  "Return t if point is in the final bibliography section."
  (let ((bib-bounds (greger-ui-find-final-bibliography)))
    (when bib-bounds
      (and (>= (point) (car bib-bounds))
           (<= (point) (cdr bib-bounds))))))

;;; Interactive Functions

(defun greger-ui-toggle-citation-fold ()
  "Toggle folding of citation blocks or bibliography.
Works when point is in a <cite> tag or in the final bibliography section."
  (interactive)
  (cond
   ;; Point is in a cite tag
   ((greger-ui-point-in-cite-tag-p)
    (let ((cite-tag (greger-ui-get-cite-tag-at-point))
          (citation-bounds (greger-ui-find-citation-for-cite-tag)))
      (if citation-bounds
          (let ((start (car citation-bounds))
                (end (cdr citation-bounds)))
            (if (greger-ui-citation-block-hidden-p start end)
                (progn
                  (greger-ui-show-citation-block start end)
                  ;; Mark this cite tag as manually unfolded
                  (greger-ui-mark-cite-unfolded cite-tag))
              (progn
                (greger-ui-hide-citation-block start end)
                ;; Remove from manually unfolded list
                (greger-ui-unmark-cite-unfolded cite-tag))))
        (message "No citation block found for this cite tag"))))

   ;; Point is in the final bibliography section
   ((greger-ui-point-in-bibliography-p)
    (let ((bib-bounds (greger-ui-find-final-bibliography)))
      (when bib-bounds
        (let ((start (car bib-bounds))
              (end (cdr bib-bounds)))
          (if (greger-ui-bibliography-hidden-p start end)
              (progn
                (greger-ui-show-final-bibliography start end)
                ;; Mark bibliography as manually shown
                (setq greger-ui-bibliography-manually-shown t))
            (progn
              (greger-ui-hide-final-bibliography start end)
              ;; Mark bibliography as manually hidden
              (setq greger-ui-bibliography-manually-shown nil)))))))

   ;; Not in a cite tag or bibliography
   (t
    (message "TAB only works inside <cite> tags or in the bibliography section"))))

(defun greger-ui-mark-cite-unfolded (cite-tag)
  "Mark a cite tag as manually unfolded.
CITE-TAG is a cons (START . END) for the cite tag position."
  (when cite-tag
    (let ((cite-pos cite-tag))
      (unless (member cite-pos greger-ui-unfolded-cite-positions)
        (push cite-pos greger-ui-unfolded-cite-positions)))))

(defun greger-ui-unmark-cite-unfolded (cite-tag)
  "Remove a cite tag from the manually unfolded list.
CITE-TAG is a cons (START . END) for the cite tag position."
  (when cite-tag
    (setq greger-ui-unfolded-cite-positions
          (cl-remove cite-tag greger-ui-unfolded-cite-positions :test #'equal))))

(defun greger-ui-cite-manually-unfolded-p (cite-tag)
  "Return t if the cite tag has been manually unfolded.
CITE-TAG is a cons (START . END) for the cite tag position."
  (when cite-tag
    (cl-member cite-tag greger-ui-unfolded-cite-positions :test #'equal)))

(defun greger-ui-hide-all-citations ()
  "Hide all citation blocks that should be folded."
  (interactive)
  (let ((pairs (greger-ui-find-all-cite-citation-pairs)))
    (dolist (pair pairs)
      (let ((cite-bounds (car pair))
            (citation-bounds (cdr pair)))
        ;; Only hide if this cite tag hasn't been manually unfolded
        (unless (greger-ui-cite-manually-unfolded-p cite-bounds)
          (greger-ui-hide-citation-block (car citation-bounds) (cdr citation-bounds))))))

  ;; Also hide the final bibliography section unless manually shown
  (let ((bib-bounds (greger-ui-find-final-bibliography)))
    (when (and bib-bounds (not greger-ui-bibliography-manually-shown))
      (greger-ui-hide-final-bibliography (car bib-bounds) (cdr bib-bounds)))))

(defun greger-ui-show-all-citations ()
  "Show all citation blocks."
  (interactive)
  (remove-overlays (point-min) (point-max) 'greger-citation t)
  (remove-overlays (point-min) (point-max) 'greger-bibliography t)
  (remove-overlays (point-min) (point-max) 'greger-bibliography-summary t)
  (setq greger-ui-cite-citation-overlays nil)
  (setq greger-ui-bibliography-overlays nil)
  ;; Clear manual folding state
  (setq greger-ui-unfolded-cite-positions nil)
  (setq greger-ui-bibliography-manually-shown nil))

(defun greger-ui-cleanup-dead-overlays ()
  "Remove dead overlays from citation and tool overlays lists."
  (setq greger-ui-cite-citation-overlays
        (cl-remove-if-not (lambda (ov)
                            (and (overlay-buffer ov)
                                 (overlay-start ov)
                                 (overlay-end ov)))
                          greger-ui-cite-citation-overlays))
  (setq greger-ui-bibliography-overlays
        (cl-remove-if-not (lambda (ov)
                            (and (overlay-buffer ov)
                                 (overlay-start ov)
                                 (overlay-end ov)))
                          greger-ui-bibliography-overlays))
  (setq greger-ui-tool-overlays
        (cl-remove-if-not (lambda (ov)
                            (and (overlay-buffer ov)
                                 (overlay-start ov)
                                 (overlay-end ov)))
                          greger-ui-tool-overlays)))

(defun greger-ui-cleanup-stale-cite-positions ()
  "Remove cite positions that no longer exist in the buffer."
  (setq greger-ui-unfolded-cite-positions
        (cl-remove-if (lambda (cite-pos)
                        (let ((start (car cite-pos))
                              (end (cdr cite-pos)))
                          ;; Check if position is still valid and contains a cite tag
                          (or (< start (point-min))
                              (> end (point-max))
                              (not (save-excursion
                                     (goto-char start)
                                     (looking-at greger-ui-cite-tag-regex))))))
                      greger-ui-unfolded-cite-positions)))

;;; Font Lock Support

(defun greger-ui-setup-cite-fontification ()
  "Set up font-lock for cite tags."
  (font-lock-add-keywords
   nil
   `((,greger-ui-cite-tag-regex
      (0 'greger-cite-tag-face)))
   'append))

;;; Tool Section Functions

(defun greger-ui-setup-tool-sections ()
  "Set up tool section highlighting and collapsing in the current buffer."
  (greger-ui-clear-tool-overlays)
  (greger-ui-find-and-setup-tool-sections))

(defun greger-ui-clear-tool-overlays ()
  "Clear all tool section overlays in the current buffer."
  (cl-loop for overlay in greger-ui-tool-overlays
           do (delete-overlay overlay))
  (setq greger-ui-tool-overlays nil))

(defun greger-ui-find-and-setup-tool-sections ()
  "Find all tool sections and set them up with appropriate faces and collapsing."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "<tool\\.[^>]+>" nil t)
             do (greger-ui-setup-single-tool-section))))

(defun greger-ui-setup-single-tool-section ()
  "Set up a single tool section starting from the current match."
  (let ((start-tag-start (match-beginning 0))
        (start-tag-end (match-end 0))
        (tool-id (greger-ui-extract-tool-id (match-string 0))))
    (when tool-id
      (let ((end-tag-pattern (concat "</tool\\." (regexp-quote tool-id) ">"))
            (content-start start-tag-end)
            end-tag-start end-tag-end content-end)

        ;; Find the closing tag
        (when (re-search-forward end-tag-pattern nil t)
          (setq end-tag-start (match-beginning 0)
                end-tag-end (match-end 0)
                content-end end-tag-start)

          ;; Create overlays for styling
          (greger-ui-create-tag-overlay start-tag-start start-tag-end)
          (greger-ui-create-tag-overlay end-tag-start end-tag-end)

          ;; Set up collapsible content
          (greger-ui-setup-collapsible-content content-start content-end tool-id))))))

(defun greger-ui-extract-tool-id (tag-string)
  "Extract tool ID from a tool tag string like '<tool.abc123>'.
TAG-STRING is the tag string to extract from."
  (when (string-match "<tool\\.\\([^>]+\\)>" tag-string)
    (match-string 1 tag-string)))

(defun greger-ui-create-tag-overlay (start end)
  "Create an overlay for a tool tag to make it small and less visible.
START and END define the overlay bounds."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'greger-tool-tag-face)
    (overlay-put overlay 'greger-tool-tag t)
    (push overlay greger-ui-tool-overlays)
    overlay))

(defun greger-ui-setup-collapsible-content (content-start content-end tool-id)
  "Set up collapsible content between CONTENT-START and CONTENT-END for TOOL-ID.
CONTENT-START is the start position of the content.
CONTENT-END is the end position of the content.
TOOL-ID identifies the tool."
  (let* ((content (buffer-substring-no-properties content-start content-end))
         (lines (split-string content "\n"))
         (line-count (length lines)))

    (when (and (> line-count greger-ui-tool-section-max-lines)
               ;; Only create collapsed overlay if not manually unfolded
               (not (member tool-id greger-ui-unfolded-tool-ids)))
      ;; Create the collapsible overlay
      (greger-ui-create-collapsible-overlay content-start content-end tool-id lines))))

(defun greger-ui-create-collapsible-overlay (content-start content-end tool-id lines)
  "Create a collapsible overlay for tool content.
CONTENT-START and CONTENT-END define the overlay bounds.
TOOL-ID identifies the tool, and LINES contain the content."
  (let* ((visible-lines (cl-subseq lines 0 greger-ui-tool-section-max-lines))
         (hidden-lines (cl-subseq lines greger-ui-tool-section-max-lines))
         (hidden-line-count (length hidden-lines))
         (visible-text (mapconcat #'identity visible-lines "\n"))

         ;; Calculate positions for visible and hidden parts
         (visible-end (+ content-start (length visible-text)))
         (hidden-start (+ visible-end 1)) ; +1 for the newline

         ;; Create overlay for the hidden part
         (hidden-overlay (make-overlay hidden-start content-end)))

    (overlay-put hidden-overlay 'invisible 'greger-tool-section)
    (overlay-put hidden-overlay 'greger-tool-section t)
    (overlay-put hidden-overlay 'greger-tool-id tool-id)
    (overlay-put hidden-overlay 'greger-collapsed t)

    ;; Add expansion indicator with line count
    (let ((indicator-overlay (make-overlay visible-end visible-end)))
      (overlay-put indicator-overlay 'after-string
                   (propertize (format "... [+%d lines, TAB to expand]" hidden-line-count)
                              'face 'greger-tool-tag-face))
      (overlay-put indicator-overlay 'greger-tool-indicator t)
      (overlay-put indicator-overlay 'greger-tool-id tool-id)
      (push indicator-overlay greger-ui-tool-overlays))

    (push hidden-overlay greger-ui-tool-overlays)))

(defun greger-ui-get-tool-id-at-point ()
  "Get the tool ID for the tool section at point, if any."
  ;; First check overlays at point
  (or (cl-loop for overlay in (overlays-at (point))
               for tool-id = (overlay-get overlay 'greger-tool-id)
               when tool-id return tool-id)
      ;; If not found, search backwards and forwards for tool tags
      (greger-ui-find-tool-id-near-point)))

(defun greger-ui-find-tool-id-near-point ()
  "Find tool ID near point by searching for tool tags."
  (save-excursion
    (let ((start-pos (point))
          tool-id)
      ;; Search backwards for opening tag
      (when (re-search-backward "<tool\\.[^>]+>" nil t)
        (let ((open-tag-start (match-beginning 0))
              (tag-tool-id (greger-ui-extract-tool-id (match-string 0))))
          ;; Check if we're within this tool section
          (when tag-tool-id
            (let ((close-pattern (concat "</tool\\." (regexp-quote tag-tool-id) ">")))
              (when (re-search-forward close-pattern nil t)
                (let ((close-tag-end (match-end 0)))
                  ;; If original point is between open and close tags
                  (when (and (>= start-pos open-tag-start)
                             (<= start-pos close-tag-end))
                    (setq tool-id tag-tool-id))))))))
      tool-id)))

(defun greger-ui-toggle-tool-section-by-id (tool-id)
  "Toggle the tool section with the given TOOL-ID.
TOOL-ID is the identifier of the tool section to toggle."
  (cl-loop for overlay in greger-ui-tool-overlays
           when (and (overlay-get overlay 'greger-tool-section)
                     (string= (overlay-get overlay 'greger-tool-id) tool-id))
           do (greger-ui-toggle-overlay-visibility overlay tool-id)))

(defun greger-ui-toggle-overlay-visibility (overlay tool-id)
  "Toggle the visibility of OVERLAY for TOOL-ID."
  (let ((is-collapsed (overlay-get overlay 'greger-collapsed)))
    (if is-collapsed
        (greger-ui-expand-tool-section overlay tool-id)
      (greger-ui-collapse-tool-section overlay tool-id))))

(defun greger-ui-expand-tool-section (overlay tool-id)
  "Expand the tool section by making OVERLAY visible.
OVERLAY is the overlay to expand, TOOL-ID identifies the tool."
  (overlay-put overlay 'invisible nil)
  (overlay-put overlay 'greger-collapsed nil)

  ;; Mark this tool as manually unfolded so it doesn't get auto-folded again
  (unless (member tool-id greger-ui-unfolded-tool-ids)
    (push tool-id greger-ui-unfolded-tool-ids))

  ;; Remove the expansion indicator
  (cl-loop for indicator-overlay in greger-ui-tool-overlays
           when (and (overlay-get indicator-overlay 'greger-tool-indicator)
                     (string= (overlay-get indicator-overlay 'greger-tool-id) tool-id))
           do (progn
                (delete-overlay indicator-overlay)
                (setq greger-ui-tool-overlays
                      (remove indicator-overlay greger-ui-tool-overlays)))))

(defun greger-ui-collapse-tool-section (overlay tool-id)
  "Collapse the tool section by making OVERLAY invisible.
OVERLAY is the overlay to hide.
TOOL-ID is the tool identifier."
  (overlay-put overlay 'invisible 'greger-tool-section)
  (overlay-put overlay 'greger-collapsed t)

  ;; Remove from unfolded list since it's now manually collapsed
  (setq greger-ui-unfolded-tool-ids
        (delete tool-id greger-ui-unfolded-tool-ids))

  ;; Calculate the number of hidden lines for the indicator
  (let* ((content (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))
         (lines (split-string content "\n"))
         (hidden-line-count (length lines))
         (overlay-start (overlay-start overlay))
         (indicator-pos (max (point-min) (1- overlay-start)))
         (indicator-overlay (make-overlay indicator-pos indicator-pos)))
    (overlay-put indicator-overlay 'after-string
                 (propertize (format "... [+%d lines, TAB to expand]" hidden-line-count)
                            'face 'greger-tool-tag-face))
    (overlay-put indicator-overlay 'greger-tool-indicator t)
    (overlay-put indicator-overlay 'greger-tool-id tool-id)
    (push indicator-overlay greger-ui-tool-overlays)))

;;; Integration Functions

(defun greger-ui-toggle-section ()
  "Toggle the tool section or citation at point between collapsed and expanded state.
If not inside a tool section or citation, fall back to `markdown-cycle'."
  (interactive)
  (let ((tool-id (greger-ui-get-tool-id-at-point)))
    (cond
     ;; First try tool sections
     (tool-id
      (greger-ui-toggle-tool-section-by-id tool-id))
     ;; Then try citation folding
     ((or (greger-ui-point-in-cite-tag-p) (greger-ui-point-in-bibliography-p))
      (greger-ui-toggle-citation-fold))
     ;; Fall back to markdown-cycle if available
     ((fboundp 'markdown-cycle)
      (call-interactively #'markdown-cycle))
     ;; Final fallback
     (t
      (message "Not inside a tool section or citation area")))))

(defun greger-ui-setup-folding ()
  "Set up both citation and tool folding for the current buffer."
  ;; Set up invisible text for tool sections
  (add-to-invisibility-spec 'greger-tool-section)

  ;; Clean up any existing overlays first
  (greger-ui-cleanup-dead-overlays)

  ;; Set up fontification
  (greger-ui-setup-cite-fontification)

  ;; Set up tool sections
  (greger-ui-setup-tool-sections)

  ;; Initially hide appropriate citation blocks and tool sections
  (greger-ui-hide-all-citations)

  ;; Add hook to clean up overlays when buffer is killed
  (add-hook 'kill-buffer-hook #'greger-ui-show-all-citations nil t)
  (add-hook 'kill-buffer-hook #'greger-ui-clear-tool-overlays nil t))

(defun greger-ui-teardown-folding ()
  "Tear down both citation and tool folding for the current buffer."
  (greger-ui-show-all-citations)
  (greger-ui-clear-tool-overlays)
  (remove-hook 'kill-buffer-hook #'greger-ui-show-all-citations t)
  (remove-hook 'kill-buffer-hook #'greger-ui-clear-tool-overlays t))

(defun greger-ui-refresh-folding ()
  "Refresh tool sections and citations after buffer changes."
  (when (> (point-max) 1)  ; Only if there's actual content
    ;; Clean up stale positions first
    (greger-ui-cleanup-stale-cite-positions)
    ;; Then refresh the UI
    (greger-ui-setup-tool-sections)
    (greger-ui-hide-all-citations)))

(defun greger-ui-setup-citation-folding ()
  "Set up citation folding for the current buffer."
  ;; Clean up any existing overlays first
  (greger-ui-cleanup-dead-overlays)

  ;; Set up fontification
  (greger-ui-setup-cite-fontification)

  ;; Initially hide appropriate citation blocks
  (greger-ui-hide-all-citations)

  ;; Add hook to clean up overlays when buffer is killed
  (add-hook 'kill-buffer-hook #'greger-ui-show-all-citations nil t))

(defun greger-ui-teardown-citation-folding ()
  "Tear down citation folding for the current buffer."
  (greger-ui-show-all-citations)
  (remove-hook 'kill-buffer-hook #'greger-ui-show-all-citations t))

(provide 'greger-ui)

;;; greger-ui.el ends here
