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
  '((t (:foreground "#FFFF00" :slant italic)))
  "Face for folded citation summary text."
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
            ;; This is the final bibliography
            (goto-char start)
            (forward-line 1)  ; Skip the header
            (setq end (point-max))
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
         (content-start (save-excursion (goto-char start) (forward-line 1) (point)))
         (overlay (make-overlay content-start end))
         (summary-overlay (make-overlay content-start content-start)))

    ;; Hide the bibliography content (but not the header)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'greger-bibliography t)
    (overlay-put overlay 'evaporate t)
    (push overlay greger-ui-bibliography-overlays)

    ;; Show the summary
    (overlay-put summary-overlay 'after-string
                 (propertize (format "\n[+%d citations, TAB to expand]\n"
                                   count)
                           'face 'greger-folded-citations-face))
    (overlay-put summary-overlay 'greger-bibliography-summary t)
    (overlay-put summary-overlay 'evaporate t)
    (push summary-overlay greger-ui-bibliography-overlays)))

(defun greger-ui-show-final-bibliography (start end)
  "Show the final bibliography section."
  (let ((content-start (save-excursion (goto-char start) (forward-line 1) (point))))
    (remove-overlays content-start end 'greger-bibliography t)
    (remove-overlays content-start end 'greger-bibliography-summary t)
    (setq greger-ui-bibliography-overlays
          (cl-remove-if (lambda (ov)
                          (or (not (overlay-buffer ov))
                              (and (overlay-start ov)
                                   (overlay-end ov)
                                   (>= (overlay-start ov) content-start)
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
    (let ((citation-bounds (greger-ui-find-citation-for-cite-tag)))
      (if citation-bounds
          (let ((start (car citation-bounds))
                (end (cdr citation-bounds)))
            (if (greger-ui-citation-block-hidden-p start end)
                (greger-ui-show-citation-block start end)
              (greger-ui-hide-citation-block start end)))
        (message "No citation block found for this cite tag"))))

   ;; Point is in the final bibliography section
   ((greger-ui-point-in-bibliography-p)
    (let ((bib-bounds (greger-ui-find-final-bibliography)))
      (when bib-bounds
        (let ((start (car bib-bounds))
              (end (cdr bib-bounds)))
          (if (greger-ui-bibliography-hidden-p start end)
              (greger-ui-show-final-bibliography start end)
            (greger-ui-hide-final-bibliography start end))))))

   ;; Not in a cite tag or bibliography
   (t
    (message "TAB only works inside <cite> tags or in the bibliography section"))))

(defun greger-ui-hide-all-citations ()
  "Hide all citation blocks that should be folded."
  (interactive)
  (let ((pairs (greger-ui-find-all-cite-citation-pairs)))
    (dolist (pair pairs)
      (let ((citation-bounds (cdr pair)))
        (greger-ui-hide-citation-block (car citation-bounds) (cdr citation-bounds)))))

  ;; Also hide the final bibliography section
  (let ((bib-bounds (greger-ui-find-final-bibliography)))
    (when bib-bounds
      (greger-ui-hide-final-bibliography (car bib-bounds) (cdr bib-bounds)))))

(defun greger-ui-show-all-citations ()
  "Show all citation blocks."
  (interactive)
  (remove-overlays (point-min) (point-max) 'greger-citation t)
  (remove-overlays (point-min) (point-max) 'greger-bibliography t)
  (remove-overlays (point-min) (point-max) 'greger-bibliography-summary t)
  (setq greger-ui-cite-citation-overlays nil)
  (setq greger-ui-bibliography-overlays nil))

(defun greger-ui-cleanup-dead-overlays ()
  "Remove dead overlays from citation overlays lists."
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
                          greger-ui-bibliography-overlays)))

;;; Font Lock Support

(defun greger-ui-setup-cite-fontification ()
  "Set up font-lock for cite tags."
  (font-lock-add-keywords
   nil
   `((,greger-ui-cite-tag-regex
      (0 'greger-cite-tag-face)))
   'append))

;;; Integration Functions

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
