;;; test-citation-folding.el --- Test citation folding functionality

(require 'greger)
(require 'greger-ui)

(defun test-citation-folding ()
  "Test the citation folding functionality."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "test-citations.md")
    (greger-mode)

    ;; Test final bibliography detection
    (let ((bib-bounds (greger-ui-find-final-bibliography)))
      (if bib-bounds
          (let ((start (car bib-bounds))
                (end (cdr bib-bounds)))
            (message "Bibliography found from %d to %d" start end)
            (message "Content after CITATIONS header: %S"
                     (buffer-substring-no-properties start end))
            ;; Check that it doesn't include the USER section
            (goto-char end)
            (if (looking-at "## USER:")
                (message "✓ Bibliography correctly stops before USER section")
              (message "✗ Bibliography does not stop at USER section"))
            ;; Test counting citations
            (let ((count (greger-ui-count-citations-in-section start end)))
              (message "Found %d citations in bibliography" count)))
        (message "✗ No bibliography found")))

    ;; Test cite-citation pairs
    (let ((pairs (greger-ui-find-all-cite-citation-pairs)))
      (message "Found %d cite-citation pairs" (length pairs))
      (dolist (pair pairs)
        (let ((cite-bounds (car pair))
              (citation-bounds (cdr pair)))
          (message "Cite: %S -> Citation: %d-%d"
                   (buffer-substring-no-properties (car cite-bounds) (cdr cite-bounds))
                   (car citation-bounds) (cdr citation-bounds)))))))

(defun test-bibliography-folding-visual ()
  "Test the visual aspects of bibliography folding."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "test-citations.md")
    (greger-mode)

    ;; Test folding the bibliography
    (let ((bib-bounds (greger-ui-find-final-bibliography)))
      (when bib-bounds
        (message "Testing bibliography folding...")
        (greger-ui-hide-final-bibliography (car bib-bounds) (cdr bib-bounds))
        (message "Bibliography should now be folded with indicator")

        ;; Test unfolding
        (sit-for 2)
        (greger-ui-show-final-bibliography (car bib-bounds) (cdr bib-bounds))
        (message "Bibliography should now be unfolded")))))

;; Run both tests
(test-citation-folding)
(test-bibliography-folding-visual)
