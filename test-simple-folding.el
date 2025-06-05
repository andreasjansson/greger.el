;;; test-simple-folding.el --- Simple test for citation folding

(require 'greger)
(require 'greger-ui)

(defun test-simple-folding ()
  "Simple test to verify citation folding works."
  (interactive)
  (let ((test-content "## ASSISTANT:

The current king of Sweden is <cite>Carl XVI Gustaf</cite>.

## CITATIONS:

### https://en.wikipedia.org/wiki/Carl_XVI_Gustaf

Title: Carl XVI Gustaf - Wikipedia
Cited text: Carl XVI Gustaf is King of Sweden.


## USER:

hello"))

    (with-temp-buffer
      (insert test-content)
      (greger-mode)

      ;; Test bibliography detection
      (let ((bib-bounds (greger-ui-find-final-bibliography)))
        (if bib-bounds
            (progn
              (message "Bibliography found: %d to %d" (car bib-bounds) (cdr bib-bounds))
              ;; Test the count
              (let ((count (greger-ui-count-citations-in-section (car bib-bounds) (cdr bib-bounds))))
                (message "Citation count: %d" count))
              ;; Test folding
              (greger-ui-hide-final-bibliography (car bib-bounds) (cdr bib-bounds))
              (message "Bibliography folded - check the buffer!")

              ;; Switch to this buffer to see the result
              (switch-to-buffer (current-buffer)))
          (message "No bibliography found"))))))

;; Run the test
(test-simple-folding)
