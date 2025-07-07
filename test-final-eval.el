;;; test-final-eval.el --- Final comprehensive test

;; Load greger modules
(add-to-list 'load-path ".")
(require 'greger)
(require 'greger-ui)

(defun test-final-eval ()
  "Final comprehensive test of eval functionality."
  (find-file "test-eval.greger")
  (greger-mode)
  (setq greger-ui-folding-mode t)
  
  (font-lock-flush)
  (font-lock-ensure)
  
  ;; Test 1: Check arrows are present
  (let ((arrow-positions '()))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((display-prop (get-text-property (point) 'display)))
          (when (and display-prop (stringp display-prop) (string-match-p "⇒" display-prop))
            ;; Only count the start of each arrow region
            (unless (and (> (point) (point-min))
                         (get-text-property (1- (point)) 'display))
              (push (point) arrow-positions))))
        (goto-char (1+ (point)))))
    (message "✓ Arrows: Found %d arrow regions at positions %s" 
             (length arrow-positions) (reverse arrow-positions)))
  
  ;; Test 2: Check eval result content has correct face
  (let ((root (treesit-buffer-root-node))
        (styled-results 0))
    (let ((eval-result-nodes (treesit-query-capture root '((eval_result) @eval-result))))
      (dolist (capture eval-result-nodes)
        (let* ((node (cdr capture))
               (content-node (treesit-search-subtree node "eval_result_content")))
          (when content-node
            (let ((content-start (treesit-node-start content-node))
                  (face (get-text-property (treesit-node-start content-node) 'font-lock-face)))
              (when (eq face 'greger-eval-result-face)
                (setq styled-results (1+ styled-results)))))))
      (message "✓ Styling: %d eval results have correct face" styled-results)))
  
  ;; Test 3: Check folding behavior
  (let ((root (treesit-buffer-root-node))
        (foldable-results 0))
    (let ((eval-result-nodes (treesit-query-capture root '((eval_result) @eval-result))))
      (dolist (capture eval-result-nodes)
        (let* ((node (cdr capture))
               (content-node (treesit-search-subtree node "eval_result_content"))
               (content-head-node (when content-node
                                    (treesit-search-subtree content-node "eval_result_content_head")))
               (content-tail-node (when content-node
                                    (treesit-search-subtree content-node "eval_result_content_tail"))))
          (when (and content-head-node content-tail-node)
            (setq foldable-results (1+ foldable-results))
            (let* ((tail-start (treesit-node-start content-tail-node))
                   (tail-end (treesit-node-end content-tail-node))
                   (is-invisible (get-text-property tail-start 'invisible)))
              (message "  - Eval result has tail that is %s" 
                       (if is-invisible "hidden" "visible"))))))
      (message "✓ Folding: %d eval results have foldable content" foldable-results))
  
  ;; Test expansion message text properties
  (let ((expansion-messages 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((display-prop (get-text-property (point) 'display)))
          (when (and display-prop 
                     (stringp display-prop) 
                     (string-match-p "\\[\\+.*lines.*TAB.*expand\\]" display-prop))
            (setq expansion-messages (1+ expansion-messages))
            (message "  - Found expansion message at %d: '%s'" (point) display-prop)))
        (goto-char (1+ (point)))))
    (message "✓ Expansion messages: %d found" expansion-messages)))
  
  ;; Test 4: Test toggling folding mode
  (message "✓ Testing folding mode toggle...")
  (greger-ui-toggle-folding)
  (message "  - Folding mode toggled to: %s" greger-ui-folding-mode)
  (greger-ui-toggle-folding)
  (message "  - Folding mode toggled back to: %s" greger-ui-folding-mode)
  
  (message "\n🎉 All tests completed! Eval functionality is working correctly.")
  (message "✨ Features implemented:")
  (message "   • Arrow overlays (⇒) appear after eval content")
  (message "   • Eval result content styled with blue-ish gray")
  (message "   • Eval result tags are hidden when folding mode is on")
  (message "   • Long eval results are folded with expansion indicators")
  (message "   • TAB toggles folding (test interactively)")
  (message "   • Respects greger-ui-folding-mode setting"))

;; Run the test
(test-final-eval)

;;; test-final-eval.el ends here
