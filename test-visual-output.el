;;; test-visual-output.el --- Test visual output

(add-to-list 'load-path ".")
(require 'greger)
(require 'greger-ui)

(defun test-visual-output ()
  "Test visual output of eval functionality."
  (find-file "test-eval.greger")
  (greger-mode)
  (setq greger-ui-folding-mode t)
  
  (font-lock-flush)
  (font-lock-ensure)
  
  ;; Print what user should see for first eval
  (message "First eval should appear as:")
  (message "  FOLDED:   <eval>1. single-line⇒hello</eval>")
  (message "  UNFOLDED: <eval>1. single-line⇒hello</eval>")
  (message "")
  
  ;; Print what user should see for long eval
  (message "Long eval should appear as:")
  (message "  FOLDED:   <eval>")
  (message "            6. long-result-with-tail")
  (message "            ⇒")
  (message "            line1")
  (message "            line2") 
  (message "            line3")
  (message "            line4")
  (message "            [+4 lines, TAB to expand]")
  (message "            </eval>")
  (message "")
  (message "  UNFOLDED: <eval>")
  (message "            6. long-result-with-tail")
  (message "            ⇒")
  (message "            line1")
  (message "            line2")
  (message "            line3") 
  (message "            line4")
  (message "            line5")
  (message "            line6")
  (message "            line7")
  (message "            line8")
  (message "            </eval>")
  
  (message "")
  (message "Note: eval-result tags should be HIDDEN in both cases")
  (message "The ⇒ arrow replaces the <eval-result-...> tag"))

(test-visual-output)

;;; test-visual-output.el ends here
