;;; Test to reproduce empty tool result content issue

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'greger-stdlib)
(require 'vterm)

(defvar test-streaming-calls '())
(defvar test-final-output nil)
(defvar test-final-error nil)
(defvar test-completed nil)

(defun test-streaming-callback (text)
  "Capture streaming calls."
  (push text test-streaming-calls)
  (message "STREAMING: %s" (if (> (length text) 100) 
                               (concat (substring text 0 100) "...")
                             text)))

(defun test-final-callback (output error)
  "Capture final callback."
  (setq test-final-output output)
  (setq test-final-error error)
  (setq test-completed t)
  (message "FINAL CALLBACK:")
  (message "  Output: %s" (if output 
                              (if (> (length output) 100)
                                  (concat (substring output 0 100) "...")
                                output)
                            "NIL"))
  (message "  Error: %s" error))

(defun test-empty-result-reproduction ()
  "Test that reproduces the empty result issue."
  (interactive)
  (message "=== TESTING EMPTY RESULT ISSUE ===")
  
  ;; Reset test variables
  (setq test-streaming-calls '())
  (setq test-final-output nil)
  (setq test-final-error nil)
  (setq test-completed nil)
  
  ;; Run the shell command with vterm
  (greger-stdlib--shell-command
   "echo 'Hello World'"                 ; Simple command
   #'test-final-callback                ; callback
   nil                                  ; working-directory
   nil                                  ; timeout
   nil                                  ; enable-environment
   t                                    ; use-vterm
   #'test-streaming-callback            ; streaming-callback
   '(:safe-shell-commands ("echo") :allow-all-shell-commands t)) ; metadata
  
  (message "Command submitted, waiting for completion...")
  
  ;; Wait for completion
  (let ((wait-count 0))
    (while (and (not test-completed) (< wait-count 100))
      (sleep-for 0.1)
      (setq wait-count (1+ wait-count)))
    
    (message "=== RESULTS ===")
    (message "Completed: %s" test-completed)
    (message "Number of streaming calls: %d" (length test-streaming-calls))
    (message "Final output: %s" (if test-final-output 
                                    (concat "'" test-final-output "'")
                                  "NIL"))
    (message "Final error: %s" test-final-error)
    
    ;; Show streaming calls in reverse order (newest first)
    (message "=== STREAMING CALLS (newest first) ===")
    (let ((i 0))
      (dolist (call (reverse test-streaming-calls))
        (message "%d: %s" i (if (> (length call) 200)
                               (concat (substring call 0 200) "...")
                             call))
        (setq i (1+ i))
        (when (> i 5) (return))))  ; Only show first 5
    
    ;; Diagnose the issue
    (cond
     ((not test-completed)
      (message "❌ ISSUE: Command did not complete within timeout"))
     ((not test-final-output)
      (message "❌ ISSUE: Final output is NIL"))
     ((string= test-final-output "")
      (message "❌ ISSUE: Final output is empty string"))
     (t
      (message "✅ PASS: Final output received: %s" test-final-output)))))

;; Run the test
(test-empty-result-reproduction)
