#!/usr/bin/env emacs --script

;; Test if mode-line-format and format-mode-line work in batch mode

(message "Testing mode line in batch mode...")

;; Check if we're in batch mode
(message "noninteractive: %s" noninteractive)

;; Test 1: Check default mode-line-format
(message "default mode-line-format: %S" mode-line-format)

;; Test 2: Try format-mode-line with default format
(let ((result (format-mode-line mode-line-format)))
  (message "format-mode-line result: %S" result)
  (message "format-mode-line result type: %s" (type-of result)))

;; Test 3: Try with a simple string format
(let ((result (format-mode-line "Test")))
  (message "format-mode-line with simple string: %S" result))

;; Test 4: Try setting a custom mode-line-format
(setq mode-line-format '("Custom mode line"))
(let ((result (format-mode-line mode-line-format)))
  (message "format-mode-line with custom format: %S" result))

;; Test 5: Create a buffer and test in that context
(with-temp-buffer
  (message "In temp buffer:")
  (message "  mode-line-format: %S" mode-line-format)
  (let ((result (format-mode-line mode-line-format)))
    (message "  format-mode-line result: %S" result)))

(message "Test complete.")
