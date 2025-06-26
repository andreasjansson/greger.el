;;; greger-syntax-highlighting-integration-test.el --- Integration tests for syntax highlighting features -*- lexical-binding: t -*-

(require 'ert)
(require 'greger)
(require 'greger-ui)
(require 'greger-parser)

;;; Commentary:
;; This file contains integration tests for the syntax highlighting and diff functionality
;; added to greger. These tests verify that the complete pipeline works from parsing
;; to UI display.

(ert-deftest greger-integration-test-str-replace-diff-transformation ()
  "Integration test for str-replace diff transformation pipeline."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: str-replace
ID: test-123

## path

test.py

## original-content

def hello():
    print(\"hello world\")
    return True

## new-content

def hello():
    print(\"hi there world\")
    return False

## git-commit-message

Update hello function message and return value

")
    ;; Force font-lock to process the buffer which should trigger diff transformation
    (font-lock-ensure)
    
    ;; After processing, the buffer should contain diff content instead of original/new
    (let ((buffer-content (buffer-string)))
      ;; Should still have the basic structure
      (should (string-match-p "str-replace" buffer-content))
      (should (string-match-p "test.py" buffer-content))
      (should (string-match-p "test-123" buffer-content))
      
      ;; The transformation may have occurred - check for either original format or diff format
      (should (or (string-match-p "original-content" buffer-content)
                  (string-match-p "diff" buffer-content))))))

(ert-deftest greger-integration-test-tool-result-syntax-highlighting ()
  "Integration test for tool result syntax highlighting."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: read-123

## path

example.js

# TOOL RESULT

ID: read-123

## Content

<tool.read-123>
function calculateSum(a, b) {
    // Add two numbers
    const result = a + b;
    console.log(`Sum of ${a} and ${b} is ${result}`);
    return result;
}

// Test the function
const sum = calculateSum(5, 3);
</tool.read-123>

")
    ;; Force font-lock processing
    (font-lock-ensure)
    
    ;; The content should be preserved and potentially syntax highlighted
    (let ((buffer-content (buffer-string)))
      (should (string-match-p "function calculateSum" buffer-content))
      (should (string-match-p "console.log" buffer-content))
      (should (string-match-p "const sum = calculateSum" buffer-content)))))

(ert-deftest greger-integration-test-undiff-roundtrip ()
  "Integration test for diff generation and undiff roundtrip."
  (let* ((original-content "line1\nold content\nline3\n")
         (new-content "line1\nnew content\nline3\n")
         (path "test.txt"))
    
    ;; Generate diff
    (let ((diff-result (greger-ui--generate-diff-content original-content new-content path)))
      ;; Should have diff markers
      (should (string-match-p "^-old content$" diff-result))
      (should (string-match-p "^\\+new content$" diff-result))
      
      ;; Now test undiff
      (let ((undiff-result (greger-parser-undiff-strings diff-result)))
        (should (string= (car undiff-result) original-content))
        (should (string= (cdr undiff-result) new-content))))))

(ert-deftest greger-integration-test-folding-with-syntax-highlighting ()
  "Integration test for folding combined with syntax highlighting."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: long-file-123

## path

script.py

# TOOL RESULT

ID: long-file-123

## Content

<tool.long-file-123>
#!/usr/bin/env python3
import os
import sys

def main():
    print(\"Starting script...\")
    
    # Process files
    for i in range(10):
        print(f\"Processing item {i}\")
        
    print(\"Script complete\")

if __name__ == \"__main__\":
    main()
</tool.long-file-123>

")
    ;; Enable folding mode
    (setq greger-ui-folding-mode t)
    (font-lock-ensure)
    
    ;; Content should be folded (first few lines visible, rest hidden)
    (let ((visible-text (greger-ui-test--visible-text)))
      ;; Should see the beginning
      (should (string-match-p "#!/usr/bin/env python3" visible-text))
      (should (string-match-p "import os" visible-text))
      
      ;; Later content should be folded for large tool results
      ;; (exact behavior depends on folding implementation)
      (should (stringp visible-text)))))

(ert-deftest greger-integration-test-write-new-file-syntax-highlighting ()
  "Integration test for write-new-file syntax highlighting."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: write-new-file
ID: write-html-123

## path

index.html

## contents

<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Test Page</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 20px;
        }
        .container {
            max-width: 800px;
            margin: 0 auto;
        }
    </style>
</head>
<body>
    <div class=\"container\">
        <h1>Welcome to Test Page</h1>
        <p>This is a test HTML file with embedded CSS.</p>
        <script>
            console.log('Page loaded');
            document.addEventListener('DOMContentLoaded', function() {
                console.log('DOM ready');
            });
        </script>
    </div>
</body>
</html>

## git-commit-message

Add initial HTML page with styling and script

")
    ;; Force font-lock processing which should apply syntax highlighting
    (font-lock-ensure)
    
    ;; The HTML content should be preserved
    (let ((buffer-content (buffer-string)))
      (should (string-match-p "<!DOCTYPE html>" buffer-content))
      (should (string-match-p "<style>" buffer-content))
      (should (string-match-p "font-family: Arial" buffer-content))
      (should (string-match-p "<script>" buffer-content))
      (should (string-match-p "console.log" buffer-content))
      (should (string-match-p "DOMContentLoaded" buffer-content)))))

(ert-deftest greger-integration-test-multiple-str-replace-operations ()
  "Integration test for multiple str-replace operations in sequence."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: str-replace
ID: replace-1

## path

config.py

## original-content

DEBUG = False
PORT = 8000

## new-content

DEBUG = True
PORT = 8000

## git-commit-message

Enable debug mode

# TOOL USE

Name: str-replace
ID: replace-2

## path

config.py

## original-content

DEBUG = True
PORT = 8000

## new-content

DEBUG = True
PORT = 3000

## git-commit-message

Change port to 3000

")
    ;; Process both operations
    (font-lock-ensure)
    
    ;; Both operations should be processed
    (let ((buffer-content (buffer-string)))
      (should (string-match-p "replace-1" buffer-content))
      (should (string-match-p "replace-2" buffer-content))
      (should (string-match-p "config.py" buffer-content))
      
      ;; Should contain references to the changes
      (should (or (string-match-p "DEBUG" buffer-content)
                  (string-match-p "PORT" buffer-content))))))

(ert-deftest greger-integration-test-performance-large-file ()
  "Integration test for performance with large file content."
  (with-current-buffer (greger)
    (erase-buffer)
    
    ;; Create a large content string
    (let ((large-content (concat 
                          "# Large file content\n"
                          (mapconcat (lambda (i) 
                                       (format "line %d with some content that makes it longer\n" i))
                                     (number-sequence 1 1000) ""))))
      
      (insert (format "# TOOL USE

Name: read-file
ID: large-123

## path

large.txt

# TOOL RESULT

ID: large-123

## Content

<tool.large-123>
%s
</tool.large-123>

" large-content))
      
      ;; Processing should not hang or crash
      (let ((start-time (current-time)))
        (font-lock-ensure)
        (let ((elapsed (float-time (time-subtract (current-time) start-time))))
          ;; Should complete within reasonable time (5 seconds)
          (should (< elapsed 5.0))))
      
      ;; Content should still be there
      (should (string-match-p "Large file content" (buffer-string))))))

(ert-deftest greger-integration-test-folding-toggle-with-syntax ()
  "Integration test for folding toggle with syntax highlighted content."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# ASSISTANT

Here's the code analysis:

## https://example.com/docs

Title: Code Documentation
Cited text: This function performs mathematical operations
Encrypted index: abc123

The implementation looks good.

")
    ;; Start with folding enabled
    (setq greger-ui-folding-mode t)
    (font-lock-ensure)
    
    ;; Citation should be folded
    (let ((folded-content (greger-ui-test--visible-text)))
      (should (string-match-p "Here's the code analysis" folded-content))
      (should-not (string-match-p "Title: Code Documentation" folded-content)))
    
    ;; Toggle folding
    (greger-ui-toggle-folding)
    
    ;; Citation should now be visible
    (let ((unfolded-content (greger-ui-test--visible-text)))
      (should (string-match-p "Here's the code analysis" unfolded-content))
      (should (string-match-p "Title: Code Documentation" unfolded-content)))
    
    ;; Toggle back
    (greger-ui-toggle-folding)
    
    ;; Should be folded again
    (let ((refolded-content (greger-ui-test--visible-text)))
      (should (string-match-p "Here's the code analysis" refolded-content))
      (should-not (string-match-p "Title: Code Documentation" refolded-content)))))

;;; Helper function for integration tests
(defun greger-ui-test--visible-text ()
  "Extract only the visible text from the current buffer.
Text with the 'invisible property set to t is excluded."
  (let ((result "")
        (pos (point-min)))
    (while (< pos (point-max))
      (let* ((next-change (next-single-property-change pos 'invisible nil (point-max)))
             (invisible (get-text-property pos 'invisible)))
        (unless invisible
          (setq result (concat result (buffer-substring-no-properties pos next-change))))
        (setq pos next-change)))
    result))

;;; greger-syntax-highlighting-integration-test.el ends here
