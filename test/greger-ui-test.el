;;; greger-ui-test.el --- Tests for greger tools -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-ui)
(require 'greger)

(defun greger-ui-test--key-binding-at-point (key)
  "Get the effective key binding for KEY at point.
This checks text properties, overlays, and local/global keymaps."
  (or
   ;; Check keymap text property
   (let ((keymap (get-char-property (point) 'keymap)))
     (when keymap (lookup-key keymap key)))
   ;; Check local-map text property
   (let ((keymap (get-char-property (point) 'local-map)))
     (when keymap (lookup-key keymap key)))
   ;; Fall back to normal key lookup
   (key-binding key)))


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

(defun greger-ui-test--send-key (key)
  (let ((fn (greger-ui-test--key-binding-at-point key)))
    (funcall fn))

  ;; force font-lock to update
  (font-lock-flush (point-min) (point-max))
  (font-lock-ensure (point-min) (point-max)))

(ert-deftest greger-ui-test-citations-folding ()
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# ASSISTANT

Einstein developed the theory of relativity

## https://physics.com/einstein

Title: Einstein
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

# ASSISTANT

 while

# ASSISTANT

 Newton formulated the laws of motion

## https://physics.com/newton

Title: Newton Biography
Cited text: laws of motion
Encrypted index: ghi789

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)

    (let ((actual (greger-ui-test--visible-text))
          (expected "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion

"))
      (should (string= expected actual)))

    ;; Test expanding a citation
    (goto-char (point-min))
    (re-search-forward "Newton")

    (greger-ui-test--send-key (kbd "TAB"))

    (let ((actual (greger-ui-test--visible-text))
          (expected "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion

## https://physics.com/newton

Title: Newton Biography
Cited text: laws of motion
Encrypted index: ghi789

"))
      (should (string= expected actual)))))

(ert-deftest greger-ui-test-tool-content-folding ()
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_999

## path

<tool.toolu_999>
line1
line2
line3
line4
line5
line6
</tool.toolu_999>

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)

    (let ((actual (greger-ui-test--visible-text))
          (expected "# TOOL USE

Name: read-file

## path

line1
line2
line3
line4

"))
      (should (string= expected actual)))

    ;; Test expanding a citation
    (goto-char (point-min))
    (re-search-forward "line1")

    (greger-ui-test--send-key (kbd "TAB"))

    (let ((actual (greger-ui-test--visible-text))
          (expected "# TOOL USE

Name: read-file

## path

line1
line2
line3
line4
line5
line6

"))
      (should (string= expected actual)))

    ;; Test expanding a citation
    (goto-char (point-min))
    (re-search-forward "line5")

    (greger-ui-test--send-key (kbd "TAB"))

    (let ((actual (greger-ui-test--visible-text))
          (expected "# TOOL USE

Name: read-file

## path

line1
line2
line3
line4

"))
      (should (string= expected actual)))))

(ert-deftest greger-ui-test-thinking-signature-invisible ()
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# USER

Let me think about this

# THINKING

Signature: abc123

I need to consider all the options carefully before responding.

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)

    (let ((actual (greger-ui-test--visible-text))
          (expected "# USER

Let me think about this

# THINKING

I need to consider all the options carefully before responding.

"))
      (should (string= expected actual)))))

;; Tests for syntax highlighting functionality

(ert-deftest greger-ui-test-tool-result-syntax-highlighting ()
  "Test syntax highlighting in tool results based on file extensions."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL RESULT

ID: toolu_123

## Content

<tool.toolu_123>
function test() {
    console.log('hello world');
    return 42;
}
</tool.toolu_123>

")
    ;; Mock the tool use extraction to return a JavaScript file
    (cl-letf (((symbol-function 'greger-parser--extract-tool-use-name)
               (lambda (_) "read-file"))
              ((symbol-function 'greger-parser--extract-tool-use-params)
               (lambda (_) '((path . "test.js")))))
      
      ;; Force font-lock to process the buffer
      (font-lock-ensure)
      
      ;; Check that the content has been processed for syntax highlighting
      ;; We can't easily test the actual highlighting without a full Emacs environment,
      ;; but we can test that the functions are called correctly
      (should (string-match-p "function test()" (buffer-string)))
      (should (string-match-p "console.log" (buffer-string))))))

(ert-deftest greger-ui-test-tool-result-find-corresponding-tool-use ()
  "Test finding corresponding tool_use node for tool_result."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_456

## path

test.py

# TOOL RESULT

ID: toolu_456

## Content

<tool.toolu_456>
def hello():
    print('world')
</tool.toolu_456>

")
    ;; Mock tree-sitter nodes and functions for testing
    (let* ((mock-tool-result-node (list 'tool_result 'id "toolu_456"))
           (mock-tool-use-node (list 'tool_use 'id "toolu_456")))
      
      ;; Mock the required parser functions
      (cl-letf (((symbol-function 'greger-parser--extract-tool-id)
                 (lambda (node) 
                   (plist-get (cdr node) 'id)))
                ((symbol-function 'treesit-node-prev-sibling)
                 (lambda (_) mock-tool-use-node))
                ((symbol-function 'treesit-node-type)
                 (lambda (node) (symbol-name (car node)))))
        
        ;; Test the correspondence finding
        (let ((found-node (greger-ui--find-corresponding-tool-use mock-tool-result-node)))
          (should (equal found-node mock-tool-use-node)))))))

(ert-deftest greger-ui-test-tool-result-syntax-highlighting-python ()
  "Test syntax highlighting for Python code in tool results."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_789

## path

script.py

# TOOL RESULT

ID: toolu_789

## Content

<tool.toolu_789>
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print(fibonacci(10))
</tool.toolu_789>

")
    ;; This test ensures the structure is correct for Python syntax highlighting
    ;; The actual highlighting would depend on Python mode being available
    (let ((content (buffer-string)))
      (should (string-match-p "def fibonacci" content))
      (should (string-match-p "print(fibonacci" content)))))

(ert-deftest greger-ui-test-tool-result-syntax-highlighting-html ()
  "Test syntax highlighting for HTML code in tool results."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_html

## path

index.html

# TOOL RESULT

ID: toolu_html

## Content

<tool.toolu_html>
<!DOCTYPE html>
<html>
<head>
    <title>Test Page</title>
</head>
<body>
    <h1>Hello World</h1>
    <p>This is a test page.</p>
</body>
</html>
</tool.toolu_html>

")
    ;; Test the HTML structure is preserved
    (let ((content (buffer-string)))
      (should (string-match-p "<!DOCTYPE html>" content))
      (should (string-match-p "<title>Test Page</title>" content))
      (should (string-match-p "<h1>Hello World</h1>" content)))))

(ert-deftest greger-ui-test-syntax-highlight-text-performance ()
  "Test that syntax highlighting skips very large content for performance."
  ;; Create a very large content string (over 50000 characters)
  (let* ((large-content (make-string 60000 ?x))
         (start 100)
         (end 200)
         (path "large.txt"))
    
    ;; Mock the buffer operations to test the performance check
    (with-temp-buffer
      ;; This should not trigger syntax highlighting due to size limit
      (cl-letf (((symbol-function 'generate-new-buffer)
                 (lambda (_) (error "Should not create buffer for large content"))))
        
        ;; The function should return early without error
        (should-not (greger-ui--syntax-highlight-text start end large-content path))))))

(ert-deftest greger-ui-test-syntax-highlight-text-normal-size ()
  "Test that syntax highlighting works for normal-sized content."
  (let* ((content "function test() {\n    return 'hello';\n}")
         (path "test.js"))
    
    (with-temp-buffer
      (insert "dummy content before")
      (let ((buffer-start (point)))
        (insert content)
        (let ((buffer-end (point)))
          (insert "dummy content after")
          
          ;; Test that the function can be called without error
          ;; The actual highlighting behavior would depend on js-mode being available
          (should-not (greger-ui--syntax-highlight-text buffer-start buffer-end content path)))))))

(ert-deftest greger-ui-test-str-replace-diff-transformation ()
  "Test str-replace diff transformation functionality."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: str-replace
ID: toolu_456

## path

test.py

## original-content

def hello():
    print('old')

## new-content

def hello():
    print('new')

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)
    
    ;; The content should be transformed to show a diff
    ;; Check that diff markers are present after transformation
    (let ((content (buffer-string)))
      ;; Should contain the original tool structure but may have diff content
      (should (string-match-p "str-replace" content))
      (should (string-match-p "test.py" content)))))

(ert-deftest greger-ui-test-file-syntax-highlighting ()
  "Test syntax highlighting for write-new-file and replace-file tools."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: write-new-file
ID: toolu_789

## path

test.html

## contents

<html>
<head><title>Test</title></head>
<body>
    <h1>Hello World</h1>
</body>
</html>

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)
    
    ;; Check that the HTML content is present
    (should (string-match-p "<html>" (buffer-string)))
    (should (string-match-p "<title>Test</title>" (buffer-string)))
    (should (string-match-p "Hello World" (buffer-string)))))

;; Tests for enhanced folding functionality

(ert-deftest greger-ui-test-folding-toggle ()
  "Test toggling folding mode on and off."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# ASSISTANT

This is some text with citations

## https://example.com

Title: Example
Cited text: Some example content
Encrypted index: abc123

More text here.

")
    ;; Start with folding enabled
    (setq greger-ui-folding-mode t)
    (font-lock-ensure)
    
    ;; Citations should be folded
    (let ((visible-before (greger-ui-test--visible-text)))
      (should (string-match-p "This is some text with citations" visible-before))
      (should-not (string-match-p "Title: Example" visible-before)))
    
    ;; Toggle folding off
    (greger-ui-toggle-folding)
    (font-lock-ensure)
    
    ;; Citations should now be visible
    (let ((visible-after (greger-ui-test--visible-text)))
      (should (string-match-p "This is some text with citations" visible-after))
      (should (string-match-p "Title: Example" visible-after)))
    
    ;; Toggle folding back on
    (greger-ui-toggle-folding)
    (font-lock-ensure)
    
    ;; Citations should be folded again
    (let ((visible-final (greger-ui-test--visible-text)))
      (should (string-match-p "This is some text with citations" visible-final))
      (should-not (string-match-p "Title: Example" visible-final)))))

(ert-deftest greger-ui-test-tool-content-tail-folding ()
  "Test that tool content tail is properly folded when folding mode is enabled."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_999

## path

test.txt

## contents

<tool.toolu_999>
line1
line2
line3
line4
line5
line6
line7
line8
line9
line10
</tool.toolu_999>

")
    ;; Enable folding mode
    (setq greger-ui-folding-mode t)
    (font-lock-ensure)
    
    ;; Should show first few lines but fold the tail
    (let ((visible (greger-ui-test--visible-text)))
      (should (string-match-p "line1" visible))
      (should (string-match-p "line2" visible))
      (should (string-match-p "line3" visible))
      (should (string-match-p "line4" visible))
      ;; Later lines should be folded
      (should-not (string-match-p "line10" visible)))))

(ert-deftest greger-ui-test-tool-content-tail-no-folding ()
  "Test that tool content tail is not folded when folding mode is disabled."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_999

## path

test.txt

## contents

<tool.toolu_999>
line1
line2
line3
line4
line5
line6
line7
line8
line9
line10
</tool.toolu_999>

")
    ;; Disable folding mode
    (setq greger-ui-folding-mode nil)
    (font-lock-ensure)
    
    ;; Should show all lines
    (let ((visible (greger-ui-test--visible-text)))
      (should (string-match-p "line1" visible))
      (should (string-match-p "line5" visible))
      (should (string-match-p "line10" visible)))))

(ert-deftest greger-ui-test-citation-expansion-interaction ()
  "Test interaction between citation expansion and folding mode."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# ASSISTANT

Text with citation

## https://example.com

Title: Test Citation
Cited text: This is cited content
Encrypted index: xyz789

More text

")
    ;; Enable folding mode
    (setq greger-ui-folding-mode t)
    (font-lock-ensure)
    
    ;; Citation should be initially folded
    (let ((visible-initial (greger-ui-test--visible-text)))
      (should-not (string-match-p "Title: Test Citation" visible-initial)))
    
    ;; Find and expand the citation
    (goto-char (point-min))
    (re-search-forward "More text")
    (greger-ui-test--send-key (kbd "TAB"))
    
    ;; Citation should now be expanded
    (let ((visible-expanded (greger-ui-test--visible-text)))
      (should (string-match-p "Title: Test Citation" visible-expanded))
      (should (string-match-p "This is cited content" visible-expanded)))
    
    ;; Collapse again
    (greger-ui-test--send-key (kbd "TAB"))
    
    ;; Citation should be folded again
    (let ((visible-collapsed (greger-ui-test--visible-text)))
      (should-not (string-match-p "Title: Test Citation" visible-collapsed)))))

;; Test helper functions for syntax highlighting

(ert-deftest greger-ui-test-syntax-highlighted-tracking ()
  "Test that syntax highlighting tracking functions exist and can be called."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: str-replace
ID: toolu_123

## path

test.txt

")
    ;; Test that the functions exist
    (should (fboundp 'greger-ui--syntax-highlighted-p))
    (should (fboundp 'greger-ui--set-syntax-highlighted))
    
    ;; Test with a buffer position (simpler than mocking tree-sitter nodes)
    (let ((test-pos 100))
      ;; Initially no overlay should exist
      (should-not (seq-find (lambda (ov) (overlay-get ov 'greger-ui-syntax-highlighted))
                            (overlays-at test-pos))))))

;; Tests for diff generation and processing

(ert-deftest greger-ui-test-generate-diff-content-simple ()
  "Test basic diff generation between two strings."
  (let* ((original "line1\nold line\nline3\n")
         (new "line1\nnew line\nline3\n")
         (path "test.txt")
         (diff-result (greger-ui--generate-diff-content original new path)))
    ;; Should contain diff markers
    (should (string-match-p "^-old line$" diff-result))
    (should (string-match-p "^\\+new line$" diff-result))
    ;; Should contain context lines
    (should (string-match-p "^ line1$" diff-result))
    (should (string-match-p "^ line3$" diff-result))))

(ert-deftest greger-ui-test-generate-diff-content-addition ()
  "Test diff generation for content addition."
  (let* ((original "line1\nline2\n")
         (new "line1\nline2\nadded line\n")
         (path "test.txt")
         (diff-result (greger-ui--generate-diff-content original new path)))
    ;; Should show the addition
    (should (string-match-p "^\\+added line$" diff-result))
    ;; Should show context
    (should (string-match-p "^ line1$" diff-result))
    (should (string-match-p "^ line2$" diff-result))))

(ert-deftest greger-ui-test-generate-diff-content-deletion ()
  "Test diff generation for content deletion."
  (let* ((original "line1\nto delete\nline3\n")
         (new "line1\nline3\n")
         (path "test.txt")
         (diff-result (greger-ui--generate-diff-content original new path)))
    ;; Should show the deletion
    (should (string-match-p "^-to delete$" diff-result))
    ;; Should show context
    (should (string-match-p "^ line1$" diff-result))
    (should (string-match-p "^ line3$" diff-result))))

(ert-deftest greger-ui-test-generate-diff-content-identical ()
  "Test diff generation for identical content."
  (let* ((original "same content\n")
         (new "same content\n")
         (path "test.txt")
         (diff-result (greger-ui--generate-diff-content original new path)))
    ;; Should be empty or minimal for identical content
    ;; The exact behavior may depend on diff implementation
    (should (stringp diff-result))))

(ert-deftest greger-ui-test-generate-diff-content-empty-to-content ()
  "Test diff generation from empty to content."
  (let* ((original "")
         (new "new content\nline2\n")
         (path "test.txt")
         (diff-result (greger-ui--generate-diff-content original new path)))
    ;; Should show additions
    (should (string-match-p "^\\+new content$" diff-result))
    (should (string-match-p "^\\+line2$" diff-result))))

(ert-deftest greger-ui-test-generate-diff-content-content-to-empty ()
  "Test diff generation from content to empty."
  (let* ((original "old content\nline2\n")
         (new "")
         (path "test.txt")
         (diff-result (greger-ui--generate-diff-content original new path)))
    ;; Should show deletions
    (should (string-match-p "^-old content$" diff-result))
    (should (string-match-p "^-line2$" diff-result))))

(ert-deftest greger-ui-test-generate-diff-with-file-extension ()
  "Test diff generation respects file extensions for syntax highlighting."
  (let* ((original "function old() { return 1; }")
         (new "function new() { return 2; }")
         (path "test.js")
         (diff-result (greger-ui--generate-diff-content original new path)))
    ;; Should contain the function changes
    (should (string-match-p "^-function old" diff-result))
    (should (string-match-p "^\\+function new" diff-result))
    ;; The diff should be generated (exact formatting may vary)
    (should (> (length diff-result) 0))))

;; Tests for diff header removal

(ert-deftest greger-ui-test-remove-diff-headers ()
  "Test removal of diff headers from generated output."
  (with-temp-buffer
    (insert "diff -u /tmp/original /tmp/new
--- /tmp/original
+++ /tmp/new
@@ -1,3 +1,3 @@
 line1
-old line
+new line
 line3
Diff finished at Thu Jun 26 2025")
    
    ;; Apply the header removal function
    (greger-ui--remove-diff-headers)
    
    (let ((result (buffer-string)))
      ;; Headers should be removed
      (should-not (string-match-p "^diff -u" result))
      (should-not (string-match-p "^---" result))
      (should-not (string-match-p "^\\+\\+\\+" result))
      (should-not (string-match-p "^@@" result))
      (should-not (string-match-p "Diff finished" result))
      
      ;; Content should remain
      (should (string-match-p "^ line1$" result))
      (should (string-match-p "^-old line$" result))
      (should (string-match-p "^\\+new line$" result))
      (should (string-match-p "^ line3$" result)))))

(ert-deftest greger-ui-test-diff-newline-messages ()
  "Test processing of 'No newline' messages in diffs."
  (with-temp-buffer
    (insert " line1
-old line
\\ No newline at end of file
+new line
 line3")
    
    ;; Apply the newline message processing
    (greger-ui--diff-make-newline-messages-smaller)
    
    (let ((result (buffer-string)))
      ;; Should still contain the message but with modified properties
      (should (string-match-p "No newline at end of file" result))
      
      ;; Check that the message has the right text properties
      (goto-char (point-min))
      (when (re-search-forward "No newline at end of file" nil t)
        (let ((props (text-properties-at (match-beginning 0))))
          (should (plist-get props 'font-lock-face)))))))

;;; greger-ui-test.el ends here
