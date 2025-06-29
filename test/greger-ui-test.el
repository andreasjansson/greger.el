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

(defun greger-ui-test-font-lock-face-at (text &optional offset)
  "Get the font-lock-face property at TEXT in the current buffer.
OFFSET specifies position within the match (default 0 for match beginning).
Returns nil if TEXT is not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (regexp-quote text) nil t)
      (let ((pos (+ (match-beginning 0) (or offset 0))))
        (when (and (>= pos (match-beginning 0))
                   (< pos (match-end 0)))
          (get-text-property pos 'font-lock-face))))))

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

(ert-deftest greger-ui-test-toggle-folding ()
  (with-current-buffer (greger)
    (erase-buffer)

    ;; Start with folding enabled
    (let ((greger-ui-folding-mode t)
          (original-content "# USER

Let me analyze this with multiple tools and citations.

# ASSISTANT

Here's my analysis with citations and tool use:

# ASSISTANT

 Some analysis here.

## https://example.com/tutorial

Title: Programming Tutorial
Cited text: Best practices for writing clean code
Encrypted index: def456

# THINKING

Signature: think123

I need to use multiple tools to verify this information and provide a comprehensive analysis.

# TOOL USE

Name: read-file
ID: toolu_001

## path

<tool.toolu_001>
test.py
</tool.toolu_001>

# TOOL RESULT

ID: toolu_001

<tool.toolu_001>
def calculate_sum(a, b):
    \"\"\"Calculate the sum of two numbers.\"\"\"
    result = a + b
    print(f\"Sum: {result}\")
    return result

# More complex example
class Calculator:
    def __init__(self):
        self.history = []

    def add(self, x, y):
        result = x + y
        self.history.append(f\"{x} + {y} = {result}\")
        return result

    def multiply(self, x, y):
        result = x * y
        self.history.append(f\"{x} * {y} = {result}\")
        return result
</tool.toolu_001>

# TOOL USE

Name: shell-command
ID: toolu_002

## command

<tool.toolu_002>
python test.py
</tool.toolu_002>

# TOOL RESULT

ID: toolu_002

<tool.toolu_002>
Sum: 15
Calculator created successfully
Addition: 10 + 5 = 15
Multiplication: 3 * 4 = 12
</tool.toolu_002>

")
          (folded-content "# USER

Let me analyze this with multiple tools and citations.

# ASSISTANT

Here's my analysis with citations and tool use: Some analysis here.

# THINKING

I need to use multiple tools to verify this information and provide a comprehensive analysis.

# TOOL USE

Name: read-file

## path

test.py

# TOOL RESULT

def calculate_sum(a, b):
    \"\"\"Calculate the sum of two numbers.\"\"\"
    result = a + b
    print(f\"Sum: {result}\")

# TOOL USE

Name: shell-command

## command

python test.py

# TOOL RESULT

Sum: 15
Calculator created successfully
Addition: 10 + 5 = 15
Multiplication: 3 * 4 = 12

"))
      (insert original-content)

      ;; Force font-lock to process the buffer
      (font-lock-ensure)

      ;; Check that folding is working - content should be folded
      (should (string= folded-content (greger-ui-test--visible-text)))

      ;; Toggle folding off
      (greger-ui-toggle-folding)

      (font-lock-ensure)

      ;; Check that everything is now visible
      (should (string= original-content (greger-ui-test--visible-text)))

      ;; Toggle folding back on
      (greger-ui-toggle-folding)

      (font-lock-ensure)

      ;; Check that folding is working again
      (should (string= folded-content (greger-ui-test--visible-text))))))

(ert-deftest greger-ui-test-write-new-file-syntax-highlighting ()
  "Test syntax highlighting for write-new-file tool."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: write-new-file
ID: toolu_123

## path

<tool.tool001>
test.py
</tool.tool001>

## contents

<tool.tool001>
def hello_world():
    print('Hello world!')
    return 42
</tool.tool001>

")

    ;; Force font-lock to process the buffer
    (font-lock-ensure)

    (should (eq (greger-ui-test-font-lock-face-at "def") 'font-lock-keyword-face))
    (should (eq (greger-ui-test-font-lock-face-at "hello_world") 'font-lock-function-name-face))
    (should (eq (greger-ui-test-font-lock-face-at "'Hello world!'") 'font-lock-string-face))
    (should (eq (greger-ui-test-font-lock-face-at "return") 'font-lock-keyword-face))))

(ert-deftest greger-ui-test-replace-file-syntax-highlighting ()
  "Test syntax highlighting for replace-file tool."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: replace-file
ID: toolu_456

## path

<tool.toolu_456>
script.js
</tool.toolu_456>

## contents

<tool.toolu_456>
function calculateSum(a, b) {
    return a + b;
}
</tool.toolu_456>

")

    ;; Force font-lock to process the buffer
    (font-lock-ensure)

    ;; Check that JavaScript syntax highlighting has been applied
    ;; Check that "function" has keyword face
    (should (eq (greger-ui-test-font-lock-face-at "function") 'font-lock-keyword-face))

    ;; Check that "calculateSum" has function name face
    (should (eq (greger-ui-test-font-lock-face-at "calculateSum") 'font-lock-function-name-face))

    ;; Check that "return" has keyword face
    (should (eq (greger-ui-test-font-lock-face-at "return") 'font-lock-keyword-face))))

(ert-deftest greger-ui-test-read-file-syntax-highlighting ()
  "Test syntax highlighting for read-file tool result."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_789

## path

<tool.toolu_789>
utils.py
</tool.toolu_789>

# TOOL RESULT

ID: toolu_789

<tool.toolu_789>
class Calculator:
    def __init__(self):
        self.result = 0

    def add(self, value):
        self.result += value
        return self.result
</tool.toolu_789>

")

    ;; Force font-lock to process the buffer
    (font-lock-ensure)

    ;; Check that Python syntax highlighting has been applied to the tool result
    ;; Check that "class" has keyword face
    (should (eq (greger-ui-test-font-lock-face-at "class") 'font-lock-keyword-face))

    ;; Check that "Calculator" has type face
    (should (eq (greger-ui-test-font-lock-face-at "Calculator") 'font-lock-type-face))

    ;; Check that "def" has keyword face
    (should (eq (greger-ui-test-font-lock-face-at "def") 'font-lock-keyword-face))

    ;; Check that "return" has keyword face
    (should (eq (greger-ui-test-font-lock-face-at "return") 'font-lock-keyword-face))))

(ert-deftest greger-ui-test-str-replace-diff-and-syntax-highlighting ()
  "Test str-replace diff transformation and syntax highlighting."
  (with-current-buffer (greger)
    (let ((greger-ui-folding-mode nil))
      (erase-buffer)
      ;; Temporarily disable folding mode for this test to prevent invisible text issues
      (insert "# TOOL USE

Name: str-replace
ID: toolu_999

## path

<tool.toolu_999>
example.py
</tool.toolu_999>

## original-content

<tool.toolu_999>
def old_function():
    print('old implementation')
    return False
</tool.toolu_999>

## new-content

<tool.toolu_999>
def new_function():
    print('new implementation')
    return True
</tool.toolu_999>

")

      (font-lock-ensure)

      ;; Check that the content has been transformed to diff format
      (let ((expected "# TOOL USE

Name: str-replace
ID: toolu_999

## path

<tool.toolu_999>
example.py
</tool.toolu_999>

## diff

<tool.toolu_999>
-def old_function():
-    print('old implementation')
-    return False
\\ No newline at end of file
+def new_function():
+    print('new implementation')
+    return True
\\ No newline at end of file

</tool.toolu_999>

"))
        (should (string= expected (greger-ui-test--visible-text))))

      (greger-ui-toggle-folding)
      (font-lock-ensure)

      ;; Check that the content has been transformed to diff format
      (let ((expected "# TOOL USE

Name: str-replace

## path

example.py

## diff

-def old_function():
-    print('old implementation')
-    return False
\\ No newline at end of file

"))
        (should (string= expected (greger-ui-test--visible-text))))

      ;; Check that Python syntax highlighting has been applied to the diff
      ;; The diff transformation applies both syntax highlighting and diff faces
      ;; so we check that font-lock-face properties are present (not nil)
      (should (greger-ui-test-font-lock-face-at "def"))
      (should (greger-ui-test-font-lock-face-at "old_function"))
      (should (greger-ui-test-font-lock-face-at "'old implementation'"))
      (should (greger-ui-test-font-lock-face-at "return")))))

;; Terminal sequence processing tests

(ert-deftest greger-ui-test-process-terminal-sequences-carriage-return-basic ()
  "Test basic carriage return handling - overwrites current line."
  ;; Basic carriage return at end of string
  (with-temp-buffer
    (greger-ui--process-terminal-sequences "old content\rnew content")
    (should (string= "new content" (buffer-string))))
  
  ;; Multiple carriage returns  
  (with-temp-buffer
    (greger-ui--process-terminal-sequences "first\rsecond\rfinal")
    (should (string= "final" (buffer-string))))
  
  ;; Carriage return with newline preservation
  (with-temp-buffer
    (greger-ui--process-terminal-sequences "line1\noriginal\roverwritten\nline3")
    (should (string= "line1\noverwritten\nline3" (buffer-string)))))

(ert-deftest greger-ui-test-process-terminal-sequences-progress-bar-simulation ()
  "Test progress bar simulation with carriage returns."
  ;; Simulate typical progress bar output like wget or homebrew
  (should (string= "Progress: 100% Complete!"
                   (greger-ui--process-terminal-sequences "Downloading file...\rProgress: 25%\rProgress: 50%\rProgress: 100% Complete!")))
  
  ;; Multiple progress bars on separate lines - each line handled independently
  (should (string= "File1: 100%\nFile2: 100%"
                   (greger-ui--process-terminal-sequences "File1: 0%\rFile1: 50%\rFile1: 100%\nFile2: 0%\rFile2: 30%\rFile2: 100%"))))

(ert-deftest greger-ui-test-process-terminal-sequences-homebrew-example ()
  "Test with actual Homebrew-style output containing carriage returns."
  ;; Simulate the exact pattern from the user's example
  (should (string= "==> Downloading https://example.com/file\n######################################################################## 100.0%"
                   (greger-ui--process-terminal-sequences "==> Downloading https://example.com/file\n######################################################################## 100.0%\r###                                                                        4.3%\r######################################################################## 100.0%"))))

(ert-deftest greger-ui-test-process-terminal-sequences-escape-sequences ()
  "Test handling of ANSI escape sequences for cursor movement."
  ;; ESC[K - clear to end of line (just removes the sequence for now)
  (should (string= "keep" 
                   (greger-ui--process-terminal-sequences "keep\e[K")))
  
  ;; ESC[2K - clear entire line (just removes the sequence, text remains)
  (should (string= "remove this" 
                   (greger-ui--process-terminal-sequences "remove this\e[2K")))
  
  ;; ESC[A and ESC[B - not fully implemented yet, just preserve for now
  (should (string= "line1\nremove\e[Afinal"
                   (greger-ui--process-terminal-sequences "line1\nremove\e[Afinal")))
  
  (should (string= "line1\e[Bline3"
                   (greger-ui--process-terminal-sequences "line1\e[Bline3"))))

(ert-deftest greger-ui-test-process-terminal-sequences-mixed-control-codes ()
  "Test mixed control codes and escape sequences."
  ;; Combination of carriage return and escape sequences
  (should (string= "Progress 50%Progress 100%" 
                   (greger-ui--process-terminal-sequences "Start\rProgress 50%\e[KProgress 100%")))
  
  ;; Complex sequence with line clearing and carriage returns
  (should (string= "Line1\nNew Content\nLine3"
                   (greger-ui--process-terminal-sequences "Line1\nOld\e[2K\rNew Content\nLine3"))))

(ert-deftest greger-ui-test-process-terminal-sequences-edge-cases ()
  "Test edge cases in terminal sequence processing."
  ;; Empty string
  (should (string= "" (greger-ui--process-terminal-sequences "")))
  
  ;; Just carriage return
  (should (string= "" (greger-ui--process-terminal-sequences "\r")))
  
  ;; Just escape sequence
  (should (string= "" (greger-ui--process-terminal-sequences "\e[K")))
  
  ;; Text with no control sequences
  (should (string= "normal text\nwith lines" 
                   (greger-ui--process-terminal-sequences "normal text\nwith lines")))
  
  ;; Invalid escape sequence (should be preserved)
  (should (string= "text\e[Zinvalid"
                   (greger-ui--process-terminal-sequences "text\e[Zinvalid")))
  
  ;; ESC without bracket
  (should (string= "text\ealone"
                   (greger-ui--process-terminal-sequences "text\ealone"))))

(ert-deftest greger-ui-test-process-terminal-sequences-newline-preservation ()
  "Test that newlines are properly preserved in various scenarios."
  ;; Text ending with newline should preserve it
  (should (string= "line1\nline2\n"
                   (greger-ui--process-terminal-sequences "line1\nline2\n")))
  
  ;; Text not ending with newline should not add one
  (should (string= "line1\nline2"
                   (greger-ui--process-terminal-sequences "line1\nline2")))
  
  ;; Carriage return at end without newline
  (should (string= "final"
                   (greger-ui--process-terminal-sequences "original\rfinal")))
  
  ;; Carriage return at end with newline
  (should (string= "final\n"
                   (greger-ui--process-terminal-sequences "original\rfinal\n"))))

(ert-deftest greger-ui-test-process-terminal-sequences-real-world-patterns ()
  "Test patterns commonly seen in real-world terminal output."
  ;; Git clone progress
  (should (string= "Receiving objects: 100%"
                   (greger-ui--process-terminal-sequences "Cloning into 'repo'...\rReceiving objects: 50%\rReceiving objects: 100%")))
  
  ;; npm install progress - spinner characters cleaned up
  (should (string= "Installing dependencies...\n✓ package1"
                   (greger-ui--process-terminal-sequences "Installing dependencies...\n⠋ package1\r⠙ package1\r⠹ package1\r✓ package1")))
  
  ;; wget download progress - final state preserved
  (should (string= "file.tar.gz      100%[======>] 12.0MB  1.2MB/s"
                   (greger-ui--process-terminal-sequences "file.tar.gz       10%[=>     ]  1.2MB  500KB/s\rfile.tar.gz      100%[======>] 12.0MB  1.2MB/s"))))

(ert-deftest greger-ui-test-process-terminal-sequences-multiline-with-overwrite ()
  "Test multiline text with overwrite patterns on different lines."
  ;; Each line has its own overwrite pattern
  (should (string= "Line1: done\nLine2: done\nLine3: final"
                   (greger-ui--process-terminal-sequences "Line1: start\rLine1: done\nLine2: start\rLine2: done\nLine3: final")))
  
  ;; Mixed patterns across lines
  (should (string= "Static line\nProgress: 100%\nAnother static line"
                   (greger-ui--process-terminal-sequences "Static line\nProgress: 0%\rProgress: 100%\nAnother static line"))))

(ert-deftest greger-ui-test-process-terminal-sequences-cursor-movement ()
  "Test cursor movement sequences for more complex terminal interactions."
  ;; Cursor sequences are not fully implemented yet - just preserve them
  (should (string= "Line 1\nLine 2\nLine 3\e[A\e[AOverwritten Line 1"
                   (greger-ui--process-terminal-sequences "Line 1\nLine 2\nLine 3\e[A\e[AOverwritten Line 1")))
  
  ;; Cursor down sequences also preserved for now
  (should (string= "Line 1\e[B\e[BLine 4"
                   (greger-ui--process-terminal-sequences "Line 1\e[B\e[BLine 4"))))

(ert-deftest greger-ui-test-process-terminal-sequences-performance ()
  "Test that the function handles large inputs reasonably well."
  ;; Large text with many carriage returns (simulating long progress output)
  (let* ((iterations 1000)
         (large-input (mapconcat (lambda (i) (format "Progress: %d%%\r" (/ (* i 100) iterations)))
                                 (number-sequence 0 iterations) ""))
         (start-time (current-time)))
    
    ;; Add final progress
    (setq large-input (concat large-input "Progress: 100% Complete!"))
    
    (let ((result (greger-ui--process-terminal-sequences large-input)))
      (should (string= "Progress: 100% Complete!" result))
      
      ;; Check that it completes in reasonable time (less than 1 second)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (should (< elapsed 1.0))))))

;;; greger-ui-test.el ends here
