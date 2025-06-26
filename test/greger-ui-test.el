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

;;; greger-ui-test.el ends here
