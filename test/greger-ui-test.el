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

(ert-deftest greger-ui-test-toggle-folding ()
  (with-current-buffer (greger)
    (erase-buffer)
    
    ;; Start with folding enabled
    (let ((greger-ui-folding-mode t))
      (insert "# USER

Let me analyze this with multiple tools and citations.

# ASSISTANT

Here's my analysis with citations and tool use:

## https://example.com/docs

Title: Code Documentation
Cited text: This function performs mathematical operations efficiently
Encrypted index: abc123

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

test.py

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

# TOOL RESULT

ID: toolu_001

File contents read successfully.

# TOOL USE

Name: shell-command
ID: toolu_002

## command

python test.py

<tool.toolu_002>
Sum: 15
Calculator created successfully
Addition: 10 + 5 = 15
Multiplication: 3 * 4 = 12
</tool.toolu_002>

# TOOL RESULT

ID: toolu_002

Command executed successfully.

")
      
      ;; Force font-lock to process the buffer
      (font-lock-ensure)
      
      ;; Check that folding is working - content should be folded
      (let ((expected "# USER

Let me analyze this with multiple tools and citations.

# ASSISTANT

Here's my analysis with citations and tool use:Some analysis here.

# THINKING

I need to use multiple tools to verify this information and provide a comprehensive analysis.

# TOOL USE

Name: read-file

## path

test.py

def calculate_sum(a, b):
    \"\"\"Calculate the sum of two numbers.\"\"\"
    result = a + b
    print(f\"Sum: {result}\")

# TOOL RESULT

File contents read successfully.

# TOOL USE

Name: shell-command

## command

python test.py

Sum: 15
Calculator created successfully
Addition: 10 + 5 = 15

# TOOL RESULT

Command executed successfully.

"))
        (should (string= expected (greger-ui-test--visible-text))))
      
      ;; Toggle folding off
      (greger-ui-toggle-folding)
      
      ;; Check that everything is now visible  
      (let ((expected "# USER

Let me analyze this with multiple tools and citations.

# ASSISTANT

Here's my analysis with citations and tool use:

## https://example.com/docs

Title: Code Documentation
Cited text: This function performs mathematical operations efficiently
Encrypted index: abc123

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

test.py

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

# TOOL RESULT

ID: toolu_001

File contents read successfully.

# TOOL USE

Name: shell-command
ID: toolu_002

## command

python test.py

<tool.toolu_002>
Sum: 15
Calculator created successfully
Addition: 10 + 5 = 15
Multiplication: 3 * 4 = 12
</tool.toolu_002>

# TOOL RESULT

ID: toolu_002

Command executed successfully.

"))
        (should (string= expected (greger-ui-test--visible-text))))
      
      ;; Toggle folding back on
      (greger-ui-toggle-folding)
      
      ;; Check that folding is working again
      (let ((expected "# USER

Let me analyze this with multiple tools and citations.

# ASSISTANT

Here's my analysis with citations and tool use:Some analysis here.

# THINKING

I need to use multiple tools to verify this information and provide a comprehensive analysis.

# TOOL USE

Name: read-file

## path

test.py

def calculate_sum(a, b):
    \"\"\"Calculate the sum of two numbers.\"\"\"
    result = a + b
    print(f\"Sum: {result}\")

# TOOL RESULT

File contents read successfully.

# TOOL USE

Name: shell-command

## command

python test.py

Sum: 15
Calculator created successfully
Addition: 10 + 5 = 15

# TOOL RESULT

Command executed successfully.

"))
        (should (string= expected (greger-ui-test--visible-text)))))))

;;; greger-ui-test.el ends here
