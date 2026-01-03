;;; test-end-to-end.el --- True end-to-end tests for greger -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file contains true end-to-end tests that use greger as an end user would,
;; calling only the public API functions `greger` and `greger-buffer`.
;; These tests require a valid ANTHROPIC_API_KEY environment variable.
;;
;; WARNING: These tests make real API calls and may incur costs.
;;

(require 'ert)
(require 'greger)

(defvar greger-test-timeout 30
  "Timeout in seconds for API calls in tests.")

(defun greger-test-parse-tree ()
  (greger-test-treesit-node-to-simplified-tree (treesit-buffer-root-node)))

(defun greger-test-last-assistant-message ()
  (let (start end)
    (save-excursion
      (goto-char (point-max))
      (re-search-backward "^# ASSISTANT")
      (forward-line 2)
      (setq start (point))
      (re-search-forward "^# USER")
      (forward-line -1)
      (forward-char -1)
      (setq end (point)))
    (buffer-substring-no-properties start end)))

(defun greger-test-treesit-node-to-simplified-tree (node)
  "Convert a treesit NODE to a simplified parse tree expression.
Returns a list where the first element is the node type (symbol)
and the remaining elements are the simplified representations of child nodes.
Leaf nodes are wrapped in parentheses as single-element lists."
  (let ((node-type (intern (treesit-node-type node)))
        (children (treesit-node-children node t)))
    (if children
        (cons node-type
              (mapcar #'greger-test-treesit-node-to-simplified-tree children))
      (list node-type))))

(defun greger-test-parse-tree-contains-p (parse-tree name)
  (cond
   ((null parse-tree) nil)
   ((atom parse-tree) (equal parse-tree name))
   ((listp parse-tree)
    (or (greger-test-parse-tree-contains-p (car parse-tree) name)
        (greger-test-parse-tree-contains-p (cdr parse-tree) name)))))

(defun greger-test-mode-line-text (&optional buffer)
  "Get mode line text from BUFFER or current buffer.
Uses greger--mode-line-info to get the greger-specific portion of the mode line."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (if (fboundp 'greger--mode-line-info)
          (greger--mode-line-info)
        ""))))

(defun greger-test-wait-for-status (status &optional timeout)
  "Wait for greger buffer to reach STATUS within TIMEOUT seconds."
  (let ((start-time (current-time))
        (current-status nil)
        (timeout (or timeout greger-test-timeout)))
    (while (and (not (equal status current-status))
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (setq current-status (greger--get-current-status))
      (sit-for 0.2))
    (equal status current-status)))

(ert-deftest greger-end-to-end-test-greger-function ()
  "Test the main greger function creates a buffer and sets it up correctly."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((original-buffers (buffer-list)))
    (unwind-protect
        (progn
          ;; Call the main greger function
          (greger)

          ;; Verify a new greger buffer was created
          (let ((new-buffers (cl-remove-if (lambda (buf) (memq buf original-buffers))
                                           (buffer-list))))
            (should (= 1 (length new-buffers)))
            (let ((greger-buffer (car new-buffers)))
              (should (string-match-p "\\*greger\\*" (buffer-name greger-buffer)))

              ;; Verify buffer is in greger-mode
              (with-current-buffer greger-buffer
                (should (eq major-mode 'greger-mode))

                ;; Verify initial content is set up correctly
                (let ((content (buffer-string)))
                  (should (string-match-p "# SYSTEM" content))
                  (should (string-match-p "# USER" content))
                  (should (string-match-p greger-default-system-prompt content)))

                ;; Verify we're at the end of the buffer (ready for user input)
                (should (= (point) (point-max)))))))

      ;; Cleanup: kill any greger buffers we created
      (dolist (buffer (buffer-list))
        (when (and (string-match-p "\\*greger\\*" (buffer-name buffer))
                   (not (memq buffer original-buffers)))
          (kill-buffer buffer))))))

(ert-deftest greger-end-to-end-test-simple-conversation ()
  "Test a simple conversation using the public API."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger)))

          (goto-char (point-max))
          (insert "Respond with exactly 'Hello from greger test!' and nothing else (don't include the quotes).")

          (let ((greger-current-thinking-budget 0))
            (greger-buffer))

          (should (greger-test-wait-for-status 'idle))

          (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
                (expected "# SYSTEM

You are an agent.

# USER

Respond with exactly 'Hello from greger test!' and nothing else (don't include the quotes).

# ASSISTANT

Hello from greger test!

# USER

"))
            (should (string= expected buffer-contents))))
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-tool-use-conversation ()
  "Test a conversation that involves tool use using the public API."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  ;; Register a basic read-file tool that only accepts path
  (defun greger-test-read-file-basic (path)
    "Simple wrapper around greger-stdlib--read-file that only accepts path."
    (greger-stdlib--read-file path nil nil nil))

  (greger-register-tool "read-file-basic"
                        :description "Read the contents of a file from the filesystem (basic version with only path argument)"
                        :properties '((path . ((type . "string")
                                               (description . "Path to the file to read"))))
                        :required '("path")
                        :function 'greger-test-read-file-basic)

  (let ((greger-buffer nil)
        (test-file nil))
    (unwind-protect
        (progn
          (setq test-file (make-temp-file "greger-test-" nil ".txt"))
          (with-temp-file test-file
            (insert "This is a test file for greger end-to-end testing wahey"))

          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger)))

          (goto-char (point-max))
          (insert (format "Read the file %s and output only the last word of that file, nothing else." test-file))

          (let ((greger-current-thinking-budget 1024)
                (greger-tools '("read-file-basic")))
            (greger-buffer)

            (should (greger-test-wait-for-status 'idle))

            (let ((expected-parse-tree '(source_file
                                         (system
                                          (system_header)
                                          (text))
                                         (user
                                          (user_header)
                                          (text))
                                         (thinking
                                          (thinking_header)
                                          (thinking_signature
                                           (key)
                                           (value))
                                          (text))
                                         (tool_use
                                          (tool_use_header)
                                          (name
                                           (key)
                                           (value))
                                          (id
                                           (key)
                                           (value))
                                          (tool_param
                                           (tool_param_header
                                            (name))
                                           (value
                                            (tool_start_tag)
                                            (tool_content
                                             (tool_content_head))
                                            (tool_end_tag))))
                                         (tool_result
                                          (tool_result_header)
                                          (id
                                           (key)
                                           (value))
                                          (content))
                                         (assistant
                                          (assistant_header)
                                          (text))
                                         (user
                                          (user_header)
                                          (text)))))
              (should (equal expected-parse-tree (greger-test-parse-tree))))

            (should (equal "wahey" (greger-test-last-assistant-message)))))

      (when (and test-file (file-exists-p test-file))
        (delete-file test-file))
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-no-tools-mode ()
  "Test the no-tools mode using C-M-return."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil)
        (test-file nil))
    (unwind-protect
        (progn
          (setq test-file (make-temp-file "greger-test-" nil ".txt"))
          (with-temp-file test-file
            (insert "This file should not be read in no-tools mode wahey."))

          (setq greger-buffer (greger))

          (goto-char (point-max))
          (insert (format "Read the file %s and output only the last word of that file, nothing else." test-file))

          (greger-buffer-no-tools)

          (should (greger-test-wait-for-status 'idle))

          (let ((parse-tree (greger-test-parse-tree)))
            (should (not (greger-test-parse-tree-contains-p parse-tree 'thinking)))
            (should (not (greger-test-parse-tree-contains-p parse-tree 'tool_use)))
            (should (not (greger-test-parse-tree-contains-p parse-tree 'tool_result))))

          (should-not (string-match-p "wahey" (greger-test-last-assistant-message))))

      (when (and test-file (file-exists-p test-file))
        (delete-file test-file))
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-model-configuration ()
  "Test that model configuration works correctly."
  (let ((original-model greger-model)
        (greger-buffer nil))
    (unwind-protect
        (progn
          ;; Test that we can set different supported models
          (setq greger-model 'claude-opus-4-20250514)
          (should (eq greger-model 'claude-opus-4-20250514))

          (setq greger-model 'claude-sonnet-4-20250514)
          (should (eq greger-model 'claude-sonnet-4-20250514))

          ;; Create a greger buffer and verify model is displayed
          (greger)
          (setq greger-buffer (current-buffer))

          ;; The model should be shown in the mode line
          (should (string-match-p "claude-sonnet-4-20250514"
                                  (format "%s" (symbol-name greger-model)))))

      ;; Cleanup
      (setq greger-model original-model)
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-sleep-and-interrupt ()
  "Test sleep command with interruption and state transitions."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger)))

          (goto-char (point-max))
          (re-search-backward "# SYSTEM")
          (forward-line 1)
          (insert "\n<safe-shell-commands>\nsleep 5\n</safe-shell-commands>\n")

          (goto-char (point-max))
          (insert "Run the shell command 'sleep 5' using the shell-command tool.")

          (let ((greger-current-thinking-budget 0)
                (greger-tools '("shell-command")))
            (greger-buffer)

            (should (greger-test-wait-for-status 'executing))

            ;; Wait 1 second while in executing state
            (sit-for 1.0)

            ;; Interrupt execution
            (let ((interrupted-state (greger-interrupt)))
              (should (eq interrupted-state 'executing)))

            (should (greger-test-wait-for-status 'generating))

            ;; Interrupt assistant message before it even started
            (let ((interrupted-state (greger-interrupt)))
              (should (eq interrupted-state 'generating)))

            ;; Should immediately become idle
            (should (greger-test-wait-for-status 'idle 0.1))

            (let ((content (buffer-string)))
              (should (string-match-p "Command failed with exit code 2" content)))

            (insert "Write me a long poem with ten paragraphs about Emacs lisp")

            (greger-buffer)

            (should (greger-test-wait-for-status 'generating))

            ;; Wait for assistant text to start appearing
            (sit-for 3.0)

            ;; Interrupt assistant text generation
            (let ((interrupted-state (greger-interrupt)))
              (should (eq interrupted-state 'generating)))

            (should (greger-test-wait-for-status 'idle 0.1))))

      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-server-tool-web-search ()
  "Test server tool functionality with web search."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger))(greger))

          (let ((greger-server-tools '(web_search)))
            (goto-char (point-max))
            (insert "What is the current weather in San Francisco? Please search for this information and give me a short one-sentence summary.")

            (let ((greger-current-thinking-budget 0))
              (greger-buffer))

            (greger-test-wait-for-status 'idle)

            ;; Verify response was added to buffer
            (let ((content (buffer-string)))
              (should (string-match-p "# ASSISTANT" content))
              ;; Should contain server tool use section
              (should (string-match-p "# SERVER TOOL USE" content))
              (should (string-match-p "Name: web_search" content))
              ;; Should contain server tool result section
              (should (string-match-p "# WEB SEARCH TOOL RESULT" content))
              ;; Should contain some weather-related information
              (should (string-match-p "\\(weather\\|temperature\\|San Francisco\\)" content)))))

      ;; Cleanup
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-thinking ()
  "Test thinking functionality works end-to-end."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger)))

          (insert "2+2")

          (let ((greger-current-thinking-budget 1024))
            (greger-buffer))

          (should (greger-test-wait-for-status 'idle))

          (let ((expected-parse-tree '(source_file
                                       (system
                                        (system_header)
                                        (text))
                                       (user
                                        (user_header)
                                        (text))
                                       (thinking
                                        (thinking_header)
                                        (thinking_signature
                                         (key)
                                         (value))
                                        (text))
                                       (assistant
                                        (assistant_header)
                                        (text))
                                       (user
                                        (user_header)
                                        (text)))))
            (should (equal expected-parse-tree (greger-test-parse-tree))))

          (should (string-match-p "\\(2\\+2\\|four\\|addition\\|math\\)" (buffer-string))))

      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-thinking-toggle ()
  "Test thinking toggle functionality."

  (let ((greger-buffer nil)
        (greger-thinking-budget 2048))
    (unwind-protect
        (progn
          (setq greger-buffer (greger))

          (with-current-buffer greger-buffer
            (should (string-match-p "\\[T:2048\\]" (greger-test-mode-line-text)))

            (greger-toggle-thinking)
            (should (= greger-current-thinking-budget 0))
            (should-not (string-match-p "\\[T:" (greger-test-mode-line-text)))

            (greger-toggle-thinking)
            (should (> greger-current-thinking-budget 0))
            (should (= greger-current-thinking-budget 2048))

            (should (string-match-p "\\[T:2048\\]" (greger-test-mode-line-text)))))

      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-follow-mode ()
  "Test follow-mode functionality and toggle."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger)))

          (with-current-buffer greger-buffer
            ;; Test that follow mode is enabled by default
            (should (eq greger-follow-mode t))

            ;; Position cursor at beginning of buffer
            (goto-char (point-min))
            (let ((initial-point (point)))

              ;; Add a simple message and run without tools (to avoid complex async)
              (goto-char (point-max))
              (insert "Say 'Hello' and nothing else.")

              (let ((greger-current-thinking-budget 0))
                ;; With follow mode enabled, point should move to bottom
                (goto-char initial-point)
                (should (= (point) initial-point))

                (greger-buffer)
                (greger-test-wait-for-status 'idle)

                ;; Point should be at the end now (follow mode behavior)
                (should (= (point) (point-max))))

              ;; Now test with follow mode disabled
              (greger-toggle-follow-mode)
              (should (eq greger-follow-mode nil))

              ;; Clear buffer and start fresh for second test
              (let ((inhibit-read-only t))
                (erase-buffer))
              (insert "# SYSTEM\n\nYou are an agent.\n\n# USER\n\nSay 'Goodbye' and nothing else.")

              ;; Position cursor at beginning again
              (goto-char (point-min))
              (let ((test-point (point)))

                (let ((greger-current-thinking-budget 0))
                  (greger-buffer)
                  (greger-test-wait-for-status 'idle)

                  ;; Point should still be at the beginning (no follow mode)
                  (should (= (point) test-point))))

              ;; Test toggling back to enabled
              (greger-toggle-follow-mode)
              (should (eq greger-follow-mode t)))))

      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-bad-key-from-function ()
  "Test that greger fails when greger-anthropic-key-fn returns a bad key."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil)
        (original-key-fn greger-anthropic-key-fn))
    (unwind-protect
        (progn
          ;; Set greger-anthropic-key-fn to return a bad key
          (setq greger-anthropic-key-fn (lambda () "bad-api-key"))

          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger)))

          (with-current-buffer greger-buffer
            ;; Add a simple message
            (goto-char (point-max))
            (insert "Say 'Hello' and nothing else.")

            ;; Clear any existing warnings
            (when (get-buffer "*Warnings*")
              (with-current-buffer "*Warnings*"
                (erase-buffer)))

            ;; Run greger-buffer with bad key
            (let ((greger-current-thinking-budget 0))
              (greger-buffer)
              (greger-test-wait-for-status 'idle))

            ;; Check that an authentication error warning was generated
            (let ((warnings-buffer (get-buffer "*Warnings*")))
              (should warnings-buffer)
              (with-current-buffer warnings-buffer
                (should (string-match-p "authentication_error.*invalid x-api-key"
                                        (buffer-string)))))))

      ;; Cleanup
      (setq greger-anthropic-key-fn original-key-fn)
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-good-key-from-function-bad-env ()
  "Test that greger works when greger-anthropic-key-fn returns good key but ANTHROPIC_API_KEY is bad."
  (skip-unless (getenv "ANTHROPIC_API_KEY"))

  (let ((greger-buffer nil)
        (original-key-fn greger-anthropic-key-fn)
        (original-env-key (getenv "ANTHROPIC_API_KEY")))
    (unwind-protect
        (progn
          ;; Set environment variable to a bad key
          (setenv "ANTHROPIC_API_KEY" "bad-env-key")

          ;; Set greger-anthropic-key-fn to return the real key
          (setq greger-anthropic-key-fn (lambda () original-env-key))

          (let ((greger-default-system-prompt "You are an agent."))
            (setq greger-buffer (greger)))

          (with-current-buffer greger-buffer
            ;; Add a simple message
            (goto-char (point-max))
            (insert "Say 'Hello' and nothing else.")

            ;; Run greger-buffer - should work because greger-anthropic-key-fn provides good key
            (let ((greger-current-thinking-budget 0))
              (greger-buffer)
              (greger-test-wait-for-status 'idle))

            ;; Verify we got a response
            (let ((response (greger-test-last-assistant-message)))
              (should (string-match-p "Hello\\|hello" response)))))

      ;; Cleanup
      (setq greger-anthropic-key-fn original-key-fn)
      (setenv "ANTHROPIC_API_KEY" original-env-key)
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))



(ert-deftest greger-end-to-end-test-interactive-input-configuration ()
  "Test that interactive input configuration options are available."
  (should (boundp 'greger-stdlib-claude-interactive-input))
  (should (boundp 'greger-stdlib-claude-interactive-timeout))
  (should (fboundp 'greger-stdlib--query-claude-for-interactive-input))
  (should (fboundp 'greger-stdlib--run-shell-command-with-vterm)))

(ert-deftest greger-end-to-end-test-interactive-input-prompt-detection ()
  "Test that interactive prompt detection works correctly."
  (let ((test-prompts '(("Enter your name: " . t)
                       ("Password: " . t)
                       ("Continue? [y/n] " . t)
                       ("Press any key to continue" . t)
                       ("Type your response >> " . t)
                       ("Select option: " . t)
                       ("Just some text" . nil)
                       ("Command output" . nil))))
    (dolist (test test-prompts)
      (let* ((prompt (car test))
             (expected (cdr test))
             (detected (or (string-match-p ":\\s-*$" prompt)
                          (string-match-p "\\?\\s-*$" prompt)
                          (string-match-p "\\]\\s-*$" prompt)
                          (string-match-p ">>\\s-*$" prompt)
                          (string-match-p "Password:" prompt)
                          (string-match-p "Enter " prompt)
                          (string-match-p "\\(y/n\\|Y/N\\)" prompt)
                          (string-match-p "Press" prompt)
                          (string-match-p "Continue" prompt)
                          (string-match-p "Confirm" prompt)
                          (string-match-p "Type" prompt)
                          (string-match-p "Input" prompt)
                          (string-match-p "Select" prompt))))
        (should (eq (not (not detected)) expected))))))

(ert-deftest greger-end-to-end-test-interactive-input-password-detection ()
  "Test that password prompts are detected correctly."
  (let ((password-prompts '("Enter password: " "Password: " "Enter PASS: " "sudo password: "))
        (regular-prompts '("Enter name: " "Continue? " "Select option: " "Username: ")))
    (dolist (prompt password-prompts)
      (should (string-match-p "password\\|Password\\|PASS" prompt)))
    (dolist (prompt regular-prompts)
      (should-not (string-match-p "password\\|Password\\|PASS" prompt)))))

;; Skill tests

(ert-deftest greger-end-to-end-test-skill-tool-loading ()
  "Test that the model discovers and calls the skill tool to load skills.
Creates a skill with a secret code that the model couldn't know without loading it.
The model should see the skill in the tool description and call it."
  (skip-unless (or (getenv "ANTHROPIC_API_KEY") greger-anthropic-key-fn))

  (let* ((temp-dir (make-temp-file "greger-skill-test" t))
         (skill-dir (expand-file-name "secret-keeper" temp-dir))
         (greger-buffer nil)
         (original-skill-dirs greger-skill-directories))
    (unwind-protect
        (progn
          ;; Create skill directory and SKILL.md
          (make-directory skill-dir t)
          (with-temp-file (expand-file-name "SKILL.md" skill-dir)
            (insert "---\n")
            (insert "name: secret-keeper\n")
            (insert "description: Use this skill when asked for secret codes or passphrases\n")
            (insert "---\n\n")
            (insert "# Secret Keeper Skill\n\n")
            (insert "When the user asks for \"the secret code\" or \"the secret passphrase\",\n")
            (insert "respond with exactly this text and nothing else: GREGER_SECRET_7X9Q\n"))

          ;; Set up skill directories and discover base skills
          (setq greger-skill-directories (list temp-dir))
          (greger-skill-discover)

          ;; Verify skill was discovered
          (should (greger-skill-exists-p "secret-keeper"))

          ;; Create greger buffer with skill tag (adds to registry for this session)
          (setq greger-buffer (generate-new-buffer "*greger-skill-test*"))
          (with-current-buffer greger-buffer
            (greger-mode)
            (insert "# SYSTEM\n\n")
            (insert "<skill>secret-keeper</skill>\n\n")
            (insert "You are a helpful assistant. Use the skill tool to load skills when relevant.\n\n")
            (insert "# USER\n\n")
            (insert "What is the secret code? Load the secret-keeper skill to find out.")

            ;; Run agent without thinking for speed
            (let ((greger-current-thinking-budget 0))
              (greger-buffer))

            ;; Wait for completion
            (should (greger-test-wait-for-status 'idle))

            ;; Verify the model called the skill tool
            (let ((buffer-content (buffer-string)))
              (should (string-match-p "# TOOL USE" buffer-content))
              (should (string-match-p "Name: skill" buffer-content))
              ;; Verify the secret is in the response
              (should (string-match-p "GREGER_SECRET_7X9Q" buffer-content)))))

      ;; Cleanup
      (setq greger-skill-directories original-skill-dirs)
      (when (buffer-live-p greger-buffer)
        (kill-buffer greger-buffer))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest greger-end-to-end-test-skill-selects-correct-from-multiple ()
  "Test that the model selects and loads the correct skill from multiple options.
Creates three skills with different secrets and verifies the model loads the right one."
  (skip-unless (or (getenv "ANTHROPIC_API_KEY") greger-anthropic-key-fn))

  (let* ((temp-dir (make-temp-file "greger-skill-test" t))
         (greger-buffer nil)
         (original-skill-dirs greger-skill-directories))
    (unwind-protect
        (progn
          ;; Create three skills with different purposes
          (let ((skill-dir (expand-file-name "color-codes" temp-dir)))
            (make-directory skill-dir t)
            (with-temp-file (expand-file-name "SKILL.md" skill-dir)
              (insert "---\n")
              (insert "name: color-codes\n")
              (insert "description: Use when asked about color codes or hex values\n")
              (insert "---\n\n")
              (insert "The special color code is: #FF5733_COLOR\n")))

          (let ((skill-dir (expand-file-name "math-secrets" temp-dir)))
            (make-directory skill-dir t)
            (with-temp-file (expand-file-name "SKILL.md" skill-dir)
              (insert "---\n")
              (insert "name: math-secrets\n")
              (insert "description: Use when asked about mathematical secrets or special numbers\n")
              (insert "---\n\n")
              (insert "The special number is: 42_MATH_SECRET\n")))

          (let ((skill-dir (expand-file-name "password-vault" temp-dir)))
            (make-directory skill-dir t)
            (with-temp-file (expand-file-name "SKILL.md" skill-dir)
              (insert "---\n")
              (insert "name: password-vault\n")
              (insert "description: Use when asked about passwords or access codes\n")
              (insert "---\n\n")
              (insert "The vault password is: VAULT_PASS_XYZ\n")))

          ;; Discover skills
          (setq greger-skill-directories (list temp-dir))
          (greger-skill-discover)

          ;; Verify all skills were discovered
          (should (greger-skill-exists-p "color-codes"))
          (should (greger-skill-exists-p "math-secrets"))
          (should (greger-skill-exists-p "password-vault"))

          ;; Test: Ask for the math secret - model should load math-secrets skill
          (setq greger-buffer (generate-new-buffer "*greger-skill-multi-test*"))
          (with-current-buffer greger-buffer
            (greger-mode)
            (insert "# SYSTEM\n\n")
            (insert "<skill>color-codes</skill>\n")
            (insert "<skill>math-secrets</skill>\n")
            (insert "<skill>password-vault</skill>\n\n")
            (insert "You are a helpful assistant. Use the skill tool to load skills when needed.\n\n")
            (insert "# USER\n\n")
            (insert "What is the special mathematical secret number?")

            ;; Run agent
            (let ((greger-current-thinking-budget 0))
              (greger-buffer))

            ;; Wait for completion
            (should (greger-test-wait-for-status 'idle))

            ;; Verify the model loaded the correct skill and got the right answer
            (let ((buffer-content (buffer-string)))
              ;; Should have called the skill tool
              (should (string-match-p "# TOOL USE" buffer-content))
              (should (string-match-p "Name: skill" buffer-content))
              ;; Should have loaded math-secrets
              (should (string-match-p "math-secrets" buffer-content))
              ;; Should contain the math secret
              (should (string-match-p "42_MATH_SECRET" buffer-content))
              ;; Should NOT contain the other secrets (didn't load those skills)
              (should-not (string-match-p "#FF5733_COLOR" buffer-content))
              (should-not (string-match-p "VAULT_PASS_XYZ" buffer-content)))))

      ;; Cleanup
      (setq greger-skill-directories original-skill-dirs)
      (when (buffer-live-p greger-buffer)
        (kill-buffer greger-buffer))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest greger-end-to-end-test-skill-from-file-path ()
  "Test that skills can be loaded from file paths via <skill> tags."
  (skip-unless (or (getenv "ANTHROPIC_API_KEY") greger-anthropic-key-fn))

  (let* ((skill-file (make-temp-file "greger-skill" nil ".md"))
         (greger-buffer nil)
         (original-skill-dirs greger-skill-directories))
    (unwind-protect
        (progn
          ;; Create a skill file directly (not in a skill directory)
          (with-temp-file skill-file
            (insert "---\n")
            (insert "name: inline-secret\n")
            (insert "description: A secret from an inline file path\n")
            (insert "---\n\n")
            (insert "The inline file secret is: INLINE_FILE_SECRET_999\n"))

          ;; Clear skill directories so only file-path skill is available
          (setq greger-skill-directories nil)
          (greger-skill-discover)

          ;; Create greger buffer with skill from file path
          (setq greger-buffer (generate-new-buffer "*greger-skill-file-test*"))
          (with-current-buffer greger-buffer
            (greger-mode)
            (insert "# SYSTEM\n\n")
            (insert (format "<skill>%s</skill>\n\n" skill-file))
            (insert "You are a helpful assistant. Use the skill tool to load skills.\n\n")
            (insert "# USER\n\n")
            (insert "Load the inline-secret skill and tell me the secret.")

            ;; Run agent
            (let ((greger-current-thinking-budget 0))
              (greger-buffer))

            ;; Wait for completion
            (should (greger-test-wait-for-status 'idle))

            ;; Verify the secret is in the response
            (let ((buffer-content (buffer-string)))
              (should (string-match-p "INLINE_FILE_SECRET_999" buffer-content)))))

      ;; Cleanup
      (setq greger-skill-directories original-skill-dirs)
      (when (buffer-live-p greger-buffer)
        (kill-buffer greger-buffer))
      (when (file-exists-p skill-file)
        (delete-file skill-file)))))

(provide 'greger-end-to-end-test)

;;; greger-end-to-end-test.el ends here
