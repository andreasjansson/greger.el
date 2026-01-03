;;; greger-skill-test.el --- Tests for greger skills system -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-skill)

;; Test helper functions

(defvar greger-plugin-test--temp-dir nil
  "Temporary directory for test skills.")

(defun greger-plugin-test--setup-temp-dir ()
  "Create temporary directory for test skills."
  (setq greger-plugin-test--temp-dir (make-temp-file "greger-skills-test" t))
  greger-plugin-test--temp-dir)

(defun greger-plugin-test--cleanup-temp-dir ()
  "Clean up temporary directory."
  (when (and greger-plugin-test--temp-dir
             (file-directory-p greger-plugin-test--temp-dir))
    (delete-directory greger-plugin-test--temp-dir t)
    (setq greger-plugin-test--temp-dir nil)))

(defun greger-plugin-test--create-skill (name description content)
  "Create a test skill with NAME, DESCRIPTION, and CONTENT."
  (let ((skill-dir (expand-file-name name greger-plugin-test--temp-dir)))
    (make-directory skill-dir t)
    (with-temp-file (expand-file-name "SKILL.md" skill-dir)
      (insert "---\n")
      (insert (format "name: %s\n" name))
      (insert (format "description: %s\n" description))
      (insert "---\n\n")
      (insert content))
    skill-dir))

(defun greger-plugin-test--cleanup-registry ()
  "Clean up the skill registry."
  (clrhash greger-skill-registry))

;; Skill discovery tests

(ert-deftest greger-skill-test-discover-skills ()
  "Test that skills are discovered from directories."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "test-skill" "A test skill" "# Test\n\nDo the thing.")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (should (greger-skill-exists-p "test-skill"))
          (let ((skill (greger-skill-get "test-skill")))
            (should (equal "test-skill" (greger-skill-name skill)))
            (should (equal "A test skill" (greger-skill-description skill)))
            (should (string-match-p "Do the thing" (greger-skill-content skill))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-discover-multiple-skills ()
  "Test that multiple skills are discovered."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "skill-one" "First skill" "Content one")
        (greger-plugin-test--create-skill "skill-two" "Second skill" "Content two")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (should (greger-skill-exists-p "skill-one"))
          (should (greger-skill-exists-p "skill-two"))
          (should (= 2 (length (greger-skill-list))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-list ()
  "Test greger-skill-list returns all skills."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "alpha" "Alpha skill" "Alpha content")
        (greger-plugin-test--create-skill "beta" "Beta skill" "Beta content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((skills (greger-skill-list)))
            (should (member "alpha" skills))
            (should (member "beta" skills)))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-list-with-descriptions ()
  "Test greger-skill-list-with-descriptions formats correctly."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "my-skill" "Does something useful" "Content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((listing (greger-skill-list-with-descriptions)))
            (should (string-match-p "my-skill" listing))
            (should (string-match-p "Does something useful" listing)))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

;; Skill loading tests

(ert-deftest greger-skill-test-load-skill ()
  "Test loading a skill by name."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "loader-test" "Test loading" "# Instructions\n\nDo this.")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((content (greger-skill--load "loader-test")))
            (should (string-match-p "Skill: loader-test" content))
            (should (string-match-p "Do this" content)))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-load-nonexistent-skill ()
  "Test loading a skill that doesn't exist."
  (greger-plugin-test--cleanup-registry)
  (let ((content (greger-skill--load "nonexistent")))
    (should (string-match-p "not found" content))
    (should (string-match-p "Available skills" content))))

;; Skill tool registration tests

(ert-deftest greger-skill-test-skill-tool-registered ()
  "Test that the skill tool is registered."
  (should (gethash "skill" greger-tools-registry))
  (should (gethash "skill-list" greger-tools-registry)))

(ert-deftest greger-skill-test-skill-tool-execution ()
  "Test executing the skill tool."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "exec-test" "Execution test" "Execute instructions")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((result nil) (error nil))
            (greger-tools-execute :tool-name "skill"
                                  :args '((name . "exec-test"))
                                  :callback (lambda (r e) (setq result r error e)))
            (should (null error))
            (should (string-match-p "Execute instructions" result)))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-list-tool-execution ()
  "Test executing the skill-list tool."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "list-test" "List test skill" "Content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((result nil) (error nil))
            (greger-tools-execute :tool-name "skill-list"
                                  :args '()
                                  :callback (lambda (r e) (setq result r error e)))
            (should (null error))
            (should (string-match-p "list-test" result))
            (should (string-match-p "List test skill" result)))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

;; Buffer parsing tests for <skill> tags

(ert-deftest greger-skill-test-parse-system-skill ()
  "Test that skills in SYSTEM section apply to whole session."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "system-skill" "System skill" "System content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>system-skill</skill>\n\nYou are an agent.\n\n# USER\n\nHello")
            (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
              (should (member "system-skill" (plist-get parsed :session-skills)))
              (should (null (plist-get parsed :turn-skills)))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-parse-user-skill ()
  "Test that skills in USER section apply only to that turn."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "user-skill" "User skill" "User content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (with-temp-buffer
            (insert "# SYSTEM\n\nYou are an agent.\n\n# USER\n\n<skill>user-skill</skill>\n\nHello")
            (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
              (should (member "user-skill" (plist-get parsed :turn-skills)))
              (should (null (plist-get parsed :session-skills)))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-parse-user-skill-only-last-section ()
  "Test that only skills from the LAST user section are active."
  (with-temp-buffer
    (insert "# SYSTEM\n\nYou are an agent.\n\n")
    (insert "# USER\n\n<skill>first-skill</skill>\n\nFirst message\n\n")
    (insert "# ASSISTANT\n\nResponse\n\n")
    (insert "# USER\n\n<skill>second-skill</skill>\n\nSecond message")
    (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
      ;; Should only have second-skill from last user section
      (should (member "second-skill" (plist-get parsed :turn-skills)))
      ;; Should NOT have first-skill from first user section
      (should-not (member "first-skill" (plist-get parsed :turn-skills))))))

(ert-deftest greger-skill-test-parse-both-system-and-user-skills ()
  "Test that system and user skills are collected correctly."
  (with-temp-buffer
    (insert "# SYSTEM\n\n<skill>session-skill</skill>\n\nYou are an agent.\n\n")
    (insert "# USER\n\n<skill>turn-skill</skill>\n\nHello")
    (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
      (should (member "session-skill" (plist-get parsed :session-skills)))
      (should (member "turn-skill" (plist-get parsed :turn-skills))))))

(ert-deftest greger-skill-test-parse-multiple-skills-same-section ()
  "Test that multiple skills in the same section all apply."
  (with-temp-buffer
    (insert "# SYSTEM\n\n<skill>skill-one</skill>\n<skill>skill-two</skill>\n\n# USER\n\nHello")
    (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
      (should (member "skill-one" (plist-get parsed :session-skills)))
      (should (member "skill-two" (plist-get parsed :session-skills))))))

(ert-deftest greger-skill-test-parse-no-skills ()
  "Test that buffers without skills work correctly."
  (with-temp-buffer
    (insert "# SYSTEM\n\nYou are an agent.\n\n# USER\n\nHello")
    (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
      (should (null (plist-get parsed :session-skills)))
      (should (null (plist-get parsed :turn-skills))))))

(ert-deftest greger-skill-test-parse-file-path-skill ()
  "Test that file paths work as skill references."
  (let ((skill-file (make-temp-file "test-skill" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file skill-file
            (insert "# Direct File Skill\n\nThis is loaded from a file path."))
          (with-temp-buffer
            (insert (format "# SYSTEM\n\n<skill>%s</skill>\n\n# USER\n\nHello" skill-file))
            (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
              (should (member skill-file (plist-get parsed :session-skills))))))
      (delete-file skill-file))))

(ert-deftest greger-skill-test-load-from-file-path ()
  "Test loading a skill directly from a file path."
  (let ((skill-file (make-temp-file "test-skill" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file skill-file
            (insert "# Direct File Skill\n\nLoad me directly."))
          (let ((content (greger-skill-load-from-ref skill-file)))
            (should (string-match-p "Load me directly" content))))
      (delete-file skill-file))))

(ert-deftest greger-skill-test-get-buffer-skills-content ()
  "Test getting combined skill content for a buffer."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "content-test" "Content test" "Skill content here")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>content-test</skill>\n\n# USER\n\nHello")
            (let ((content (greger-skill-get-buffer-skills-content (current-buffer))))
              (should (stringp content))
              (should (string-match-p "Skill content here" content))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-get-buffer-skills-content-empty ()
  "Test that buffers without skills return nil."
  (with-temp-buffer
    (insert "# SYSTEM\n\nYou are an agent.\n\n# USER\n\nHello")
    (should (null (greger-skill-get-buffer-skills-content (current-buffer))))))

;; skill-disable tests

(ert-deftest greger-skill-test-parse-skill-disable ()
  "Test that skill-disable tags are parsed from USER section."
  (with-temp-buffer
    (insert "# SYSTEM\n\n<skill>session-skill</skill>\n\n# USER\n\n<skill-disable>session-skill</skill-disable>\n\nHello")
    (let* ((parsed (greger-skill-parse-buffer (current-buffer))))
      (should (member "session-skill" (plist-get parsed :session-skills)))
      (should (member "session-skill" (plist-get parsed :turn-disabled))))))

(ert-deftest greger-skill-test-skill-disable-filters-content ()
  "Test that disabled skills are not included in content."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "to-disable" "Will be disabled" "DISABLED_CONTENT_XYZ")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          ;; Without disable - should include content
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>to-disable</skill>\n\n# USER\n\nHello")
            (let ((content (greger-skill-get-buffer-skills-content (current-buffer))))
              (should (string-match-p "DISABLED_CONTENT_XYZ" content))))
          ;; With disable - should NOT include content
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>to-disable</skill>\n\n# USER\n\n<skill-disable>to-disable</skill-disable>\n\nHello")
            (let ((content (greger-skill-get-buffer-skills-content (current-buffer))))
              (should (null content))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-disable-only-affects-specified ()
  "Test that skill-disable only disables the specified skill."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "keep-me" "Keep this" "KEEP_CONTENT")
        (greger-plugin-test--create-skill "disable-me" "Disable this" "DISABLE_CONTENT")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>keep-me</skill>\n<skill>disable-me</skill>\n\n")
            (insert "# USER\n\n<skill-disable>disable-me</skill-disable>\n\nHello")
            (let ((content (greger-skill-get-buffer-skills-content (current-buffer))))
              (should (string-match-p "KEEP_CONTENT" content))
              (should-not (string-match-p "DISABLE_CONTENT" content))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-disable-multiple ()
  "Test that multiple skills can be disabled."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "skill-a" "Skill A" "CONTENT_A")
        (greger-plugin-test--create-skill "skill-b" "Skill B" "CONTENT_B")
        (greger-plugin-test--create-skill "skill-c" "Skill C" "CONTENT_C")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>skill-a</skill>\n<skill>skill-b</skill>\n<skill>skill-c</skill>\n\n")
            (insert "# USER\n\n<skill-disable>skill-a</skill-disable>\n<skill-disable>skill-c</skill-disable>\n\nHello")
            (let ((content (greger-skill-get-buffer-skills-content (current-buffer))))
              (should-not (string-match-p "CONTENT_A" content))
              (should (string-match-p "CONTENT_B" content))
              (should-not (string-match-p "CONTENT_C" content))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

(provide 'greger-skill-test)

;;; greger-plugin-test.el ends here
