;;; greger-skill-test.el --- Tests for greger skills system -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-skill)

;; Test helper functions

(defvar greger-skill-test--temp-dir nil
  "Temporary directory for test skills.")

(defun greger-skill-test--setup-temp-dir ()
  "Create temporary directory for test skills."
  (setq greger-skill-test--temp-dir (make-temp-file "greger-skills-test" t))
  greger-skill-test--temp-dir)

(defun greger-skill-test--cleanup-temp-dir ()
  "Clean up temporary directory."
  (when (and greger-skill-test--temp-dir
             (file-directory-p greger-skill-test--temp-dir))
    (delete-directory greger-skill-test--temp-dir t)
    (setq greger-skill-test--temp-dir nil)))

(defun greger-skill-test--create-skill (name description content)
  "Create a test skill with NAME, DESCRIPTION, and CONTENT."
  (let ((skill-dir (expand-file-name name greger-skill-test--temp-dir)))
    (make-directory skill-dir t)
    (with-temp-file (expand-file-name "SKILL.md" skill-dir)
      (insert "---\n")
      (insert (format "name: %s\n" name))
      (insert (format "description: %s\n" description))
      (insert "---\n\n")
      (insert content))
    skill-dir))

(defun greger-skill-test--cleanup-registry ()
  "Clean up the skill registry."
  (clrhash greger-skill-registry))

;; Skill discovery tests

(ert-deftest greger-skill-test-discover-skills ()
  "Test that skills are discovered from directories."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "test-skill" "A test skill" "# Test\n\nDo the thing.")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (should (greger-skill-exists-p "test-skill"))
          (let ((skill (greger-skill-get "test-skill")))
            (should (equal "test-skill" (greger-skill-name skill)))
            (should (equal "A test skill" (greger-skill-description skill)))
            (should (string-match-p "Do the thing" (greger-skill-content skill))))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(ert-deftest greger-skill-test-discover-multiple-skills ()
  "Test that multiple skills are discovered."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "skill-one" "First skill" "Content one")
        (greger-skill-test--create-skill "skill-two" "Second skill" "Content two")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (should (greger-skill-exists-p "skill-one"))
          (should (greger-skill-exists-p "skill-two"))
          (should (= 2 (length (greger-skill-list))))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-list ()
  "Test greger-skill-list returns all skills."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "alpha" "Alpha skill" "Alpha content")
        (greger-skill-test--create-skill "beta" "Beta skill" "Beta content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((skills (greger-skill-list)))
            (should (member "alpha" skills))
            (should (member "beta" skills)))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-list-with-descriptions ()
  "Test greger-skill-list-with-descriptions formats correctly."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "my-skill" "Does something useful" "Content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((listing (greger-skill-list-with-descriptions)))
            (should (string-match-p "my-skill" listing))
            (should (string-match-p "Does something useful" listing)))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

;; Skill loading tests

(ert-deftest greger-skill-test-load-skill ()
  "Test loading a skill by name."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "loader-test" "Test loading" "# Instructions\n\nDo this.")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((content (greger-skill--load "loader-test")))
            (should (string-match-p "Skill: loader-test" content))
            (should (string-match-p "Do this" content)))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(ert-deftest greger-skill-test-load-nonexistent-skill ()
  "Test loading a skill that doesn't exist throws an error."
  (greger-skill-test--cleanup-registry)
  (should-error (greger-skill--load "nonexistent") :type 'error))

;; Skill tool registration tests

(ert-deftest greger-skill-test-skill-tool-registered ()
  "Test that the skill tool is registered."
  (should (gethash "skill" greger-tools-registry)))

(ert-deftest greger-skill-test-skill-tools-in-greger-tools ()
  "Test that skill tool is in greger-tools (offered to model)."
  (should (member "skill" greger-tools)))

(ert-deftest greger-skill-test-skill-tool-dynamic-schema ()
  "Test that the skill tool has a dynamic schema with available skills."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "schema-test" "Schema test skill" "Content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((schema (greger-skill--get-tool-schema)))
            (should (equal "skill" (alist-get 'name schema)))
            (let ((description (alist-get 'description schema)))
              (should (string-match-p "<available_skills>" description))
              (should (string-match-p "schema-test" description))
              (should (string-match-p "Schema test skill" description))))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(ert-deftest greger-skill-test-skill-tool-execution ()
  "Test executing the skill tool."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "exec-test" "Execution test" "Execute instructions")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((result nil) (error nil))
            (greger-tools-execute :tool-name "skill"
                                  :args '((name . "exec-test"))
                                  :callback (lambda (r e) (setq result r error e)))
            (should (null error))
            (should (string-match-p "Execute instructions" result)))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

;; Buffer parsing tests - <skill> tags add to registry

(ert-deftest greger-skill-test-register-from-buffer-adds-to-registry ()
  "Test that <skill> tags in buffer add skills to registry."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "buffer-skill" "Buffer skill" "Buffer content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          ;; Clear registry to test that register-from-buffer adds it back
          (greger-skill-test--cleanup-registry)
          (should-not (greger-skill-exists-p "buffer-skill"))
          ;; Now register from buffer
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>buffer-skill</skill>\n\n# USER\n\nHello")
            ;; Need to re-discover since we cleared registry
            (let ((greger-skill-directories (list temp-dir)))
              (greger-skill-discover)
              (greger-skill-register-from-buffer (current-buffer))
              (should (greger-skill-exists-p "buffer-skill"))))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(ert-deftest greger-skill-test-register-from-buffer-file-path ()
  "Test that <skill> tags with file paths add skills to registry."
  (let ((skill-file (make-temp-file "test-skill" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file skill-file
            (insert "---\n")
            (insert "name: file-path-skill\n")
            (insert "description: A skill from file path\n")
            (insert "---\n\n")
            (insert "# File Path Skill\n\nDo file path things."))
          ;; Clear registry
          (greger-skill-test--cleanup-registry)
          (should-not (greger-skill-exists-p "file-path-skill"))
          ;; Register from buffer with file path
          (with-temp-buffer
            (insert (format "# SYSTEM\n\n<skill>%s</skill>\n\n# USER\n\nHello" skill-file))
            (greger-skill-register-from-buffer (current-buffer))
            (should (greger-skill-exists-p "file-path-skill"))))
      (delete-file skill-file)
      (greger-skill-test--cleanup-registry))))

(ert-deftest greger-skill-test-register-from-buffer-multiple-skills ()
  "Test that multiple <skill> tags all add to registry."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "skill-a" "Skill A" "Content A")
        (greger-skill-test--create-skill "skill-b" "Skill B" "Content B")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (with-temp-buffer
            (insert "# SYSTEM\n\n<skill>skill-a</skill>\n<skill>skill-b</skill>\n\n# USER\n\nHello")
            (greger-skill-register-from-buffer (current-buffer))
            (should (greger-skill-exists-p "skill-a"))
            (should (greger-skill-exists-p "skill-b")))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(ert-deftest greger-skill-test-parse-skill-tags ()
  "Test that skill tags are parsed from buffer."
  (with-temp-buffer
    (insert "# SYSTEM\n\n<skill>skill-one</skill>\n\n# USER\n\n<skill>skill-two</skill>\n\nHello")
    (let ((refs (greger-skill--parse-skill-tags (current-buffer))))
      (should (member "skill-one" refs))
      (should (member "skill-two" refs)))))

(ert-deftest greger-skill-test-parse-skill-tags-empty ()
  "Test that buffers without skill tags return empty list."
  (with-temp-buffer
    (insert "# SYSTEM\n\nYou are an agent.\n\n# USER\n\nHello")
    (let ((refs (greger-skill--parse-skill-tags (current-buffer))))
      (should (null refs)))))

(ert-deftest greger-skill-test-dynamic-schema-updates-after-register ()
  "Test that skill tool schema updates after registering skills from buffer."
  (unwind-protect
      (let ((temp-dir (greger-skill-test--setup-temp-dir)))
        (greger-skill-test--create-skill "dynamic-skill" "Dynamic skill" "Dynamic content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          ;; Check that skill appears in schema
          (let* ((schema (greger-skill--get-tool-schema))
                 (description (alist-get 'description schema)))
            (should (string-match-p "dynamic-skill" description))
            (should (string-match-p "Dynamic skill" description)))))
    (greger-skill-test--cleanup-temp-dir)
    (greger-skill-test--cleanup-registry)))

(provide 'greger-skill-test)

;;; greger-skill-test.el ends here
