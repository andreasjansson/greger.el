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
  (should (gethash "skill" greger-tools-registry)))

(ert-deftest greger-skill-test-skill-tools-in-greger-tools ()
  "Test that skill tool is in greger-tools (offered to model)."
  (should (member "skill" greger-tools)))

(ert-deftest greger-skill-test-skill-tool-dynamic-schema ()
  "Test that the skill tool has a dynamic schema with available skills."
  (unwind-protect
      (let ((temp-dir (greger-plugin-test--setup-temp-dir)))
        (greger-plugin-test--create-skill "schema-test" "Schema test skill" "Content")
        (let ((greger-skill-directories (list temp-dir)))
          (greger-skill-discover)
          (let ((schema (greger-skill--get-tool-schema)))
            (should (equal "skill" (alist-get 'name schema)))
            (let ((description (alist-get 'description schema)))
              (should (string-match-p "<available_skills>" description))
              (should (string-match-p "schema-test" description))
              (should (string-match-p "Schema test skill" description))))))
    (greger-plugin-test--cleanup-temp-dir)
    (greger-plugin-test--cleanup-registry)))

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

(provide 'greger-skill-test)

;;; greger-skill-test.el ends here
