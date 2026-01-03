;;; greger-plugin.el --- Skills system for greger -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides a skills system for Greger, inspired by Claude Code's skills.
;; Skills are markdown documents that teach the agent how to perform specific tasks.
;; Skills can be declared per-session using <skill>path</skill> in the buffer.
;;
;; In # SYSTEM: skills apply to the whole session
;; In # USER: skills apply only to that turn (last USER section only)

;;; Code:

(require 'treesit)
(require 'greger-tools)

(defcustom greger-skill-directories
  (list (expand-file-name "~/.config/greger/skills")
        ".greger/skills")
  "Directories to search for skill definitions.
Each directory is searched for subdirectories containing SKILL.md files.
Later directories take precedence when skill names conflict."
  :type '(repeat directory)
  :group 'greger)

(defvar greger-skill-registry (make-hash-table :test 'equal)
  "Registry mapping skill names to their definitions.")

(cl-defstruct greger-skill
  "Structure representing a Greger skill."
  name
  description
  content
  source-file)

;; Skill discovery

(defun greger-skill-discover ()
  "Discover and register skills from `greger-skill-directories'."
  (clrhash greger-skill-registry)
  (dolist (dir (reverse greger-skill-directories))
    (let ((expanded-dir (expand-file-name dir)))
      (when (file-directory-p expanded-dir)
        (dolist (skill-dir (directory-files expanded-dir t "^[^.]"))
          (when (file-directory-p skill-dir)
            (let ((skill-file (expand-file-name "SKILL.md" skill-dir)))
              (when (file-exists-p skill-file)
                (greger-skill--register-from-file skill-file)))))))))

(defun greger-skill--register-from-file (file)
  "Register a skill from FILE (SKILL.md format)."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((frontmatter (greger-skill--parse-frontmatter))
           (content (buffer-substring-no-properties (point) (point-max)))
           (name (or (cdr (assoc "name" frontmatter))
                     (file-name-nondirectory (directory-file-name 
                                              (file-name-directory file)))))
           (description (or (cdr (assoc "description" frontmatter)) ""))
           (skill (make-greger-skill
                   :name name
                   :description description
                   :content content
                   :source-file file)))
      (puthash name skill greger-skill-registry))))

(defun greger-skill--parse-frontmatter ()
  "Parse YAML frontmatter from current buffer.
Returns alist of key-value pairs.  Moves point past frontmatter."
  (goto-char (point-min))
  (when (looking-at "---\n")
    (forward-line 1)
    (let ((start (point))
          (result '()))
      (when (re-search-forward "^---$" nil t)
        (let ((yaml-text (buffer-substring-no-properties start (match-beginning 0))))
          (dolist (line (split-string yaml-text "\n" t))
            (when (string-match "^\\([^:]+\\):\\s-*\\(.*\\)$" line)
              (push (cons (string-trim (match-string 1 line))
                          (string-trim (match-string 2 line)))
                    result))))
        (forward-line 1))
      result)))

(defun greger-skill-get (name)
  "Get skill by NAME from registry."
  (gethash name greger-skill-registry))

(defun greger-skill-exists-p (name)
  "Return non-nil if skill NAME exists."
  (not (null (gethash name greger-skill-registry))))

(defun greger-skill-list ()
  "Return list of all registered skill names."
  (let ((names '()))
    (maphash (lambda (name _) (push name names)) greger-skill-registry)
    (sort names #'string<)))

(defun greger-skill-list-with-descriptions ()
  "Return formatted string of all skills with descriptions."
  (let ((skills '()))
    (maphash (lambda (name skill)
               (push (format "- %s: %s" name (greger-skill-description skill))
                     skills))
             greger-skill-registry)
    (if skills
        (string-join (sort skills #'string<) "\n")
      "No skills available.")))

;; Skill tool - allows agent to load skills dynamically

(defun greger-skill--load (name)
  "Load skill NAME and return its content for injection into context."
  (if-let* ((skill (greger-skill-get name)))
      (format "# Skill: %s\n\n%s"
              (greger-skill-name skill)
              (greger-skill-content skill))
    (format "Skill '%s' not found. Available skills:\n%s"
            name
            (greger-skill-list-with-descriptions))))

(defun greger-skill--build-description ()
  "Build the skill tool description with available skills list."
  (let ((base-description "Load a skill to get specialized instructions for a task. Skills provide domain-specific knowledge and workflows.

<skills_instructions>
When users ask you to perform tasks, check if any of the available skills below can help complete the task more effectively. Skills provide specialized capabilities and domain knowledge. Invoke a skill by calling this tool with the skill name.
</skills_instructions>

<available_skills>
")
        (skills-list (let ((skills '()))
                       (maphash (lambda (name skill)
                                  (push (format "<skill>\n<name>%s</name>\n<description>%s</description>\n</skill>"
                                                name
                                                (greger-skill-description skill))
                                        skills))
                                greger-skill-registry)
                       (if skills
                           (string-join (sort skills #'string<) "\n")
                         "<no_skills_available/>"))))
    (concat base-description skills-list "\n</available_skills>")))

(defun greger-skill--get-tool-schema ()
  "Get the skill tool schema with dynamic description."
  `((name . "skill")
    (description . ,(greger-skill--build-description))
    (input_schema . ((type . "object")
                     (properties . ((name . ((type . "string")
                                             (description . "Name of the skill to load")))))
                     (required . ("name"))))))

;; Register the skill tool with dynamic schema
(greger-register-tool "skill"
  :description "Load a skill to get specialized instructions for a task."
  :properties '((name . ((type . "string")
                         (description . "Name of the skill to load"))))
  :required '("name")
  :function #'greger-skill--load
  :schema-fn #'greger-skill--get-tool-schema)

(provide 'greger-skill)

;;; greger-skill.el ends here
