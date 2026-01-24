;;; greger-skill.el --- Skills system for greger -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides a skills system for Greger, inspired by Claude Code's skills.
;; Skills are markdown documents that teach the agent how to perform specific tasks.
;;
;; Skills can be added to the registry in two ways:
;; 1. Discovered from directories in `greger-skill-directories'
;; 2. Declared in buffer using <skill>name-or-path</skill> tags
;;
;; Once in the registry, skills appear in the `skill` tool's description.
;; The model can then call the skill tool to load the skill content.

;;; Code:

(require 'treesit)
(require 'greger-tools)

(defvar greger-skill-registry (make-hash-table :test 'equal)
  "Registry mapping skill names to their definitions.")

(cl-defstruct greger-skill
  "Structure representing a Greger skill."
  name
  description
  content
  source-file)

;; Skill discovery from directories

(defun greger-skill--find-project-claude-dirs ()
  "Find all .claude directories from current directory up to git root."
  (let ((dirs '())
        (current default-directory)
        (root (locate-dominating-file default-directory ".git")))
    (while (and current
                (not (string= current "/"))
                (or (null root)
                    (string-prefix-p root current)))
      (let ((claude-dir (expand-file-name ".claude" current)))
        (when (file-directory-p claude-dir)
          (push claude-dir dirs)))
      (setq current (file-name-directory (directory-file-name current))))
    (nreverse dirs)))

(defun greger-skill--scan-skills-in-dir (dir)
  "Scan DIR/skills/ for SKILL.md files recursively."
  (let ((skills-dir (expand-file-name "skills" dir)))
    (when (file-directory-p skills-dir)
      (directory-files-recursively skills-dir "^SKILL\\.md$" nil nil t))))

(defun greger-skill-discover ()
  "Discover and register skills from Claude-compatible directories.
Scans:
- ~/.claude/skills/**/SKILL.md (global)
- ~/.config/opencode/skill/**/SKILL.md (opencode)
- .claude/skills/**/SKILL.md (project, walking up to git root)"
  (clrhash greger-skill-registry)
  (let ((skill-files '()))
    ;; Global ~/.claude/skills/
    (let ((global-claude (expand-file-name "~/.claude")))
      (when (file-directory-p global-claude)
        (setq skill-files (append skill-files
                                  (greger-skill--scan-skills-in-dir global-claude)))))
    ;; Opencode skills (scans directly, no /skills/ subdirectory)
    (let ((opencode-skills (expand-file-name "~/.config/opencode/skill")))
      (when (file-directory-p opencode-skills)
        (setq skill-files (append skill-files
                                  (directory-files-recursively opencode-skills "^SKILL\\.md$" nil nil t)))))
    ;; Project .claude/skills/ directories (walking up)
    (dolist (claude-dir (greger-skill--find-project-claude-dirs))
      (setq skill-files (append skill-files
                                (greger-skill--scan-skills-in-dir claude-dir))))
    ;; Register all found skills (later ones override earlier for same name)
    (dolist (skill-file skill-files)
      (greger-skill--register-from-file skill-file))))

(defun greger-skill--register-from-file (file &optional strict)
  "Register a skill from FILE (SKILL.md format).
If STRICT is non-nil, throw an error if frontmatter is missing or invalid.
When called from discovery, STRICT is nil (skip invalid files silently).
When called from <skill> tags, STRICT is t (report errors to user)."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((frontmatter (greger-skill--parse-frontmatter))
           (content (buffer-substring-no-properties (point) (point-max)))
           (name (cdr (assoc "name" frontmatter)))
           (description (cdr (assoc "description" frontmatter))))
      (when strict
        (unless frontmatter
          (error "Skill file '%s' is missing YAML frontmatter (---)" file))
        (unless name
          (error "Skill file '%s' is missing required 'name' in frontmatter" file))
        (unless description
          (error "Skill file '%s' is missing required 'description' in frontmatter" file)))
      (when (and name description)
        (let ((skill (make-greger-skill
                      :name name
                      :description description
                      :content content
                      :source-file file)))
          (puthash name skill greger-skill-registry))))))

(defun greger-skill--parse-frontmatter ()
  "Parse YAML frontmatter from current buffer.
Returns alist of key-value pairs.  Moves point past frontmatter.
Handles multi-line values using > (folded) or | (literal) block scalars."
  (goto-char (point-min))
  (when (looking-at "---\n")
    (forward-line 1)
    (let ((start (point))
          (result '()))
      (when (re-search-forward "^---$" nil t)
        (let* ((yaml-text (buffer-substring-no-properties start (match-beginning 0)))
               (lines (split-string yaml-text "\n"))
               (current-key nil)
               (current-value nil)
               (in-multiline nil))
          (dolist (line lines)
            (cond
             ((and in-multiline
                   (string-match "^\\s-+" line))
              (setq current-value
                    (concat current-value
                            (if (eq in-multiline 'folded) " " "\n")
                            (string-trim line))))
             ((string-match "^\\([^:]+\\):\\s-*\\([>|]\\)\\s-*$" line)
              (when current-key
                (push (cons current-key (string-trim current-value)) result))
              (setq current-key (string-trim (match-string 1 line)))
              (setq current-value "")
              (setq in-multiline (if (string= (match-string 2 line) ">") 'folded 'literal)))
             ((string-match "^\\([^:]+\\):\\s-*\\(.*\\)$" line)
              (when current-key
                (push (cons current-key (string-trim current-value)) result))
              (setq current-key (string-trim (match-string 1 line)))
              (setq current-value (string-trim (match-string 2 line)))
              (setq in-multiline nil))))
          (when current-key
            (push (cons current-key (string-trim current-value)) result)))
        (forward-line 1))
      result)))

;; Registry access

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

;; Buffer parsing - adds skills from <skill> tags to registry

(defun greger-skill-register-from-buffer (buffer)
  "Parse BUFFER for <skill> tags and add referenced skills to registry.
Skills can be referenced by:
- Name (if already discovered from directories)
- File path to a SKILL.md file"
  (let ((skill-refs (greger-skill--parse-skill-tags buffer)))
    (dolist (ref skill-refs)
      (greger-skill--register-from-ref ref))))

(defun greger-skill--parse-skill-tags (buffer)
  "Parse BUFFER and return list of skill references from <skill> tags."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((refs '()))
        (while (re-search-forward "<skill>\\([^<]+\\)</skill>" nil t)
          (let ((ref (string-trim (match-string 1))))
            (push ref refs)))
        (delete-dups (nreverse refs))))))

(defun greger-skill--register-from-ref (ref)
  "Register a skill from REF.
REF can be a skill name (already in registry) or a file path.
Throws an error if the skill cannot be found or is invalid."
  (cond
   ;; Already in registry - nothing to do
   ((greger-skill-exists-p ref)
    nil)
   ;; File path to .md file
   ((and (file-exists-p ref)
         (file-regular-p ref)
         (string-suffix-p ".md" ref))
    (greger-skill--register-from-file ref t))
   ;; File path to directory containing SKILL.md
   ((and (file-exists-p ref)
         (file-directory-p ref))
    (let ((skill-file (expand-file-name "SKILL.md" ref)))
      (if (file-exists-p skill-file)
          (greger-skill--register-from-file skill-file t)
        (error "Skill directory '%s' does not contain SKILL.md" ref))))
   ;; File path that doesn't exist
   ((or (string-suffix-p ".md" ref)
        (string-prefix-p "/" ref)
        (string-prefix-p "~" ref)
        (string-prefix-p "./" ref))
    (error "Skill file not found: %s" ref))
   ;; Skill name not in registry
   (t
    (error "Skill '%s' not found. Run greger-skill-discover or check your .claude/skills directory" ref))))

;; Skill tool - allows model to load skills

(defun greger-skill--load (name)
  "Load skill NAME and return its content."
  (if-let* ((skill (greger-skill-get name)))
      (let ((dir (if (greger-skill-source-file skill)
                     (file-name-directory (greger-skill-source-file skill))
                   default-directory)))
        (format "## Skill: %s\n\n**Base directory**: %s\n\n%s"
                (greger-skill-name skill)
                dir
                (string-trim (greger-skill-content skill))))
    (let ((available (greger-skill-list)))
      (error "Skill \"%s\" not found. Available skills: %s"
             name
             (if available (string-join available ", ") "none")))))

(defun greger-skill--build-description ()
  "Build the skill tool description with available skills list."
  (let ((skill-count (hash-table-count greger-skill-registry)))
    (if (= skill-count 0)
        "Load a skill to get detailed instructions for a specific task. No skills are currently available."
      (let ((skills-xml (let ((skills '()))
                          (maphash (lambda (name skill)
                                     (push (format "<skill> <name>%s</name> <description>%s</description> </skill>"
                                                   name
                                                   (greger-skill-description skill))
                                           skills))
                                   greger-skill-registry)
                          (string-join (sort skills #'string<) " "))))
        (concat "Load a skill to get detailed instructions for a specific task. "
                "Skills provide specialized knowledge and step-by-step guidance. "
                "Use this when a task matches an available skill's description. "
                "<available_skills> " skills-xml " </available_skills>")))))

(defun greger-skill--get-tool-schema ()
  "Get the skill tool schema with dynamic description."
  `((name . "skill")
    (description . ,(greger-skill--build-description))
    (input_schema . ((type . "object")
                     (properties . ((name . ((type . "string")
                                             (description . "The skill identifier from available_skills (e.g., 'code-review' or 'category/helper')")))))
                     (required . ("name"))))))

;; Register the skill tool with dynamic schema
(greger-register-tool "skill"
  :description "Load a skill to get detailed instructions for a specific task."
  :properties '((name . ((type . "string")
                         (description . "The skill identifier from available_skills (e.g., 'code-review' or 'category/helper')"))))
  :required '("name")
  :function #'greger-skill--load
  :schema-fn #'greger-skill--get-tool-schema)

(provide 'greger-skill)

;;; greger-skill.el ends here
