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

(defun greger-skill--list-available ()
  "List all available skills."
  (greger-skill-list-with-descriptions))

;; Register the skill tool
(greger-register-tool "skill"
  :description "Load a skill to get specialized instructions for a task. Skills provide domain-specific knowledge and workflows. Use skill-list to see available skills first."
  :properties '((name . ((type . "string")
                         (description . "Name of the skill to load"))))
  :required '("name")
  :function #'greger-skill--load)

(greger-register-tool "skill-list"
  :description "List all available skills with their descriptions. Use this to discover what skills are available before loading one."
  :properties '()
  :required '()
  :function #'greger-skill--list-available)

;; Buffer parsing for <skill> and <skill-disable> tags

(defun greger-skill-parse-buffer (buffer)
  "Parse BUFFER for <skill> and <skill-disable> tags.
Returns a plist with:
  :session-skills - Skills from SYSTEM (apply to whole session)
  :turn-skills - Skills from last USER section (apply only to that turn)
  :turn-disabled - Skills disabled in last USER section via <skill-disable>"
  (with-current-buffer buffer
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser))
           (session-skills '())
           (turn-skills '())
           (turn-disabled '()))

      ;; Walk all nodes to find system and user sections
      (dolist (child (treesit-node-children root-node))
        (let ((node-type (treesit-node-type child)))
          (cond
           ;; System section: skills apply to whole session
           ((string= node-type "system")
            (let ((skills (greger-skill--extract-from-node child)))
              (setq session-skills (append session-skills skills))))

           ;; User section: only keep skills/disables from the LAST user section
           ((string= node-type "user")
            (setq turn-skills (greger-skill--extract-from-node child))
            (setq turn-disabled (greger-skill--extract-disabled-from-node child))))))

      (list :session-skills (delete-dups session-skills)
            :turn-skills (delete-dups turn-skills)
            :turn-disabled (delete-dups turn-disabled)))))

(defun greger-skill--extract-from-node (node)
  "Extract skill paths from <skill>path</skill> tags in NODE.
Paths can be:
- A skill name (looked up in registry)
- A file path to a SKILL.md or markdown file
Uses regex to parse the text content of the node."
  (let ((text (treesit-node-text node t))
        (skills '()))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "<skill>\\([^<]+\\)</skill>" nil t)
        (let ((skill-ref (string-trim (match-string 1))))
          (push skill-ref skills))))
    (nreverse skills)))

(defun greger-skill--extract-disabled-from-node (node)
  "Extract disabled skill names from <skill-disable>name</skill-disable> tags in NODE."
  (let ((text (treesit-node-text node t))
        (disabled '()))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "<skill-disable>\\([^<]+\\)</skill-disable>" nil t)
        (let ((skill-ref (string-trim (match-string 1))))
          (push skill-ref disabled))))
    (nreverse disabled)))

(defun greger-skill-load-from-ref (skill-ref)
  "Load a skill from SKILL-REF.
SKILL-REF can be:
- A skill name (looked up in registry)
- A file path to a SKILL.md or markdown file"
  (cond
   ;; Skill name in registry
   ((greger-skill-exists-p skill-ref)
    (greger-skill--load skill-ref))
   ;; File path
   ((and (file-exists-p skill-ref)
         (file-regular-p skill-ref))
    (with-temp-buffer
      (insert-file-contents skill-ref)
      (buffer-string)))
   ;; Not found
   (t nil)))

(defun greger-skill-get-buffer-skills-content (buffer)
  "Get combined skill content for BUFFER.
Returns a string with all skill content to inject, or nil if no skills."
  (let* ((parsed (greger-skill-parse-buffer buffer))
         (session-skills (plist-get parsed :session-skills))
         (turn-skills (plist-get parsed :turn-skills))
         (all-skills (delete-dups (append session-skills turn-skills)))
         (contents '()))
    (dolist (skill-ref all-skills)
      (when-let* ((content (greger-skill-load-from-ref skill-ref)))
        (push content contents)))
    (when contents
      (string-join (nreverse contents) "\n\n---\n\n"))))

(provide 'greger-plugin)

;;; greger-plugin.el ends here
