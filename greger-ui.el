;; greger-ui.el --- User interface for greger-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides user interface functionality for greger-mode.

;;; Code:

(require 'treesit)

;; Faces
(defface greger-eval-result-face
  '((t (:background "#f0f8ff" :foreground "#003366")))
  "Face for eval result content."
  :group 'greger)

(defface greger-eval-arrow-face
  '((t (:foreground "#0066cc" :weight bold)))
  "Face for eval result arrow."
  :group 'greger)

;; Variables
(defvar greger-ui-folding-mode nil
  "Non-nil if greger UI folding mode is enabled.")

;; Keymaps
(defvar greger-ui-eval-result-content-head-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'greger-ui-toggle-eval-result-content)
    map)
  "Keymap for eval result content head.")

(defvar greger-ui-eval-result-content-tail-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'greger-ui-toggle-eval-result-content)
    map)
  "Keymap for eval result content tail.")

;; Functions
(defun greger-ui-toggle-folding-mode ()
  "Toggle greger UI folding mode on or off."
  (interactive)
  (setq greger-ui-folding-mode (not greger-ui-folding-mode))
  
  ;; Always clean up overlays when toggling
  (greger-ui--cleanup-fold-overlays)
  (greger-ui--cleanup-eval-fold-overlays)
  
  (font-lock-flush (point-min) (point-max))
  (message "Greger UI folding mode: %s" (if greger-ui-folding-mode "enabled" "disabled")))

(defun greger-ui--cleanup-eval-fold-overlays ()
  "Clean up all eval fold text properties in the current buffer."
  ;; Remove display properties that were added for eval arrows and expansion messages
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((display-prop (get-text-property (point) 'display)))
        (when (and display-prop
                   (or (string-match-p "⇒" display-prop)
                       (string-match-p "\\[\\+.*lines.*TAB.*expand\\]" display-prop)))
          (remove-text-properties (point) (1+ (point)) '(display nil))))
      (forward-char 1))))

(defun greger-ui--cleanup-fold-overlays ()
  "Remove all fold overlays in the current buffer."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'greger-ui-fold-overlay)
      (delete-overlay overlay))))

(defun greger-ui--citation-hiding (node _override _start _end)
  "Font-lock function to hide citation entries within assistant blocks.
NODE is the matched tree-sitter node.
_OVERRIDE, _START, and _END are ignored font-lock parameters."
  (when greger-ui-folding-mode
    (let ((node-start (treesit-node-start node))
          (node-end (treesit-node-end node)))
      ;; Make citations invisible in folding mode
      (put-text-property node-start node-end 'invisible t))))

(defun greger-ui--tool-content-head-folding (node _override _start _end)
  "Font-lock function to make tool_content_head TAB-able for tail visibility.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (parent (treesit-node-parent node))
         ;; Check if the tool result is still generating
         (tool-result-node (treesit-parent-until node (lambda (n) (string= (treesit-node-type n) "tool_result"))))
         (is-generating (when tool-result-node
                          (get-text-property (treesit-node-start tool-result-node) 'greger-tool-result-generating))))

    (when (and parent (not is-generating))
      ;; Find the corresponding tail safely
      (let ((tail-node (treesit-search-subtree parent "^tool_content_tail$" nil nil 1)))
        (when tail-node
          (let* ((tail-start (treesit-node-start tail-node))
                 (tail-end (treesit-node-end tail-node))
                 (is-tail-visible (or (get-text-property tail-start 'greger-ui-tool-content-expanded)
                                      (not greger-ui-folding-mode)))
                 (line-count (max 1 (count-lines tail-start tail-end))))
            ;; Mark the head as foldable and store tail info
            (put-text-property node-start node-end 'greger-ui-foldable-tool-content t)
            (put-text-property node-start node-end 'greger-ui-tool-tail-start tail-start)
            (put-text-property node-start node-end 'greger-ui-tool-tail-end tail-end)
            (put-text-property node-start node-end 'keymap greger-ui-tool-content-head-keymap)
            (put-text-property node-start node-end 'font-lock-face 'greger-tool-content-face)
            
            ;; Clean up any existing overlays in this region first
            (dolist (overlay (overlays-in (- node-end 2) node-end))
              (when (overlay-get overlay 'greger-ui-fold-overlay)
                (delete-overlay overlay)))
            
            ;; Add expansion indicator when not visible
            (unless is-tail-visible
              (let ((overlay (make-overlay (-  node-end 2) (1- node-end))))
                (overlay-put overlay 'after-string
                             (propertize (format "\n[+%d lines, TAB to expand]" line-count)
                                         'face '(:foreground "gray" :height 0.8 :slant italic)))
                (overlay-put overlay 'greger-ui-fold-overlay t)
                (overlay-put overlay 'evaporate t)))))))))

(defun greger-ui--tool-content-tail-folding (node _override _start _end)
  "Font-lock function to make tool_content_tail invisible by default.
NODE is the matched tree-sitter node"
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (is-visible (get-text-property node-start 'greger-ui-tool-content-expanded))
         ;; Check if the tool result is still generating
         (tool-result-node (treesit-parent-until node (lambda (n) (string= (treesit-node-type n) "tool_result"))))
         (is-generating (when tool-result-node
                          (get-text-property (treesit-node-start tool-result-node) 'greger-tool-result-generating))))

    ;; Don't apply folding if the tool result is still generating
    (unless is-generating
      ;; Apply invisibility (default is invisible unless expanded, but respect global folding mode)
      (put-text-property node-start node-end 'invisible
                         (and greger-ui-folding-mode (not is-visible)))
      (put-text-property node-start node-end 'keymap greger-ui-tool-content-tail-keymap))))

(defun greger-ui--thinking-signature-hiding (node _override _start _end)
  "Hide thinking signature.  NODE is the matched tree-sitter node."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (invisible-end (+ node-end 2)))
    (put-text-property node-start (min invisible-end (point-max)) 'invisible greger-ui-folding-mode)))

(defun greger-ui--eval-result-content-head-folding (node _override _start _end)
  "Font-lock function to make eval_result_content_head TAB-able for tail visibility.
NODE is the matched tree-sitter node, similar to tool content head folding."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (parent (treesit-node-parent node)))

    (when parent
      ;; Find the corresponding tail safely
      (let ((tail-node (treesit-search-subtree parent "^eval_result_content_tail$" nil nil 1)))
        (when tail-node
          (let* ((tail-start (treesit-node-start tail-node))
                 (tail-end (treesit-node-end tail-node))
                 (is-tail-visible (or (get-text-property tail-start 'greger-ui-eval-result-content-expanded)
                                      (not greger-ui-folding-mode)))
                 (line-count (max 1 (count-lines tail-start tail-end))))
            ;; Mark the head as foldable and store tail info
            (put-text-property node-start node-end 'greger-ui-foldable-eval-result-content t)
            (put-text-property node-start node-end 'greger-ui-eval-result-tail-start tail-start)
            (put-text-property node-start node-end 'greger-ui-eval-result-tail-end tail-end)
            (put-text-property node-start node-end 'keymap greger-ui-eval-result-content-head-keymap)
            (put-text-property node-start node-end 'font-lock-face 'greger-eval-result-face)
            
            ;; Clean up any existing overlays in this region first
            (dolist (overlay (overlays-in (- node-end 2) node-end))
              (when (overlay-get overlay 'greger-ui-fold-overlay)
                (delete-overlay overlay)))
            
            ;; Add expansion indicator when not visible
            (unless is-tail-visible
              (let ((overlay (make-overlay (- node-end 2) (1- node-end))))
                (overlay-put overlay 'after-string
                             (propertize (format "\n[+%d lines, TAB to expand]" line-count)
                                         'face '(:foreground "gray" :height 0.8 :slant italic)))
                (overlay-put overlay 'greger-ui-fold-overlay t)
                (overlay-put overlay 'evaporate t)))))))))

(defun greger-ui--eval-result-content-tail-folding (node _override _start _end)
  "Font-lock function to make eval_result_content_tail invisible by default.
NODE is the matched tree-sitter node, similar to tool content tail folding."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (is-visible (get-text-property node-start 'greger-ui-eval-result-content-expanded)))

    ;; Apply invisibility (default is invisible unless expanded, but respect global folding mode)
    (put-text-property node-start node-end 'invisible
                       (and greger-ui-folding-mode (not is-visible)))
    (put-text-property node-start node-end 'keymap greger-ui-eval-result-content-tail-keymap)
    (put-text-property node-start node-end 'font-lock-face 'greger-eval-result-face)))

(defun greger-ui--make-tool-tag-invisible (node _override _start _end)
  "Make tool tag NODE invisible while preserving face styling."
  (condition-case nil
      (let ((node-start (treesit-node-start node))
            (node-end (min (1+ (treesit-node-end node)) (point-max))))
        ;; In Emacs 29.4, be more careful about invisible property boundaries
        ;; to prevent accidentally marking too much content as invisible
        (when (<= node-end (point-max))
          (put-text-property node-start node-end 'invisible greger-ui-folding-mode)
          (unless greger-ui-folding-mode
            (put-text-property node-start node-end 'font-lock-face '(:height 0.8 :foreground "gray70")))))
    (treesit-node-outdated
     ;; Node became outdated, skip this operation
     nil)))

(defun greger-ui--make-tool-result-id-invisible (node _override _start _end)
  "Make id NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (put-text-property node-start node-end 'invisible greger-ui-folding-mode)))

(defun greger-ui--make-tool-use-id-invisible (node _override _start _end)
  "Make id NODE invisible while preserving face styling."
  (let ((node-start (treesit-node-start node))
        (node-end (1- (treesit-node-end node))))
    (put-text-property node-start node-end 'invisible greger-ui-folding-mode)))

(defun greger-ui--make-eval-result-tag-invisible (node _override _start _end)
  "Handle eval result end tag visibility based on folding mode."
  (condition-case nil
      (let ((node-start (treesit-node-start node))
            (node-end (min (1+ (treesit-node-end node)) (point-max))))

        (when (<= node-end (point-max))
          (if greger-ui-folding-mode
              ;; Hide end tag when folding mode is enabled
              (put-text-property node-start node-end 'invisible t)
            ;; Show tag normally when folding mode is disabled
            (remove-text-properties node-start node-end '(invisible nil)))))
    (treesit-node-outdated
     ;; Node became outdated, skip this operation
     nil)))

;; Links

(defun greger-ui--url-link (node _override _start _end)
  "Apply URL link properties to NODE for interactive behavior."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (url-start (+ node-start 3)))
    (put-text-property url-start node-end 'face 'greger-link-face)
    (put-text-property url-start node-end 'mouse-face 'highlight)
    (put-text-property url-start node-end 'keymap greger-url-keymap)
    (put-text-property url-start node-end 'help-echo "Click to open URL")))

;; Interactive functions

(defun greger-ui-toggle-eval-result-content (&optional pos)
  "Toggle visibility of eval result content tail at POS (or point)."
  (interactive)
  (let ((pos (or pos (point))))
    (when (get-text-property pos 'greger-ui-foldable-eval-result-content)
      (let* ((tail-start (get-text-property pos 'greger-ui-eval-result-tail-start))
             (tail-end (get-text-property pos 'greger-ui-eval-result-tail-end))
             (currently-visible (get-text-property tail-start 'greger-ui-eval-result-content-expanded)))
        
        (when (and tail-start tail-end)
          ;; Toggle the expanded state
          (put-text-property tail-start tail-end 'greger-ui-eval-result-content-expanded (not currently-visible))
          
          ;; Update invisibility
          (put-text-property tail-start tail-end 'invisible 
                             (and greger-ui-folding-mode (not (not currently-visible))))
          
          ;; Force fontification refresh for this region
          (let ((eval-result-node (treesit-parent-until 
                                   (treesit-node-at pos)
                                   (lambda (n) (string= (treesit-node-type n) "eval_result")))))
            (when eval-result-node
              (font-lock-flush (treesit-node-start eval-result-node) 
                               (treesit-node-end eval-result-node)))))))))

(defun greger-ui-toggle-tool-content (&optional pos)
  "Toggle visibility of tool content tail at POS (or point)."
  (interactive)
  (let ((pos (or pos (point))))
    (when (get-text-property pos 'greger-ui-foldable-tool-content)
      (let* ((tail-start (get-text-property pos 'greger-ui-tool-tail-start))
             (tail-end (get-text-property pos 'greger-ui-tool-tail-end))
             (currently-visible (get-text-property tail-start 'greger-ui-tool-content-expanded)))
        
        (when (and tail-start tail-end)
          ;; Toggle the expanded state
          (put-text-property tail-start tail-end 'greger-ui-tool-content-expanded (not currently-visible))
          
          ;; Update invisibility  
          (put-text-property tail-start tail-end 'invisible 
                             (and greger-ui-folding-mode (not (not currently-visible))))
          
          ;; Force fontification refresh for this region
          (font-lock-flush (point) tail-end))))))

;; Arrow handling for eval results

(defun greger-ui--eval-result-start-tag-with-arrow (node _override _start _end)
  "Show arrow for eval result start tag when folding mode is enabled."
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (if greger-ui-folding-mode
        ;; Show arrow when folding mode is enabled
        (put-text-property node-start node-end 'display (propertize "⇒" 'face 'greger-eval-arrow-face))
      ;; Show tag normally when folding mode is disabled
      (remove-text-properties node-start node-end '(display nil invisible nil)))))

;; Content transformation

(defun greger-ui--tool-content-transformation (node _override _start _end)
  "Font-lock function to transform tool content for syntax highlighting.
NODE is the matched tree-sitter node for tool_content."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (content-text (treesit-node-text node t))
         (parent (treesit-node-parent node)))
    
    ;; Only apply transformation if we have a parent tool_use or tool_result
    (when (and parent 
               (or (string= (treesit-node-type parent) "tool_use")
                   (string= (treesit-node-type parent) "tool_result")))
      
      ;; Create a temporary buffer for syntax highlighting
      (with-temp-buffer
        (insert content-text)
        (goto-char (point-min))
        
        ;; Determine the language mode based on content or context
        (let ((mode (greger-ui--detect-language content-text)))
          (when mode
            (condition-case nil
                (progn
                  (funcall mode)
                  ;; Ensure font-lock is active and force fontification
                  (font-lock-ensure (point-min) (point-max))
                  
                  ;; Copy face properties back to original buffer
                  (greger-ui--copy-face-properties (point-min) (point-max) node-start))
              (error
               ;; If mode activation fails, fall back to basic highlighting
               (greger-ui--apply-basic-highlighting (point-min) (point-max) node-start)))))))))

(defun greger-ui--detect-language (content)
  "Detect programming language from CONTENT and return appropriate mode.
_START and _END are ignored font-lock parameters."
  (cond
   ;; JSON detection
   ((or (string-match-p "^\\s-*[{\\[]" content)
        (string-match-p "\"[^\"]*\"\\s-*:" content))
    'js-mode)
   ;; Python detection
   ((or (string-match-p "^\\s-*def\\s+" content)
        (string-match-p "^\\s-*import\\s+" content)
        (string-match-p "^\\s-*from\\s+.*\\s+import" content))
    'python-mode)
   ;; Shell script detection
   ((or (string-match-p "^#!/bin/\\(bash\\|sh\\)" content)
        (string-match-p "\\$[A-Za-z_][A-Za-z0-9_]*" content))
    'sh-mode)
   ;; XML/HTML detection
   ((string-match-p "<[^>]+>" content)
    'sgml-mode)
   (t nil)))

(defun greger-ui--copy-face-properties (start end target-start)
  "Copy face properties from temp buffer region START to END to target at TARGET-START."
  (let ((pos start)
        (offset (- target-start start)))
    (while (< pos end)
      (let* ((face (get-text-property pos 'face))
             (original-pos (+ pos offset))
             (next-change (next-property-change pos nil end))
             (len (- next-change pos)))
        (when face
          ;; Convert 'face to 'font-lock-face for tree-sitter compatibility
          (with-current-buffer (marker-buffer (make-marker))
            (put-text-property original-pos (min (+ original-pos len) end) 'font-lock-face face)))
        (setq pos next-change)))))

(defun greger-ui--apply-basic-highlighting (start end target-start)
  "Apply basic syntax highlighting from START to END, copying to TARGET-START."
  (let ((pos start)
        (offset (- target-start start)))
    (while (< pos end)
      (let ((char (char-after pos))
            (original-pos (+ pos offset)))
        (cond
         ;; String highlighting
         ((eq char ?\")
          (let ((string-end (or (next-single-char-property-change pos 'face) end)))
            (put-text-property (1+ line-start) line-end 'font-lock-face
                               'font-lock-string-face)))
         ;; Number highlighting  
         ((and char (string-match-p "[0-9]" (string char)))
          (put-text-property (1+ line-start) line-end 'font-lock-face
                             'font-lock-constant-face)))
        (setq pos (1+ pos))))))

(defun greger-ui--syntax-highlight-region (start end)
  "Apply syntax highlighting to region from START to END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Simple keyword highlighting
        (when (re-search-forward "\\b\\(def\\|class\\|import\\|from\\|if\\|else\\|for\\|while\\)\\b" line-end t)
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0)))
            (when (and (>= match-start start) (<= match-end end))
              (add-face-text-property match-start match-end 'font-lock-keyword-face))))
        
        ;; String highlighting
        (goto-char line-start)
        (while (re-search-forward "\"[^\"]*\"" line-end t)
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0)))
            (when (and (>= match-start start) (<= match-end end))
              (add-face-text-property match-start match-end 'font-lock-string-face))))
        
        (forward-line 1)))))

(defun greger-ui--convert-faces-for-treesit (start end)
  "Convert 'face text properties and overlay faces to 'font-lock-face.
This ensures compatibility with tree-sitter font-lock."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((face (get-text-property (point) 'face))
            (overlays (overlays-at (point))))
        
        ;; Convert text property faces
        (when face
          (remove-text-properties (point) (1+ (point)) '(face))
          (put-text-property (point) (1+ (point)) 'font-lock-face face))
        
        ;; Convert overlay faces
        (dolist (overlay overlays)
          (let ((overlay-face (overlay-get overlay 'face)))
            (when overlay-face
              (overlay-put overlay 'face nil)
              (let ((overlay-start (max start (overlay-start overlay)))
                    (overlay-end (min end (overlay-end overlay))))
                (put-text-property overlay-start overlay-end
                                   'font-lock-face overlay-face)))))
        
        (forward-char 1)))))

(defun greger-ui--line-numbers (node _override _start _end)
  "Add line numbers to content.
NODE is the tree-sitter node.
_OVERRIDE, _START, and _END are font-lock parameters."
  (when greger-line-numbers
    (let* ((node-start (treesit-node-start node))
           (node-end (treesit-node-end node))
           (content-lines (split-string (treesit-node-text node t) "\n"))
           (line-count (length content-lines)))
      
      (save-excursion
        (goto-char node-start)
        (dotimes (i line-count)
          (let ((line-start (line-beginning-position))
                (line-end (line-end-position)))
            (when (< line-start node-end)
              (put-text-property line-start (min (1+ line-start) line-end)
                                 'display
                                 (propertize (format "%3d│ " (1+ i))
                                             'font-lock-face '(:height 0.6 :foreground "gray50"))))
            (forward-line 1)))))))

;; Keymap definitions

(defvar greger-ui-tool-content-head-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'greger-ui-toggle-tool-content)
    map)
  "Keymap for tool content head when it's foldable.")

(defvar greger-ui-tool-content-tail-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'greger-ui-toggle-tool-content)
    map)
  "Keymap for tool content tail.")

(defvar greger-url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'greger-browse-url-at-point)
    (define-key map [mouse-1] 'greger-browse-url-at-point)
    map)
  "Keymap for URL links.")

(defun greger-browse-url-at-point ()
  "Browse URL at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (url-text (treesit-node-text node t)))
    (when (string-match "url:\\(.*\\)" url-text)
      (let ((url (match-string 1 url-text)))
        (browse-url url)))))

;; Highlighting function

(defun greger-ui--apply-syntax-highlighting-to-buffer ()
  "Apply syntax highlighting to the entire buffer."
  (interactive)
  (when (derived-mode-p 'greger-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((root (treesit-buffer-root-node)))
        (when root
          (let ((tool-content-nodes (treesit-search-subtree root "tool_content" nil nil 'all)))
            (dolist (node tool-content-nodes)
              (greger-ui--tool-content-transformation node nil nil nil))
            
            ;; Enable font-lock and force fontification
            (font-lock-mode 1)
            (font-lock-ensure (point-min) (point-max))
            
            ;; Copy the font-lock-face properties to the original buffer
            (dolist (node tool-content-nodes)
              (let* ((node-start (treesit-node-start node))
                     (node-end (treesit-node-end node))
                     (content-text (treesit-node-text node t)))
                
                ;; Apply the copied face properties
                (with-temp-buffer
                  (insert content-text)
                  (let ((pos (point-min)))
                    (while (< pos (point-max))
                      (let ((face (get-text-property pos 'font-lock-face))
                            (len (- (next-property-change pos nil (point-max)) pos))
                            (original-pos (+ node-start (- pos (point-min)))))
                        (when face
                          (put-text-property original-pos (min (+ original-pos len) end) 'font-lock-face face)))
                      (setq pos (next-property-change pos nil (point-max))))))))))))))

(provide 'greger-ui)

;;; greger-ui.el ends here
