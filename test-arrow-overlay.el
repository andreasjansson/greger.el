;;; test-arrow-overlay.el --- Test arrow overlay

;; Load greger modules
(add-to-list 'load-path ".")
(require 'greger)
(require 'greger-ui)

(defun test-arrow-overlay ()
  "Test if arrow overlays are created."
  (find-file "test-eval.greger")
  (greger-mode)
  (setq greger-ui-folding-mode t)
  
  ;; Force font-lock
  (font-lock-flush)
  (font-lock-ensure)
  
  ;; Count all overlays in buffer
  (let ((all-overlays (overlays-in (point-min) (point-max)))
        (arrow-overlays 0)
        (fold-overlays 0))
    
    (dolist (overlay all-overlays)
      (when (overlay-get overlay 'greger-ui-eval-arrow-overlay)
        (setq arrow-overlays (1+ arrow-overlays))
        (message "Found arrow overlay at %d-%d: '%s'" 
                 (overlay-start overlay) (overlay-end overlay)
                 (overlay-get overlay 'after-string)))
      (when (overlay-get overlay 'greger-ui-eval-fold-overlay)
        (setq fold-overlays (1+ fold-overlays))
        (message "Found fold overlay at %d-%d: '%s'" 
                 (overlay-start overlay) (overlay-end overlay)
                 (overlay-get overlay 'after-string))))
    
    (message "Total overlays: %d, Arrow overlays: %d, Fold overlays: %d" 
             (length all-overlays) arrow-overlays fold-overlays))
  
  ;; Check for display text properties
  (let ((display-props 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((display-prop (get-text-property (point) 'display)))
          (when display-prop
            (setq display-props (1+ display-props))
            (message "Found display property at %d: %s" (point) display-prop)))
        (goto-char (1+ (point)))))
    (message "Total display properties found: %d" display-props))
  
  ;; Also test manually creating an arrow overlay
  (message "Testing manual arrow overlay creation...")
  (goto-char 20)
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'after-string
                 (propertize "⇒TEST" 'face 'greger-eval-arrow-face))
    (message "Manual arrow overlay created at position %d" (point))))

;; Run the test
(test-arrow-overlay)

;;; test-arrow-overlay.el ends here
