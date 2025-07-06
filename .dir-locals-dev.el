;;; .dir-locals-dev.el --- Development configuration for greger.el -*- lexical-binding: t -*-

;; This file contains development-specific configuration for greger.el
;; Copy this to .dir-locals.el to enable development branch settings
;; 
;; Usage:
;;   1. Copy this file to .dir-locals.el:
;;      cp .dir-locals-dev.el .dir-locals.el
;;   2. Update the branch name below to your development branch
;;   3. Restart Emacs or run M-x revert-buffer in greger.el files
;;   4. Run M-x greger-install-grammar to install from your branch

((nil . ((greger-local-grammar-path . "your-development-branch-name"))))

;; Alternative: You can also set this in your init.el or interactively:
;; (setq greger-local-grammar-path "your-branch-name")
;; or
;; M-x greger-set-grammar-branch RET your-branch-name RET

;;; .dir-locals-dev.el ends here
