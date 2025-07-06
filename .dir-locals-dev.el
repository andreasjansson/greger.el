;;; .dir-locals-dev.el --- Local development configuration for greger.el -*- lexical-binding: t -*-

;; This file contains development-specific configuration for greger.el
;; Copy this to .dir-locals.el to enable local development settings
;; 
;; Usage:
;;   1. Copy this file to .dir-locals.el:
;;      cp .dir-locals-dev.el .dir-locals.el
;;   2. Restart Emacs or run M-x revert-buffer in greger.el files
;;   3. The local grammar will be automatically used for development

((nil . ((greger-local-grammar-path . "../greger-grammar"))))

;; Alternative: You can also set this in your init.el or interactively:
;; (setq greger-local-grammar-path (expand-file-name "../greger-grammar"))
;; or
;; M-x customize-variable RET greger-local-grammar-path RET

;;; .dir-locals-dev.el ends here
