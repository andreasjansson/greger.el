#!/usr/bin/env emacs --script
;;; run-thinking-tests.el --- Run thinking-related tests for greger -*- lexical-binding: t -*-

;;; Commentary:
;; This script runs all thinking-related tests for greger.
;; Usage: emacs --script run-thinking-tests.el

;;; Code:

(add-to-list 'load-path "..")
(add-to-list 'load-path ".")
(require 'ert)

;; Load test files
(load "test-greger-client.el")
(load "test-end-to-end.el")

(message "Running thinking-related tests...")
(message "")

;; Run unit tests
(message "=== Unit Tests ===")
(ert-run-tests-batch-and-exit '(tag thinking))

;;; run-thinking-tests.el ends here
