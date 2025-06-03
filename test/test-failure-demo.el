;;; test-failure-demo.el --- Demo test file with failures -*- lexical-binding: t -*-

(require 'ert)

(ert-deftest test-demo-failure-1 ()
  "A test that should fail for demonstration."
  (should (= 1 2)))

(ert-deftest test-demo-failure-2 ()
  "Another test that should fail for demonstration."
  (should (string= "hello" "world")))

(ert-deftest test-demo-success ()
  "A test that should pass."
  (should (= 1 1)))

(provide 'test-failure-demo)

;;; test-failure-demo.el ends here
