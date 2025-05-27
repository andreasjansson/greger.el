(require 'ert)

(ert-deftest test-addition-basic ()
  "Test that 1 + 1 equals 2."
  (should (equal (+ 1 1) 2)))
