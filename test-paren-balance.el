;; Test file for paren balance checking

(defun test-function ()
  "A test function with balanced parens."
  (let ((x 1)
        (y 2))
    (+ x y)))

;; This has an extra closing paren: (foo))
;; This has an extra opening paren: ((foo)

(defun another-function ()
  "Another function."
  (message "Hello"))

;; String with parens: "This has (parens) in it"
;; Comment with parens: ; This comment has (parens) too
