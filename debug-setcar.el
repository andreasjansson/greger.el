;;; Debug setcar behavior

(defun test-setcar ()
  (let* ((original-list '((a . 1) (b . 2) (c . 3)))
         (current-list (cdr original-list))) ; Points to ((b . 2) (c . 3))

    (message "Original list: %S" original-list)
    (message "Current list: %S" current-list)
    (message "Car of current list: %S" (car current-list))

    ;; Modify the car of current-list
    (setcar current-list (cons '(new . "value") (car current-list)))

    (message "After setcar:")
    (message "Original list: %S" original-list)
    (message "Current list: %S" current-list)
    (message "Car of current list: %S" (car current-list))))

(test-setcar)
