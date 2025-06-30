;;; debug-terminal.el --- Debug terminal sequence processing

(require 'greger-ui)

(defun debug-terminal-processing ()
  "Debug terminal sequence processing step by step."
  (with-temp-buffer
    (greger-ui--process-terminal-sequences "old content")
    (message "After 'old content': buffer='%s', point=%d" (buffer-string) (point))
    
    (greger-ui--process-terminal-sequences "\r")
    (message "After \\r: buffer='%s', point=%d" (buffer-string) (point))
    
    (greger-ui--process-terminal-sequences "new content")
    (message "After 'new content': buffer='%s', point=%d" (buffer-string) (point))
    
    (message "Final result: '%s'" (buffer-string))))

(debug-terminal-processing)
