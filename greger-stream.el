;;; greger-stream.el --- Streaming support for greger -*- lexical-binding: t -*-

;;; Commentary:
;; Handles streaming responses from AI providers

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'greger-providers)
(require 'greger)

;;; Data structures

(cl-defstruct greger-stream-state
  accumulated-output
  complete-response
  parsed-content-blocks
  process
  output-buffer
  undo-handle
  original-quit-binding
  text-start-callback
  text-callback
  complete-callback
  cancel-callback
  restore-callback)

;;; Public API

(cl-defun greger-stream-to-buffer (&key model dialog buffer text-start-callback text-callback complete-callback cancel-callback)
  "Send streaming request for MODEL with DIALOG, inserting text into BUFFER.
TEXT-START-CALLBACK is called when text streaming starts.
TEXT-CALLBACK is called for each text chunk with (state text).
COMPLETE-CALLBACK is called when done with the parsed content blocks array.
CANCEL-CALLBACK is called if cancelled.
BUFFER defaults to current buffer if not specified."
  (greger-stream-to-buffer-with-tools
   :model model
   :dialog dialog
   :tools nil
   :buffer buffer
   :text-start-callback text-start-callback
   :text-callback text-callback
   :complete-callback complete-callback
   :cancel-callback cancel-callback))

(cl-defun greger-stream-to-buffer-with-tools (&key model dialog tools buffer text-start-callback text-callback complete-callback cancel-callback)
  "Send streaming request for MODEL with DIALOG and TOOLS, inserting text into BUFFER.
TEXT-START-CALLBACK is called when text streaming starts.
TEXT-CALLBACK is called for each text chunk with (text).
COMPLETE-CALLBACK is called when done with the parsed content blocks array.
CANCEL-CALLBACK is called if cancelled.
BUFFER defaults to current buffer if not specified."
  (let* ((output-buffer (or buffer (current-buffer)))
         (undo-handle (prepare-change-group output-buffer))
         (original-quit-binding (local-key-binding (kbd "C-g")))
         (provider-config (greger-providers-get-config model))
         (request-spec (greger-providers-build-request provider-config dialog tools))
         (restore-callback (lambda (state)
                             (with-current-buffer (greger-stream-state-output-buffer state)
                               (local-set-key (kbd "C-g")
                                              (greger-stream-state-original-quit-binding state))
                               (undo-amalgamate-change-group (greger-stream-state-undo-handle state))
                               (accept-change-group (greger-stream-state-undo-handle state)))))
         (wrapped-complete-callback (lambda (parsed-blocks state)
                                      (when complete-callback
                                        (funcall complete-callback parsed-blocks))))
         (process (greger-stream--start-curl-process request-spec))
         (state (make-greger-stream-state
                 :accumulated-output ""
                 :complete-response ""
                 :parsed-content-blocks '()
                 :process process
                 :text-start-callback text-start-callback
                 :text-callback text-callback
                 :complete-callback wrapped-complete-callback
                 :cancel-callback cancel-callback
                 :restore-callback restore-callback
                 :output-buffer output-buffer
                 :undo-handle undo-handle
                 :original-quit-binding original-quit-binding)))

    (activate-change-group undo-handle)

    (set-process-filter process
                       (lambda (proc output)
                         (declare (ignore proc))
                         (greger-stream--process-output-chunk output state provider-config)))

    (set-process-sentinel process
                         (lambda (proc event)
                           (declare (ignore event))
                           (greger-stream--handle-completion proc state provider-config)))

    (set-process-query-on-exit-flag process nil)

    (greger-stream--setup-cancel-binding state)

    state))

;;; Internal implementation

(defun greger-stream--setup-cancel-binding (state)
  "Setup C-g binding for cancellation in the output buffer."
  (with-current-buffer (greger-stream-state-output-buffer state)
    (local-set-key (kbd "C-g")
                   (lambda ()
                     (interactive)
                     (greger-stream--cancel-request state)))))

(defun greger-stream--check-for-error (output)
  "Check OUTPUT for error responses and raise an error if found.
Returns nil if no error found or if OUTPUT is not valid JSON."
  (condition-case nil
      (let ((data (json-read-from-string output)))
        (when (and (listp data)
                   (string= (alist-get 'type data) "error"))
          (let* ((error-info (alist-get 'error data))
                 (error-message (alist-get 'message error-info))
                 (error-type (alist-get 'type error-info)))
            (error "API Error (%s): %s" error-type error-message))))
    (error nil)))

(defun greger-stream--process-output-chunk (output state provider-config)
  "Process a chunk of OUTPUT using STATE."
  ;; Check for error responses and raise an error if found
  ;(greger-stream--check-for-error output)

  ;; Always accumulate for complete response
  (setf (greger-stream-state-complete-response state)
        (concat (greger-stream-state-complete-response state) output))

  ;; Update working buffer for chunk processing
  (setf (greger-stream-state-accumulated-output state)
        (concat (greger-stream-state-accumulated-output state) output))

  (greger-stream--process-claude-events state))

(defun greger-stream--process-claude-events (state)
  "Process Claude streaming events from accumulated output in STATE."
  (let ((accumulated (greger-stream-state-accumulated-output state))
        (remaining ""))

    ;; Process complete lines (events)
    (while (string-match "\n" accumulated)
      (let* ((line-end (match-end 0))
             (line (substring accumulated 0 (1- line-end))))

        ;; Process the line if it's a data event
        (when (string-prefix-p "data: " line)
          (let ((data-json (substring line 6)))
            (unless (string= data-json "[DONE]")
              (greger-stream--handle-claude-event data-json state))))

        ;; Remove processed line
        (setq accumulated (substring accumulated line-end))))

    ;; Store remaining incomplete data
    (setf (greger-stream-state-accumulated-output state) accumulated)))

(defun greger-stream--handle-claude-event (data-json state)
; Example of incoming data json (one data-json per line)
; {"type":"message_start","message":{"id":"msg_01Qm7bzEMGbdRyAuF5Lrb1Tg","type":"message","role":"assistant","model":"claude-sonnet-4-20250514","content":[],"stop_reason":null,"stop_sequence":null,"usage":{"input_tokens":2626,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"output_tokens":1,"service_tier":"standard"}}     }
; {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}        }
; {"type": "ping"}
; {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"I"}           }
; {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"'ll first read the existing file to see what's already there,"}            }
; {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":" then add a new function in the same style."}    }
; {"type":"content_block_stop","index":0        }
; {"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"toolu_01NmTNDZcJdGAMsrWQy1Heff","name":"read-file","input":{}}        }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":""}           }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\""}   }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"path\": \"~/s"}        }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"cratch/aicha"}   }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"t/hel"}     }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"lo."}            }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"py\""} }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":", \"includ"}     }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"e_li"}        }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"ne_numb"}       }
; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"ers\": true}"}            }
; {"type":"content_block_stop","index":1            }
; {"type":"message_delta","delta":{"stop_reason":"tool_use","stop_sequence":null},"usage":{"output_tokens":108}        }
; {"type":"message_stop"    }

  ;(greger-agent--debug "INCOMING data-json %s" data-json)
  (let* ((data (json-read-from-string data-json))
         (type (alist-get 'type data)))
    (cond
     ;; Content block start - create new content block
     ((string= type "content_block_start")
      (let* ((index (alist-get 'index data))
             (content-block (copy-alist (alist-get 'content_block data)))
             (blocks (greger-stream-state-parsed-content-blocks state)))

        ;; Initialize content for accumulation
        ;; TODO: is this necessary?
        (cond
         ((string= (alist-get 'type content-block) "tool_use")
          (setf (alist-get 'input content-block) ""))
         ((string= (alist-get 'type content-block) "text")
          (setf (alist-get 'text content-block) "")))

        (when (and (string= (alist-get 'type content-block) "text")
                   (greger-stream-state-text-start-callback state))
          (funcall (greger-stream-state-text-start-callback state)))

        ;; Add block at the right index
        (greger-stream--ensure-block-at-index blocks index content-block state)))

     ;; Content block delta - update existing content block
     ((string= type "content_block_delta")
      (let* ((index (alist-get 'index data))
             (delta (alist-get 'delta data))
             (delta-type (alist-get 'type delta))
             (blocks (greger-stream-state-parsed-content-blocks state)))

        (when (< index (length blocks))
          (let ((block (nth index blocks)))
            (cond
             ;; Text delta
             ((string= delta-type "text_delta")
              (let ((text (alist-get 'text delta)))
                (setf (alist-get 'text block)
                      (concat (alist-get 'text block) text))
                ;; Call text callback for live display
                (when (greger-stream-state-text-callback state)
                  (funcall (greger-stream-state-text-callback state) text))))

             ;; Tool input delta
             ((string= delta-type "input_json_delta")
              (let ((partial-json (alist-get 'partial_json delta)))
                (setf (alist-get 'input block)
                      (concat (alist-get 'input block) partial-json)))))))))

     ;; Content block stop - finalize tool input if needed
     ((string= type "content_block_stop")
      (let* ((index (alist-get 'index data))
             (blocks (greger-stream-state-parsed-content-blocks state)))

        (when (< index (length blocks))
          (let ((block (nth index blocks)))
            (when (and (string= (alist-get 'type block) "tool_use")
                       (stringp (alist-get 'input block)))
              ;; Parse accumulated JSON input
              (let ((input-str (alist-get 'input block)))
                (condition-case nil
                    (if (string-empty-p input-str)
                        (setf (alist-get 'input block) '())
                      (setf (alist-get 'input block)
                            (json-read-from-string input-str)))
                  (error
                   (setf (alist-get 'input block) '()))))))))))))

(defun greger-stream--ensure-block-at-index (blocks index new-block state)
  "Ensure BLOCKS list has NEW-BLOCK at INDEX, extending if necessary."
  (let ((current-blocks (greger-stream-state-parsed-content-blocks state)))
    ;; Extend list if needed
    (while (<= (length current-blocks) index)
      (setq current-blocks (append current-blocks (list nil))))

    ;; Set the block at index
    (setf (nth index current-blocks) new-block)
    (setf (greger-stream-state-parsed-content-blocks state) current-blocks)))

(defun greger-stream--handle-completion (proc state provider-config)
  "Handle process completion for PROC using STATE."
  (when (memq (process-status proc) '(exit signal))
    (funcall (greger-stream-state-restore-callback state) state)

    (if (= (process-exit-status proc) 0)
        (when (greger-stream-state-complete-callback state)
          (let ((parsed-blocks (greger-stream-state-parsed-content-blocks state)))
            (funcall (greger-stream-state-complete-callback state) parsed-blocks state)))
      (when (greger-stream-state-cancel-callback state)
        (funcall (greger-stream-state-cancel-callback state))))))

(defun greger-stream--cancel-request (state)
  "Cancel streaming request using STATE."
  (let ((process (greger-stream-state-process state)))
    (when (process-live-p process)
      (message "Interrupting generation")
      (interrupt-process process)
      (sit-for 0.1)
      (delete-process process)
      (when (greger-stream-state-cancel-callback state)
        (funcall (greger-stream-state-cancel-callback state))))
    (funcall (greger-stream-state-restore-callback state) state)))

;;; Utility functions

(defun greger-stream--start-curl-process (request-spec)
  "Start curl process with REQUEST-SPEC."
  (start-process-shell-command
   "greger-curl" nil
   (greger-stream--build-curl-command request-spec)))

(defun greger-stream--build-curl-command (request-spec)
  "Build curl command from REQUEST-SPEC."
  (let ((url (plist-get request-spec :url))
        (method (plist-get request-spec :method))
        (headers (plist-get request-spec :headers))
        (data (plist-get request-spec :data)))
    (format "curl -s -X %s %s %s %s"
            method
            (mapconcat (lambda (header)
                         (format "-H \"%s: %s\"" (car header) (cdr header)))
                       headers " ")
            (if data
                (format "--data-raw %s" (shell-quote-argument data))
              "")
            url)))

(provide 'greger-stream)

;;; greger-stream.el ends here
