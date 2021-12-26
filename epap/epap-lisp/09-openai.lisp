;;; This code is from Mark Watson's GitHub repository.
;;;
;;;   URL: <https://github.com/mark-watson/loving-common-lisp>
;;;   Commit: 534c9f2fda1bd44f2e22332ad7e7cceef8530e12
;;;
;;; From Watson's repository README:
;;;
;;; > This repo is for the 6th edition of my book that was released
;;; > May 30, 2020
;;; >
;;; > Open source examples for my book "Loving Common Lisp, or the
;;; > Savvy Programmer's Secret Weapon" that is available at
;;; > https://leanpub.com/lovinglisp My book is available to read free
;;; > online and the PDF for the book is licensed with Creative Common
;;; > share with no modifications or commercial reuse: this means that
;;; > you are encouraged to share copies (up to date copies only,
;;; > please) of the PDF with friends and co-workers. New editions of
;;; > the book are free if you have purchased the book before. Many
;;; > thanks to the people who support my writing by purchasing
;;; > the book!
;;; >
;;; > The source code in this repository may be used either under the
;;; > Apache 2 or the LGPL version 3 licenses. Use whichever license
;;; > that works best for you.
;;;
;;; The AGPL3 license for the EPAP repository is not "directly"
;;; compatible with LGPL3, but the LGPL3 allows us to relicense this
;;; particular file as GPL3.
;;;

(uiop:define-package :openai
  (:use :common-lisp)
  (:export completions
           summarize
           answer-question))

(in-package #:openai)

(defparameter *openai-api-base*
  "https://api.openai.com/v1/engines")

(defparameter *openai-model*
  :davinci)

(defun openai-request-url ()
  (format nil "~a/~(~a~)/completions" *openai-api-base* *openai-model*))

(defun friendly-getenv (x)
  (let ((value (uiop:getenv x)))
    (or value (restart-case (error "missing environment variable ~A" x)
                (:setenv (value)
                 :report "Set the environment variable"
                 :interactive (lambda ()
                                (format t "New value for ~A: " x)
                                (let ((new-value (eval (read))))
                                  (setf (uiop:getenv x) new-value)
                                  (list new-value)))
                  value)))))

(defun openai-secret-key ()
  (friendly-getenv "OPENAI_KEY"))

(defun openai-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string
           :error-output :interactive)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        ;; extract text (this might change if OpenAI changes JSON return format):
        (cdar (cadr (nth 4 json-as-list)))))))

(defun openai-curl-command (&rest data)
  (list "curl" (openai-request-url)
        "-H" "Content-Type: application/json"
        "-H" (format nil "Authorization: Bearer ~a" (openai-secret-key))
        "-d" (json:encode-json-plist-to-string data)))

(defun answer-question (question-text max-tokens)
  (let* ((q-text
           (concatenate
            'string
            "\nQ: " question-text "\nA:"))
         (curl-command
           (openai-curl-command
            :prompt q-text
            :max_tokens max-tokens
            :presence_penalty 0.0
            :stop '("\\n")
            :temperature 0.0
            :top_p 1.0
            :frequency_penalty 0.0))
         (answer (openai-helper curl-command))
         (index (search "nQ:" answer)))
    (if index
        (string-trim " " (subseq answer 0 index))
        (string-trim " " answer))))

(defun completions (&key
                      prompt
                      (model :davinci)
                      (temperature 0.6)
                      max-tokens
                      stops
                      include-prompt
                      )
  (let ((*openai-model* model))
    (format nil "~a~a"
            (if include-prompt prompt "")
            (openai-helper
             (openai-curl-command
              :prompt prompt
              :max_tokens max-tokens
              :temperature temperature
              :stop stops)))))

(defun summarize (some-text max-tokens)
  (openai-helper
   (openai-curl-command
    :prompt some-text
    :max_tokens max-tokens
    :temperature 0.3
    :top_p 1.0
    :frequence_penalty 0.0
    :presence_penalty 0.0)))

(defmacro with-codex (&body body)
  `(let ((*openai-model* :davinci-codex))
     ,@body))
