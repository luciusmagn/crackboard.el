;;; crackboard.el --- Crackboard heartbeat extension for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Lukas Hozda

;; Author: Lukas Hozda <me@mag.wiki>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/luciusmagn/crackboard.el

;; Fair License

;; Usage of the works is permitted provided that this instrument is
;; retained with the works, so that any entity that uses the works is
;; notified of this instrument.

;; DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

;;; Commentary:

;; Crackboard is an Emacs extension that sends coding activity heartbeats to
;; Bogan's Crackboard server.
;;
;; It should probably report languages correctly, but in case it doesn't: lmao.

;;; Code:

(require 'json)
(require 'url)

(defvar crackboard-endpoint "http://crackboard.dev/heartbeat"
  "The URL endpoint for sending heartbeats.")

(defvar crackboard-session-key nil
  "The session key for authentication with the Crackboard server.")

(defvar crackboard-last-heartbeat 0
  "Timestamp of the last heartbeat sent.")

(defvar crackboard-heartbeat-interval 123
  "Interval in seconds between heartbeats.")

(defvar crackboard-debug-buffer "*Crackboard Debug*"
  "Name of the buffer for Crackboard debug logs.")

(defun crackboard-get-language (file-or-ext)
  "Get the language name from FILE-OR-EXT (filename or extension)."
  (let* ((filename (file-name-nondirectory file-or-ext))
         (ext (file-name-extension filename t))
         (basename (file-name-sans-extension filename))
         (lang-map
          '(("rs" . "rust")
            ("zig" . "zig")
            ("c" "h" "makefile" . "c")
            ("cpp" "cxx" "cc" "hpp" "hxx" "hh" . "c++")
            ("py" . "python")
            ("go" . "go")
            ("json" . "json")
            ("js" . "javascript")
            ("ts" . "typescript")
            ("tsx" . "typescript")
            ("html" "htm" . "html")
            ("lua" . "lua")
            ("svg" . "svg")
            ("toml" . "toml")
            ("css" . "css")
            ("styl" . "css")
            ("sass" . "css")
            ("scss" . "css")
            ("less" . "css")
            ("md" "markdown" . "markdown")
            ("svelte" . "svelte")
            ("java" . "java")
            ("hbs" "handlebars" . "handlebars")
            ("coffee" . "coffeescript")
            ("el" . "elisp")
            ("lisp" "cl" . "lisp")
            ("xml" . "xml")
            ("kt" . "kotlin")
            ("cmake" "cmake.in" . "cmake")
            ("php" . "php")
            ("ps1" . "powershell")
            ("prisma" . "prisma")
            ("sql" . "sql")
            ("astro" . "astro")
            ("rb" . "ruby")
            ("templ" . "templ")
            ("tex" . "tex")
            ("yaml" "yml" . "yaml")
            ("odin" . "odin")
            ("nix" . "nix")
            ("nim" . "nim")))
         (ext-without-dot (if (string-prefix-p "." ext) (substring ext 1) ext))
         (result (or (cl-some (lambda (entry)
                                (when (member ext-without-dot (ensure-list (car entry)))
                                  (cdr entry)))
                              lang-map)
                     (cl-some (lambda (entry)
                                (when (member (downcase basename) (ensure-list (car entry)))
                                  (cdr entry)))
                              lang-map)
                     (cl-some (lambda (entry)
                                (when (member (downcase filename) (ensure-list (car entry)))
                                  (cdr entry)))
                              lang-map))))
    (crackboard-log "Language detection: file-or-ext=%s, filename=%s, ext=%s, basename=%s, result=%s"
                    file-or-ext filename ext basename (or result "txt"))
    (or result "txt")))

(defun ensure-list (x)
  "If X is already a list, return it. Otherwise, return a list with X as its only element."
  (if (listp x) x (list x)))

(defun crackboard-log (message &rest args)
  "Log MESSAGE with ARGS to the Crackboard debug buffer."
  (with-current-buffer (get-buffer-create crackboard-debug-buffer)
    (goto-char (point-max))
    (insert (apply #'format (concat "[%s] " message "\n")
                   (format-time-string "%Y-%m-%d %H:%M:%S")
                   args))))

(defun crackboard-init (key)
  "Initialize Crackboard with the given session KEY. No activity will be recorded without this."
  (setq crackboard-session-key key)
  (add-hook 'after-save-hook #'crackboard-send-heartbeat)
  (run-with-idle-timer crackboard-heartbeat-interval t #'crackboard-check-idle-time))

(defun crackboard-send-heartbeat ()
  "Send a heartbeat to the Crackboard server."
  (when crackboard-session-key
    (let* ((now (float-time))
           (language (crackboard-get-language (buffer-file-name)))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/json")))
           (url-request-data
            (encode-coding-string
             (json-encode
              `(("timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
                ("session_key" . ,crackboard-session-key)
                ("language_name" . ,language)))
             'utf-8)))
      (when (> (- now crackboard-last-heartbeat) crackboard-heartbeat-interval)
        (crackboard-log (buffer-file-name))
        (crackboard-log (crackboard-get-language (buffer-file-name)))
        (setq crackboard-last-heartbeat now)
        (url-retrieve crackboard-endpoint #'crackboard-handle-response nil t t)))))

(defun crackboard-handle-response (status &rest _)
  "Handle the response from the Crackboard server.
STATUS is the response status, _ captures any additional arguments."
  (if (plist-get status :error)
      (let ((error-details (plist-get status :error)))
        (message "Failed to send heartbeat: %S" error-details)
        (crackboard-log "Failed to send heartbeat: %S" error-details))
    (message "Heartbeat sent successfully")
    (crackboard-log "Heartbeat sent successfully"))
  (kill-buffer (current-buffer)))

(defun crackboard-check-idle-time ()
  "Check if it's time to send a heartbeat due to idle time."
  (when (and (current-idle-time)
             (> (float-time (current-idle-time)) crackboard-heartbeat-interval))
    (crackboard-send-heartbeat)))

(provide 'crackboard)
;;; crackboard.el ends here
