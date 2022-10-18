;;; youtube-sub-extractor.el --- Extract YouTube video subtitles  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 02, 2022
;; Modified: October 02, 2022
;; Version: 0.0.1
;; Keywords: convenience multimedia
;; Homepage: https://github.com/agzam/youtube-sub-extractor.el
;; Package-Requires: ((emacs "27"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;
;; This package requires https://github.com/yt-dlp cmd line tool, installed and availible in $PATH
;;
;;  Description
;;
;;; Code:

(defgroup youtube-sub-extractor nil
  "YouTube Subtitle Extractor."
  :prefix "youtube-sub-extractor-"
  :group 'applications)

(defcustom youtube-sub-extractor-languages
  '("en")
  "Languages of subtitles to extract. If nil - extracts all available."
  :group 'youtube-sub-extractor
  :type 'list)

(defcustom youtube-sub-extractor-executable-path
  nil
  "Path to yt-dlp (preferred) or youtube-dl executable."
  :group 'youtube-sub-extractor
  :type 'string)

(defcustom youtube-sub-extractor-min-chunk-size
  5
  "Minimum number of seconds between subs."
  :group 'youtube-sub-extractor
  :type 'number)

(defvar youtube-sub-extractor--ts-rx
  "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\}"
  "Regexp used to locate timestamps in subtitles file.")

(defun youtube-sub-extractor--seconds (ts)
  "Return total number of seconds for given timestamp TS."
  (when (and (stringp ts)
             (string-match-p youtube-sub-extractor--ts-rx ts))
    (pcase-let* ((`(,s ,m ,h) (parse-time-string ts))
                 (ms (string-to-number (substring ts -3))))
      (+ (* 3600 h) (* 60 m) s (/ ms 1000.0)))))

(defun youtube-sub-extractor--duration (long-ts)
  "Calculate duration in seconds in LONG-TS string.
i.e., string containing '00:00:07.200 --> 00:00:09.830'"
  (when (string-match
         (format "\\(%1$s\\) --> \\(%1$s\\)" youtube-sub-extractor--ts-rx)
         long-ts)
    (- (youtube-sub-extractor--seconds (match-string 2 long-ts))
       (youtube-sub-extractor--seconds (match-string 1 long-ts)))))

(defun youtube-sub-extractor--add (ts1 ts2)
  "Add long timestamp TS1 to another long timestamp TS2."
  (let* ((rx (format "\\(%1$s\\) --> \\(%1$s\\)"
                     youtube-sub-extractor--ts-rx))
         (start (when (string-match rx ts1)
                  (match-string 1 ts1)))
         (end (when (string-match rx ts2)
                (match-string 2 ts2))))
    (format "%s --> %s" start end)))

(defun youtube-sub-extractor--find-exe ()
  "Attempts to find the command-line util to extract subs."
  (if-let ((exe (or (executable-find (or youtube-sub-extractor-executable-path "yt-dlp"))
                    (executable-find "youtube-dl"))))
      exe
    (error "ERROR: I couldn't locate yt-dlp or youtube-dl!")))

(defun youtube-sub-extractor--process-subs (subs-string)
  "Take single SUBS-STRING and return list of tuples.
Each is a timestamp, duration and the corresponding sub."
  (let* ((ts-line-rx (format "^%1$s --> %1$s\\( .*$\\|$\\)" youtube-sub-extractor--ts-rx))
         ;; let's remove all cue and decoration tags
         (tags-n-karaoke-rx "<.>\\|<\\/.>\\|\\(<[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\}>\\)")
         ;; first we simply parse things and gather into a list of tuples
         ;; with timestamp and list of subtitles
         (first-pass
          (seq-reduce
           (lambda (acc nxt)
             (let ((prev-el (car (last acc))))
               (cond
                ;; timestamp
                ((string-match-p ts-line-rx nxt)
                 (append acc (list (list (string-trim nxt)))))
                ;; actual sub
                ((and prev-el
                      (not (string-match-p ts-line-rx nxt))
                      (listp prev-el)
                      (string-match-p ts-line-rx (car prev-el)))
                 (append
                  (butlast acc)
                  (list
                   (append
                    prev-el
                    (list (replace-regexp-in-string tags-n-karaoke-rx "" nxt))))))

                (t acc))))
           (split-string subs-string "\n" :omit-nulls " *")
           ()))
         ;; group subs by chunks of the size of `youtube-sub-extractor-min-chunk-size',
         ;; otherwise it splits them into pieces too small. Also, need to take care of
         ;; duplicates
         (second-pass
          (seq-reduce
           (lambda (acc nxt)
             (let* ((ts (when (string-match
                               (format "%1$s --> %1$s" youtube-sub-extractor--ts-rx)
                               (car nxt))
                          (match-string 0 (car nxt))))
                    (prev-ts (cl-first (cl-first (last acc))))
                    (subs (cdr nxt))
                    (prev-subs (cadr (cl-first (last acc)))))
               (cond
                ((and prev-ts
                      (< (+ (youtube-sub-extractor--duration prev-ts)
                            (youtube-sub-extractor--duration ts))
                         youtube-sub-extractor-min-chunk-size))
                 (append
                  (butlast acc)
                  (list
                   (list
                    (youtube-sub-extractor--add prev-ts ts)
                    (seq-uniq (append prev-subs subs))))))

                (t (append
                    acc
                    (list
                     (list ts (seq-difference
                               (seq-uniq subs)
                               prev-subs))))))))
           first-pass
           ())))
    second-pass))

(defun youtube-sub-extractor--create-subs-buffer (subs-file)
  "Read SUBS-FILE and insert the content in a buffer."
  (let* ((raw (with-temp-buffer
                (insert-file-contents subs-file)
                (buffer-string)))
         (subs-lst (youtube-sub-extractor--process-subs raw))
         (buf (generate-new-buffer (file-name-base subs-file)))
         (mins-only? (zerop (nth 2 (parse-time-string (cl-first (cl-first (last subs-lst))))))))
    (with-current-buffer buf
      (insert (format "%s\n\n" (file-name-base subs-file)))
      (dolist (el subs-lst)
        (let* ((ts (substring (nth 0 el) (if mins-only? 3 0) 8))
               (pos (point))
               (_ (insert (format "%s\n" (string-join (nth 1 el) " "))))
               (ovrl (make-overlay (1+ pos) (point) nil t))
               (ovrl-txt (or ts "")))
          (overlay-put ovrl 'before-string
           (propertize ovrl-txt
                       'display `((margin left-margin) ,ovrl-txt)))))
      (goto-char (point-min))
      (read-only-mode +1))
    (switch-to-buffer-other-window buf)
    (set-window-margins nil 8)))

(defun youtube-sub-extractor-extract-subs (video-url)
  "For a given YouTube vid VIDEO-URL, extract subtitles and open them in a buffer."
  (interactive (list (read-string "Enter video URL: ")))
  (let* ((langs (mapconcat 'identity youtube-sub-extractor-languages ","))
         (args (format "--write-auto-subs --write-subs --sub-langs \"%s\" --skip-download" (or langs "all")))
         (res (shell-command-to-string
               (format
                "cd /tmp && %s %s \"%s\""
                (youtube-sub-extractor--find-exe) args video-url)))
         (fnames (seq-remove
                  'null
                  (seq-map (lambda (s)
                             (when (string-match "\\[info\\] Writing video subtitles to: \\(.*\\)" s)
                               (match-string 1 s)))
                           (split-string res "\n")))))
    (unless fnames
      (error (format "Failed to extract subtitles, output log:\n\n%s" res)))

    (let* ((subs-fname (cond
                        ((and (< 1 (length fnames))
                              (or (null youtube-sub-extractor-languages)
                                  (< 1 (length youtube-sub-extractor-languages))))
                         (completing-read "Choose subtitle variant" fnames nil :require-match))

                        ((eq 1 (length fnames))
                         (car fnames)))))
      (youtube-sub-extractor--create-subs-buffer (concat "/tmp/" subs-fname))
      (dolist (f fnames)
        (delete-file (concat "/tmp/" f))))))

(provide 'youtube-sub-extractor)
;;; youtube-sub-extractor.el ends here
