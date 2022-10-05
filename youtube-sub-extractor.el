;;; yt-sub-extractor.el --- Extract YouTube video subtitles  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 02, 2022
;; Modified: October 02, 2022
;; Version: 0.0.1
;; Keywords: convenience multimedia
;; Homepage: https://github.com/agzam/yt-sub-extractor
;; Package-Requires: ((emacs "28.1") (s "1.13"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package requires youtube-dl cmd line tool installed and availible in $PATH
;;
;;  Description
;;
;;; Code:

(require 's)

(defgroup youtube-sub-extractor nil
  "YouTube Subtitle Extractor"
  :prefix "youtube-sub-extractor-"
  :group 'applications)

(defcustom yt-languages
  '("en")
  "Languages of subtitles to extract. If nil - extracts all available."
  :group 'youtube-sub-extractor
  :type 'list)

(defvar yt--sub-regexp
  "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\}"
  "regexp used to locate timestamps in subtitles file")

(defun yt--seconds (ts)
  "Return total number of seconds for given timestamp TS."
  (when (stringp ts)
    (pcase-let ((`(,s ,m ,h) (parse-time-string ts)))
      (+ (* 3600 h) (* 60 m) s))))

(defun yt-create-subs-buffer (subs-file)
  "Reads SUBS-FILE and inserts the content in a buffer."
  (let* ((raw (with-temp-buffer
                (insert-file-contents subs-file)
                (buffer-string)))
         (full-ts-rx (concat "^" yt--sub-regexp " --> " yt--sub-regexp "$"))
         (fst-sub-pos (string-match full-ts-rx raw))
         ;; split into subtitle segments
         (subs (seq-remove
                'string-blank-p
                (split-string (substring raw fst-sub-pos nil) "\n\n")))
         ;; make a list where each element is (timestamp & position)
         (subs-lst (seq-map (lambda (s)
                              (let* ((ts (when (string-match yt--sub-regexp s)
                                           (match-string 0 s)))
                                     (txt (replace-regexp-in-string full-ts-rx "" s)))
                                (list ts txt))) subs))
         (buf (generate-new-buffer (file-name-base subs-file))))
    (with-current-buffer buf
      (dolist (sub subs-lst)
        (let* ((pos (point))
               (_ (insert (cadr sub)))
               (ovrl (make-overlay (1+ pos) (point) nil t))
               (ovrl-txt (or (car sub) "")))
          (overlay-put
           ovrl 'before-string
           (propertize ovrl-txt
                       'display `((margin left-margin) ,ovrl-txt))))))
    (read-only-mode +1)
    (switch-to-buffer-other-window buf)
    (set-window-margins nil 13)
    (goto-char (point-min))))

(defun yt-extract-subs (video-url)
  "For a given YouTube vid VIDEO-URL, extracts subtitles and opens
them in a buffer."
  (interactive (list (read-string "Enter video URL: ")))
  (unless (executable-find "youtube-dl")
    (error "youtube-dl not found"))
  (let* ((langs (mapconcat 'identity yt-languages ","))
         (args (if (null yt-languages)
                   "--all-subs"
                 (format "--write-sub --sub-lang \"%s\"" langs)))
         (res (shell-command-to-string
               (format "cd /tmp && youtube-dl %s --skip-download \"%s\""
                       args video-url)))
         (fnames (seq-remove
                  'null
                  (seq-map (lambda (s)
                             (when (string-match "\\[info\\] Writing video subtitles to: \\(.*\\)" s)
                               (match-string 1 s)))
                           (split-string res "\n")))))
    (unless fnames
      (error (format "Failed to extract subtitles, youtube-dl output:\n\n%s" res)))

    (let* ((subs-fname (cond
                        ((and (< 1 (length fnames))
                              (or (null yt-languages) (< 1 (length yt-languages))))
                         (completing-read "Choose subtitle variant" fnames nil :require-match))

                        ((eq 1 (length fnames))
                         (car fnames)))))
      (yt-create-subs-buffer (concat "/tmp/" subs-fname)))))

(provide 'youtube-sub-extractor)
;;; yt-sub-extractor.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("yt-" . "youtube-sub-extractor-"))
;; End:
