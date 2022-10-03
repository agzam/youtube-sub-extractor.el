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
;; Package-Requires: ((emacs "28.1"))
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
(defgroup youtube-sub-extractor nill
  "YouTube Subtitle Extractor"
  :prefix "youtube-sub-extractor-"
  :group 'applications)

(defcustom yt-languages
  '("en")
  "Languages of subtitles to extract. If nil - extracts all available."
  :group 'youtube-sub-extractor
  :type 'list)

(defun yt-remove-timestamps ()
  "Flush lines with timestamps in subtitles buffer."
  (interactive)
  (flush-lines
   "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\} --> [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\}$"))

(defun yt-extract-subs (video-url)
  "For a given YouTube vid VIDEO-URL, extracts subtitles and opens them in a buffer."
  (interactive "P")
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
    (dolist (f fnames)
      (let ((buf (generate-new-buffer f)))
        (with-current-buffer buf
          (insert-file-contents (concat "/tmp/" f)))
        (display-buffer buf)))))

;; (setq yt-languages '("en" "es"))

;; (yt-extract-subs "https://www.youtube.com/watch?v=Hu4Yvq-g7_Y")

(provide 'youtube-sub-extractor)
;;; yt-sub-extractor.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("yt-" . "youtube-sub-extractor-"))
;; End:
