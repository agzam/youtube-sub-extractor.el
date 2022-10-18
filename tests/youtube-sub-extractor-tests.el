;;; youtube-sub-extractor-tests.el  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;;
;;; Code:
(require 'buttercup)
(require 'youtube-sub-extractor)

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(describe "parsing en.vtt sample normally"
  :var (eng-sample-str
        parsed)
  (before-all
    (setq eng-sample-str (file-to-string "./sample.en.vtt"))
    (setq parsed (youtube-sub-extractor--process-subs eng-sample-str)))

  (it "should parse"
    (pp parsed)
    (expect (nth 0 parsed) :to-equal
            '("00:00:08.000 --> 00:00:12.290"
              ("Every once in a while there comes a time when" "Mike wakes up and he wants to make a change")))
    (expect (nth 1 parsed) :to-equal
            '("00:00:12.290 --> 00:00:14.090"
              ("in his life.")))
    (expect (nth 2 parsed) :to-equal
            '("00:00:14.090 --> 00:00:18.820"
             ("Sometimes he wants to start exercising and" "get into shape, other times he wants to read")))
    (expect (nth 10 parsed) :to-equal
            '("00:00:52.450 --> 00:00:58.210"
              ("This keeps on happening until his desired" "new habit is nearly forgotten.")))))


;;; youtube-sub-extractor-tests.el ends here
