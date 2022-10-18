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

(defun file-to-string (filename)
  "File to string function"
  ;; adjust filename for `buttercup-run-at-point', for testing locally
  (when (equal '("tests") (last (split-string default-directory "/" :omit-nulls)))
    (setq filename (replace-regexp-in-string "tests/" "" filename)))
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(describe "parsing sample.en.vtt, normally"
  :var (eng-sample-str parsed)
  (before-all
    (setq eng-sample-str (file-to-string "tests/sample.en.vtt"))
    (setq parsed (youtube-sub-extractor--process-subs eng-sample-str)))

  (it "should parse sample.en.vtt"
    (expect (length parsed) :to-equal 113)
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
              ("This keeps on happening until his desired" "new habit is nearly forgotten.")))
    (expect (car (last parsed)) :to-equal
            '("00:08:43.770 --> 00:08:45.430"
              ("better than yesterday.")))
    (expect (nth (1- (length parsed)) parsed) :to-equal
            '("00:08:43.770 --> 00:08:45.430"
              ("better than yesterday.")))
    (expect (nth (- (length parsed) 2) parsed) :to-equal
            '("00:08:39.839 --> 00:08:43.770"
              ("And make sure you're subscribed so you don't" "miss out on more videos that will make you")))))

(describe "parsing sample.es.vtt, normally"
  :var (es-sample-str parsed)
  (before-all
    (setq es-sample-str (file-to-string "tests/sample.es.vtt"))
    (setq parsed (youtube-sub-extractor--process-subs es-sample-str)))

  (it "should parse sample.en.vtt"
    (expect (length parsed) :to-equal 113)
    (expect (nth 0 parsed) :to-equal
            '("00:00:08.000 --> 00:00:12.290"
              ("De vez en cuando llega un momento en que Mike se despierta y quiere hacer un cambio")))
    (expect (nth 1 parsed) :to-equal
            '("00:00:12.290 --> 00:00:14.090"
              ("en su vida.")))
    (expect (nth 2 parsed) :to-equal
            '("00:00:14.090 --> 00:00:18.820"
              ("A veces quiere comenzar a hacer ejercicio y ponerse en forma, otras veces quiere leer")))
    (expect (nth 10 parsed) :to-equal
            '("00:00:52.450 --> 00:00:58.210"
              ("Esto sigue sucediendo hasta que su nuevo hábito deseado es casi olvidado.")))
    (expect (car (last parsed)) :to-equal
            '("00:08:43.770 --> 00:08:45.430"
              ("mejor que ayer.")))
    (expect (nth (1- (length parsed)) parsed) :to-equal
            '("00:08:43.770 --> 00:08:45.430"
               ("mejor que ayer.")))
    (expect (nth (- (length parsed) 2) parsed) :to-equal
            '("00:08:39.839 --> 00:08:43.770"
              ("Y asegúrate de estar suscrito para no perderte más videos que te harán")))))

(describe "parsing sample-autogen.es.vtt, normally"
  :var (en-autogen-sample-str parsed)
  (before-all
    (setq en-autogen-sample-str (file-to-string "tests/sample-autogen.en.vtt"))
    (setq parsed (youtube-sub-extractor--process-subs en-autogen-sample-str)))

  (it "should parse sample-autogen.es.vtt"
    (expect (length parsed) :to-equal 157)
    (expect (nth 0 parsed) :to-equal
            '("00:00:00.000 --> 00:00:07.260" ("[Music]")))
    (expect (nth 1 parsed) :to-equal
            '("00:00:07.260 --> 00:00:11.840"
              ("[Music]" "every once in a while there comes a time" "when Mike wakes up and wants to make a")))
    (expect (nth 2 parsed) :to-equal
            '("00:00:11.840 --> 00:00:15.020"
              ("change in his life sometimes he wants to")))
    (expect (nth 10 parsed) :to-equal
            '("00:00:37.430 --> 00:00:41.210"
              ("anymore so mike says to himself it's")))
    (expect (car (last parsed)) :to-equal
            '("00:08:46.580 --> 00:09:04.830"
              ("[Music]")))
    (expect (nth (1- (length parsed)) parsed) :to-equal
            '("00:08:46.580 --> 00:09:04.830"
               ("[Music]")))
    (expect (nth (- (length parsed) 2) parsed) :to-equal
            '("00:08:43.490 --> 00:08:46.580"
              ("make you better than yesterday")))))

(describe "test -available-langs for when there are available subs"
  :var (youtube-sub-extractor--send-request)
  (before-all
    (setf (symbol-function 'youtube-sub-extractor--send-request)
          (lambda (video-url args)
            (expect args :to-equal "--list-subs --no-simulate --skip-download --no-playlist")
            (expect video-url :to-match "pelicula_prueba")
            ;; read fixture file instead of sending request
            (file-to-string "tests/list-subs-output.txt")))

    (spy-on 'youtube-sub-extractor--send-request))

  (it "should return the list of available languages"
    (expect (youtube-sub-extractor--available-langs "https://www.youtube.com/watch?v=pelicula_prueba")
            :to-equal
            '(("ar" "Arabic")
              ("cs" "Czech")
              ("en" "English")
              ("fr" "French")
              ("iw" "Hebrew")
              ("id" "Indonesian")
              ("pt-PT" "Portuguese (Portugal)")
              ("es" "Spanish")))))

(describe "--available-langs for when no available subs"
  :var (youtube-sub-extractor--send-request)
  (before-all
    (setf (symbol-function 'youtube-sub-extractor--send-request)
          (lambda (video-url args)
            (expect args :to-equal "--list-subs --no-simulate --skip-download --no-playlist")
            (expect video-url :to-match "pelicula_prueba")
            ;; read fixture file instead of sending request
            (file-to-string "tests/list-subs-no-available-output.txt")))

    (spy-on 'youtube-sub-extractor--send-request))

  (it "should return nil, since no available languages"
    (expect (youtube-sub-extractor--available-langs "https://www.youtube.com/watch?v=pelicula_prueba")
            :to-equal
            nil)))

;;; youtube-sub-extractor-tests.el ends here
