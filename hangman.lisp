(cl:defpackage :hangman
  (:use :cl))

(in-package :hangman)

(defmacro strings (&rest args)
  `(quote ,(loop for x in args
              collect (coerce (string x) 'list))))

(defparameter *dictionary* (strings apple banana cantaloupe dairy elephant
                                    fairy gorilla hairy))

(defparameter *guesses* nil
  "A list of upper-case guesses")

(defparameter *secret-word* nil
  "A char array")

(defparameter *play-forever* t)
(setf *play-forever* nil)

(defun choose-word ()
  (setf *secret-word*
        (nth (random (length *dictionary*)) *dictionary*)))

;; (defun )

(defun masked-word (&optional (word *secret-word*) (guesses *guesses*))
    (mapcar (lambda (c) (if (find c guesses)
                            c
                            #\_))
            word))

(defun format-word (word)
  (format nil "~{~a~^ ~}"
          word))

;;; How do I make an optional argument that points defaults to *guesses*?
(defun guess (&optional (c (first (coerce (read-line) 'list))))
  (if c (pushnew (char-upcase c) *guesses*)))


(defun won? ()
  (every (lambda (c) (find c *guesses*)) *secret-word*))

(defun wrong-guesses ()
  (remove-if (lambda (c) (find c *secret-word*)) *guesses* ))

(defun lost? ()
  (> (length (wrong-guesses)) 12))

(defun take-turn ()
  (fresh-line)
  (princ (format-word (masked-word)))
  (fresh-line)
  (princ "Please enter your guess: ")
  (guess))

(defun play ()
  (loop do (progn (setf *guesses* nil)
               (choose-word)
               (loop
                  until (or (won?) (lost?))
                  do (take-turn)
                  finally (progn (fresh-line)
                                 (princ (if (won?)
                                            "Hooray! You won!"
                                            "Game over.")))))
     while *play-forever*))

(defun play-forever ()
  (let ((*play-forever* t))
    (play)))

(setf *play-forever* nil)
