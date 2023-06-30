(import :gerbil/gambit/random
        :std/iter
        :std/misc/shuffle
        ./vault)
(export generate-password)

;;; password generation
(def lowercase "abcdefghijklmnopqrstuvwxyz")
(def uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def digit     "1234567890")
(def special   "~!@#$%^&*()-+=:;,.<>/?[]{}")
(def all-chars
  (string-append lowercase
                 uppercase
                 digit
                 special))

(def (get-password-chars src count)
  (def result (make-string count))

  (for (i (in-range count))
    (string-set! result i (string-ref src (random-integer (string-length src)))))

  result)

(def (generate-password len lowercase: l uppercase: u digit: d special: s)
  (def the-password (make-string len))
  (def cursor 0)

  (defrules add-password-chars! ()
    ((_ source count)
     (let (chars (get-password-chars source count))
       (substring-move! chars 0 count the-password cursor)
       (set! cursor (+ cursor count)))))

  (add-password-chars! lowercase l)
  (add-password-chars! uppercase u)
  (add-password-chars! digit d)
  (add-password-chars! special s)
  (add-password-chars! all-chars (max (- len l u d s) 0))

  (vector->string (vector-shuffle! (string->vector the-password))))
