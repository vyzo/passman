(import :gerbil/gambit
        :std/sugar
        :std/iter
        :std/misc/shuffle
        (prefix-in ./vault |vault:|))
(export create-vault
        generate-password)

;;; vault creation
(def (create-vault path: path passphrase: pass)
  (when (file-exists? path)
    (error "vault already exists" path))
  (let* ((pass (or pass (get-new-passphrase)))
         (dir (path-directory path)))
    (unless (string-empty? dir)
      (create-directory* dir))
    (vault:create-vault path pass)
    (void)))

;;;; passphrase input
(def (get-passphrase (prompt "Enter passphrase: "))
  (display prompt)
  (force-output)
  (##tty-mode-set! (current-input-port) #f #f #f #f 0) ; turn off echo
  (try (let (passphrase (read-line))
         (newline)
         passphrase)
       (finally (##tty-mode-reset))))

(def (get-new-passphrase)
  (let* ((pass1 (get-passphrase "Enter new passphrase: "))
         (pass2 (get-passphrase "Re-enter passphrase: ")))
    (unless (equal? pass1 pass2)
      (error "passphrases don't match"))
    pass1))

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
