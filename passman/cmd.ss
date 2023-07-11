(import :gerbil/gambit
        :std/sugar
        :std/iter
        :std/sort
        :std/text/json
        :std/misc/shuffle
        :std/misc/symbol
        :std/misc/ports
        :std/srfi/13
        :std/pregexp
        (prefix-in ./vault vault.))
(export create-vault
        add-to-vault!
        update-vault!
        get-entry
        find-entry
        delete-entry!
        dump-vault
        generate-password)

;;; vault creation
(def (create-vault path: path passphrase: pass)
  (when (file-exists? path)
    (error "vault already exists" path))
  (let* ((pass (or pass (get-new-passphrase)))
         (dir (path-directory path)))
    (unless (string-empty? dir)
      (create-directory* dir))
    (vault.create-vault path pass)
    (void)))

;;; adding stuff to the vault
(def (add-to-vault! key path: path passphrase: pass input: input json: json?)
  (unless (file-exists? path)
    (error "vault does not exist" path))
  (let* ((pass (or pass (get-passphrase)))
         (vault (vault.open-vault path pass))
         (input (or input (get-input)))
         (entries (if key
                    [(parse-entry key input json?)]
                    (parse-input input json?))))
    (vault.vault-add! vault entries)
    (vault.write-vault! vault)
    (void)))

(def (update-vault! key path: path passphrase: pass input: input json: json?)
  (unless (file-exists? path)
    (error "vault does not exist" path))
  (let* ((pass (or pass (get-passphrase)))
         (vault (vault.open-vault path pass))
         (input (or input (get-input)))
         (entries (if key
                    [(parse-entry key input json?)]
                    (parse-input input json?))))
    (vault.vault-update! vault entries)
    (vault.write-vault! vault)
    (void)))

;;; retrieving stuff from the vault
(def (get-entry key path: path passphrase: pass json: json?)
  (unless (file-exists? path)
    (error "vault does not exist" path))
  (let* ((pass (or pass (get-passphrase)))
         (vault (vault.open-vault path pass))
         (entries (vault.vault-get-fuzzy vault key)))
    (for (e entries)
      (if json?
        (write-json e)
        (write-entry e))
      (newline))))

(def (find-entry niddle path: path passphrase: pass regex: rx? json: json?)
  (unless (file-exists? path)
    (error "vault does not exist" path))
  (let* ((pass (or pass (get-passphrase)))
         (vault (vault.open-vault path pass))
         (search
          (if rx?
            (let (rx (pregexp niddle))
              (lambda (val)
                (pregexp-match rx val)))
            (lambda (val)
              (string-contains val niddle))))
         (entries (vault.vault-find vault search)))
    (for (e entries)
      (if json?
        (write-json e)
        (write-entry e))
      (newline))))

(def (delete-entry! key path: path passphrase: pass)
  (unless (file-exists? path)
    (error "vault does not exist" path))
  (let* ((pass (or pass (get-passphrase)))
         (vault (vault.open-vault path pass)))
    (unless (vault.vault-delete! vault key)
      (error "entry does not exist" key))
    (vault.write-vault! vault)))

;;; vault dump
(def (dump-vault path: path passphrase: pass json: json? confirm: confirm?)
  (let/cc return
    (unless (file-exists? path)
      (error "vault does not exist" path))
    (when confirm?
      (display "WARNING: this will dump the contexts of the vault in plain view. Really do it? [y/n]: ")
      (unless (member (read-line) '("y" "yes" "Y" "YES"))
        (return (void))))
    (let* ((pass (or pass (get-passphrase)))
           (vault (vault.open-vault path pass))
           (dump (if json? write-entry/json write-entry)))
      (for (e (vault.vault-entries vault))
        (dump e)
        (newline)))))

;;; entry I/O
(def (write-entry e)
  (displayln "---")
  (let (keys (sort (hash-keys e) symbol<?))
    (for (k keys)
      (displayln k ": " (hash-get e k)))))

(def (write-entry/json e)
  (write-json e)
  (newline))

(def (parse-input input json?)
  (if json?
    (parse-input/json input)
    (parse-entries input)))

(def (parse-input/json input)
  (let (inp (open-input-string input))
    (let lp ((entries []))
      (let (next (read-line inp))
        (if (eof-object? next)
          (reverse entries)
          (lp (cons (read-json next) entries)))))))

(def (parse-entries input)
  (let (inp (open-input-string input))
    (let lp ((entries []))
      (let (next (read-entry inp))
        (if (eof-object? next)
          (reverse entries)
          (lp (cons next entries)))))))

(def (parse-entry key input json?)
  (let* ((inp (open-input-string input))
         (entry (if json? (read-json inp) (read-entry inp))))
    (hash-put! entry 'key key)
    entry))

(def (read-entry inp)
  (let lp ((fields []))
    (let (next (read-line inp))
      (cond
       ((eof-object? next)
        (if (null? fields)
          next
          (list->hash-table-eq fields)))
       ((string-prefix? "-" next)
        (if (null? fields)
          (lp fields)
          (list->hash-table-eq fields)))
       ((string-empty? next)
        (lp fields))
       (else
        (let (colon (string-index next #\:))
          (if colon
            (let ((key (string->symbol (substring next 0 colon)))
                  (value (string-trim-both (substring next (1+ colon) (string-length next)))))
              (lp (cons (cons key value) fields)))
            (error "Malformed entry" next))))))))

;;; generic input
(def (get-input)
  (read-line (current-input-port) #f))

;;; passphrase input
(def (get-passphrase (prompt "Enter passphrase: "))
  (read-password prompt: prompt))

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
