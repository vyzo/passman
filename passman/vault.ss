(import :gerbil/gambit
        :std/iter
        :std/crypto
        :std/text/json
        :std/text/utf8
        :std/misc/ports
        :std/srfi/1
        :std/srfi/13)
(export create-vault
        open-vault
        write-vault!
        vault-add!
        vault-update!
        vault-get
        vault-get-fuzzy
        vault-find
        vault-delete!
        vault? vault-entries)

;; the vault data structure
;; entries are the contents of the vault as a list of hash tables.
;; the vault is written encrypted as ndjson with a magic marker prefix
;; to indicate the vault version.
(defstruct vault (path iv pass entries))

;; create a new vault
(def (create-vault path pass)
  (let (vault (make-vault path (make-iv (cipher-iv-length (make-cipher))) pass []))
    (write-vault! vault)
    vault))

;; open an existing vault
(def (open-vault path pass)
  (let (vault (make-vault path #f pass []))
    (read-vault! vault)
    vault))

;; add some entries to a vault
(def (vault-add! vault entries)
  (for (e entries)
    (let (key (hash-get e 'key))
      (cond
       ((not key)
        (error "Missing entry key" (hash->list e)))
       ((vault-entry-exists? vault key)
        (error "Duplicate entry" key))
       (else
        (set! (vault-entries vault) (append! (vault-entries vault) [e])))))))

;; update existing entries in a vault; non existent entries will be added
(def (vault-update! vault entries)
  (for (e entries)
    (let (key (hash-get e 'key))
      (cond
       ((not key)
        (error "Missing entry key" (hash->list e)))
       ((vault-get vault key) =>
        (lambda (ee)
          (for ((values k v) (in-hash e))
            (hash-put! ee k v))))
       (else
        (set! (vault-entries vault) (append! (vault-entries vault) [e])))))))

;;; get an entry from the vault
(def (vault-get vault key)
  (let lp ((entries (vault-entries vault)))
    (match entries
      ([e . entries]
       (if (equal? (hash-get e 'key) key)
         e
         (lp entries)))
      (else #f))))

(def (vault-get-fuzzy vault key)
  (let lp ((entries (vault-entries vault)) (result []))
    (match entries
      ([e . entries]
       (if (string-contains (hash-get e 'key) key)
         (lp entries (cons e result))
         (lp entries result)))
      (else
       (reverse result)))))

(def (vault-find vault search)
  (let lp ((entries (vault-entries vault)) (result []))
    (match entries
      ([e . entries]
       (if (vault-entry-search e search)
         (lp entries (cons e result))
         (lp entries result)))
      (else
       (reverse result)))))

(def (vault-entry-search entry search)
  (let lp ((fields (hash->list entry)))
    (match fields
      ([[k . v] . fields]
       (if (search v)
         [k . v]
         (lp fields)))
      (else #f))))

(def (vault-entry-exists? vault key)
  (if (vault-get vault key) #t #f))

;;; entry deletion
(def (vault-delete! vault key)
  (and (vault-get vault key)
       (set! (vault-entries vault)
         (remf (lambda (e) (equal? (hash-get e 'key) key))
               (vault-entries vault)))))

;;; vault I/O
(def magic "%vault/v0%")

(def (make-cipher)
  (make-aes-256-cfb-cipher))

(def (write-vault! vault)
  (let ((tmp (string-append (vault-path vault) ".tmp"))  ; where to write, before moving
        (old (string-append (vault-path vault) ".old"))) ; where to save old contents
    (when (file-exists? tmp)
      (delete-file tmp))
    (let (buf (open-output-u8vector))
      (write-string magic buf)
      (newline buf)
      (for (e (vault-entries vault))
        (write-json e buf)
        (newline buf))
      (let* ((plaintext (get-output-u8vector buf))
             (cipher (make-cipher))
             (ciphertext
              (encrypt cipher
                       (passphrase->key (vault-pass vault) (cipher-key-length cipher))
                       (vault-iv vault)
                       plaintext)))
        (call-with-output-file [path: tmp permissions: #o600]
          (lambda (outp)
            (write-u8vector (vault-iv vault) outp)
            (write-u8vector ciphertext outp)))))
    (when (file-exists? old)
      (delete-file old))
    (when (file-exists? (vault-path vault))
      (rename-file (vault-path vault) old))
    (rename-file tmp (vault-path vault))))

(def (read-vault! vault)
  (let* ((ciphertext (read-file-u8vector (vault-path vault)))
         (cipher (make-cipher))
         (iv (subu8vector ciphertext 0 (cipher-iv-length cipher)))
         (ciphertext
          (subu8vector ciphertext (u8vector-length iv) (u8vector-length ciphertext)))
         (plaintext
          (decrypt cipher
                    (passphrase->key (vault-pass vault) (cipher-key-length cipher))
                    iv
                    ciphertext))
         (input (open-input-u8vector plaintext)))
    (let (input-magic (read-line input))
      (unless (equal? magic input-magic)
        (error "Bad magic" input-magic)))
    (set! (vault-iv vault) iv)
    (let lp ((es []))
      (let (e (read-json input))
        (if (eof-object? e)
          (set! (vault-entries vault) (reverse es))
          (lp (cons e es)))))))

(def salt "passman")
(def (passphrase->key pass size)
  (scrypt pass salt size))

(def (make-iv size)
  (let (iv (make-u8vector size))
    (random-bytes! iv)
    iv))
