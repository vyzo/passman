(import :gerbil/gambit
        :std/iter
        :std/crypto
        :std/text/json
        :std/text/utf8
        :std/misc/ports)
(export create-vault
        open-vault
        vault? vault-entries)

;; the vault data structure
;; entries are the contents of the vault as a list of hash tables.
;; the vault is written encrypted as ndjson with a magic marker prefix
;; to indicate the vault version.
(defstruct vault (path pass entries))

;; create a new vault
(def (create-vault path pass)
  (let (vault (make-vault path pass []))
    (write-vault! vault)
    vault))

;; open an existing vault
(def (open-vault path pass)
  (let (vault (make-vault path pass []))
    (read-vault! vault)
    vault))

;;; vault I/O
(def magic "%vault/v0%")

(def (make-cipher)
  (make-aes-256-cbc-cipher))

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
                       (passphrase->key (vault-pass vault))
                       (make-u8vector (cipher-iv-length cipher)) ; IV = 0...
                       plaintext)))
        (call-with-output-file [path: tmp permissions: #o600]
          (lambda (outp) (write-u8vector ciphertext outp)))))
    (when (file-exists? old)
      (delete-file old))
    (when (file-exists? (vault-path vault))
      (rename-file (vault-path vault) old))
    (rename-file tmp (vault-path vault))))

(def (read-vault! vault)
  (let* ((ciphertext (read-file-u8vector (vault-path vault)))
         (cipher (make-cipher))
         (plaintext
          (decrypt cipher
                    (passphrase->key (vault-pass vault))
                    (make-u8vector (cipher-iv-length cipher)) ; IV = 0 ...
                    ciphertext))
         (input (open-input-u8vector plaintext)))
    (let (input-magic (read-line input))
      (unless (equal? magic input-magic)
        (error "Bad magic" input-magic)))
    (let lp ((es []))
      (let (e (read-json input))
        (if (eof-object? e)
          (set! (vault-entries vault) (reverse es))
          (lp (cons e es)))))))

(def (passphrase->key pass)
  (sha256 (string->utf8 pass)))
