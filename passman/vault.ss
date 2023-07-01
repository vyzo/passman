(import :gerbil/gambit
        :std/iter
        :std/crypto
        :std/text/json
        :std/text/utf8)
(export create-vault
        vault? vault-entries)

;; the vault data structure
;; entries are the contents of the vault as a list of hash tables.
;; the vault is written encrypted as ndjson with a magic marker prefix
;; to indicate the vault version.
(defstruct vault (path pass entries))

;; create a new vault
(def (create-vault path pass)
  (let (vault (make-vault path pass []))
    (write-vault vault)
    vault))

;;; vault I/O
(def magic "%vault/v0%")

(def (write-vault vault)
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
      (let* ((cipher (make-aes-256-gcm-cipher))
             (encrypted
              (encrypt cipher                                    ; AES256/GCM
                       (passphrase->key (vault-pass vault))      ; key
                       (make-u8vector (cipher-iv-length cipher)) ; IV = 0...
                       (get-output-u8vector buf))))              ; plaintext
             (call-with-output-file [path: tmp permissions: #o600]
               (lambda (outp) (write-u8vector encrypted outp)))))
    (when (file-exists? old)
      (delete-file old))
    (when (file-exists? (vault-path vault))
      (rename-file (vault-path vault) old))
    (rename-file tmp (vault-path vault))))

(def (passphrase->key pass)
  (sha256 (string->utf8 pass)))
