(import :std/test
        :std/sugar
        :std/os/temporaries
        :std/srfi/13
        ./vault)
(export vault-test)

(def (entry-equal? x y)
  (and (hash-table? x)
       (hash-table? y)
       (= (hash-length x) (hash-length y))
       (let lp ((keys (hash-keys x)))
         (match keys
           ([k . rest]
            (and (equal? (hash-get x k) (hash-get y k))
                 (lp rest)))
           (else #t)))))

(def (entry-list-equal? x y)
  (and (list? x)
       (list? y)
       (andmap entry-equal? x y)))

(def vault-test
  (test-suite "test vault functionality"
    (test-case "test create and open"
      (call-with-temporary-file-name "vault"
       (lambda (tmp)
         (let* ((_ (create-vault tmp "123"))
                (vault (open-vault tmp "123"))
                (entries (vault-entries vault)))
           (check entries ? null?)))))

    (test-case "test CRUD"
      (call-with-temporary-file-name "vault"
       (lambda (tmp)
         (let* ((_ (create-vault tmp "123"))
                (vault (open-vault tmp "123")))
           (vault-add! vault [(hash (key "foo")) (hash (key "bar"))])
           (check (length (vault-entries vault)) => 2)
           (checkf entry-equal? (vault-get vault  "foo") (hash (key "foo")))
           (checkf entry-equal? (vault-get vault "bar") (hash (key "bar")))
           (vault-update! vault [(hash (key "foo") (stuff "moo!")) (hash (key "bar") (stuff "mooooo!"))])
           (check (length (vault-entries vault)) => 2)
           (checkf entry-equal? (vault-get vault "foo") (hash (key "foo") (stuff "moo!")))
           (checkf entry-equal? (vault-get vault "bar") (hash (key "bar") (stuff "mooooo!")))
           (write-vault! vault))
         (let (vault (open-vault tmp "123"))
           (check (length (vault-entries vault)) => 2)
           (checkf entry-equal? (vault-get vault "foo") (hash (key "foo") (stuff "moo!")))
           (checkf entry-equal? (vault-get vault "bar") (hash (key "bar") (stuff "mooooo!")))
           (vault-delete! vault "foo")
           (check (length (vault-entries vault)) => 1)
           (checkf entry-equal? (vault-get  vault "bar") (hash (key "bar") (stuff "mooooo!")))
           (write-vault! vault))
         (let (vault (open-vault tmp "123"))
           (check (length (vault-entries vault)) => 1)
           (checkf entry-equal? (vault-get vault "bar") (hash (key "bar") (stuff "mooooo!")))
           (checkf entry-list-equal? (vault-get-fuzzy vault "ba") [(hash (key "bar") (stuff "mooooo!"))])
           (checkf entry-list-equal? (vault-get-fuzzy vault "ar") [(hash (key "bar") (stuff "mooooo!"))])
           (checkf entry-list-equal? (vault-find vault (lambda (val) (string-contains val "oo"))) [(hash (key "bar") (stuff "mooooo!"))])))))
    ))
