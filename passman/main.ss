(export main)
(import :std/sugar
        :std/getopt
        :gerbil/gambit/exceptions
        ./cmd)

(def (implement-me! what)
  (error "IMPLEMENT ME" what))

(def (main . args)
  ;; common getopt thingies
  (def path-option
    (option 'path "-p" "--path"
            value: path-expand
            default: (path-expand "~/.passman/default.vault")
            help: "path to the vault file"))
  (def passphrase-option
    (option 'passphrase #f "--passphrase"
            help: "specify the passphrase in the command line instead of asking for it"))
  (def input-option
    (option 'input #f "--input"
            help: "specify input non interactively"))
  (def key-arg
    (argument 'key
              value: string->symbol
              help: "the entry's key"))
  (def json-flag
    (flag 'json "-j" "--json"
          help: "output in json"))

  ;; commands
  (def create-cmd
    (command 'create
             path-option passphrase-option
             help: "create a new vault"))
  (def add-cmd
    (command 'add
             key-arg json-flag input-option path-option passphrase-option
             help: "add a new entry to the vault"))
  (def update-cmd
    (command 'update
             key-arg json-flag input-option path-option passphrase-option
             help: "update an existing entry in the vault"))
  (def get-cmd
    (command 'get
             key-arg json-flag path-option passphrase-option
             help: "get an entry from the vault"))
  (def search-cmd
    (command 'search
             path-option passphrase-option
             (argument 'key
                       help: "the key to search in; can be a comma separated list or * for all keys")
             (argument 'niddle
                       help: "the value to match; can optionally be a regex")
             (flag 'regex "-r"
                   help: "the niddle is a regular expression")
             help: "search for an entry using regular expressions"))
  (def dump-cmd
    (command 'dump
             path-option passphrase-option
             (flag 'yes "-y" "--yes"
                   help: "don't ask for confirmation, just do it!")
             help: "dump the contents of the vault"))
  (def genpass-cmd
    (command 'generate
             (option 'lowercase "-l"
                     value: string->number
                     default: 1
                     help: "minimum number of lowercase chars")
             (option 'uppercase "-u"
                     value: string->number
                     default: 1
                     help: "minimum number of uppercase chars")
             (option 'digit "-d"
                     value: string->number
                     default: 1
                     help: "minimum number of digits")
             (option 'special "-s"
                     value: string->number
                     default: 1
                     help: "minimum number of special chars")
             (option 'length "-c"
                     value: string->number
                     default: 12
                     help: "password length")
             help: "generate a random password with common rules"))
  (def help-cmd
    (command 'help
             (optional-argument 'command value: string->symbol)
             help: "display help; help <command> for command help"))

  ;; the driver
  (def gopt
    (getopt create-cmd
            add-cmd
            update-cmd
            get-cmd
            search-cmd
            dump-cmd
            genpass-cmd
            help-cmd))

  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (let-hash opt
       (case cmd
         ((create)
          ;; TODO
          (implement-me! 'create)
          )
         ((add)
          ;; TODO
          (implement-me! 'add)
          )
         ((update)
          ;; TODO
          (implement-me! 'update)
          )
         ((search)
          ;; TODO
          (implement-me! 'search)
          )
         ((dump)
          ;; TODO
          (implement-me! 'dump)
          )
         ((generate)
          (let (passwd (generate-password .length
                                          lowercase: .lowercase
                                          uppercase: .uppercase
                                          digit:     .digit
                                          special:   .special))
            (displayln passwd)))
         ((help)
          (getopt-display-help-topic gopt .?command "gxkpg")))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "passman" (current-error-port))
     (exit 1))
   (catch (e)
     (display-exception e (current-error-port))
     (exit 2))))
