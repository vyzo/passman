#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '((exe: "passman/main" bin: "passman")
    "passman/cmd"
    "passman/vault"
    ))
