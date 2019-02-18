(hall-description
  (name "config")
  (prefix "guile")
  (version "0.3")
  (author "Alex Sassmannshausen")
  (copyright (2016 2017 2018))
  (synopsis
    "Guile application configuration parsing library.")
  (description
    "Guile Config is a library providing a declarative approach to application configuration specification.  The library provides clean configuration declaration forms, and processors that take care of: configuration file creation; configuration file parsing; command-line parameter parsing using getopt-long; basic GNU command-line parameter generation (--help, --usage, --version); automatic output generation for the above command-line parameters.")
  (home-page
    "https://gitlab.com/a-sassmannshausen/guile-config")
  (license gpl3+)
  (dependencies `())
  (files (libraries
           ((scheme-file "config")
            (directory
              "config"
              ((directory "parser" ((scheme-file "sexp")))
               (scheme-file "getopt-long")
               (scheme-file "records")
               (scheme-file "api")
               (scheme-file "licenses")
               (scheme-file "helpers")))))
         (tests ((directory
                   "tests"
                   ((scheme-file "getopt-long")
                    (scheme-file "quickcheck")
                    (scheme-file "quickcheck-defs")
                    (scheme-file "config")))))
         (programs ((directory "scripts" ())))
         (documentation
           ((text-file "README")
            (text-file "HACKING")
            (text-file "COPYING")
            (text-file "NEWS")
            (text-file "ChangeLog")
            (text-file "AUTHORS")
            (directory
              "doc"
              ((texi-file "config") (texi-file "fdl-1.3")))
            (directory
              "examples"
              ((scheme-file "simple")
               (scheme-file "frobnigator")
               (scheme-file "hello-world")))
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix")
            (scheme-file "hall")))))
