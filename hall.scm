(hall-description
 (name "config")
 (prefix "guile")
 (version "0.2")
 (author "Alex Sassmannshausen")
 (copyright (2016 2017 2018))
 (synopsis "Guile application configuration parsing library.")
 (description "Guile Config is a library providing a declarative approach to application configuration specification.  The library provides clean configuration declaration forms, and processors that take care of: configuration file creation; configuration file parsing; command-line parameter parsing using getopt-long; basic GNU command-line parameter generation (--help, --usage, --version); automatic output generation for the above command-line parameters.")
 (home-page
  "https://gitlab.com/guile-projects/guile-config")
 (license gpl3+)
 (dependencies `())
 (files (libraries
         ((scheme-file "config")
          (directory
           "config"
           ((directory "parser" ((scheme-file "sexp")))
            (scheme-file "api")
            (scheme-file "licenses")
            (scheme-file "records")
            (scheme-file "getopt-long")
            (scheme-file "helpers")))))
        (tests ((directory
                 "tests"
                 ((scheme-file "config")
                  (scheme-file "quickcheck-defs")
                  (scheme-file "quickcheck")))))
        (programs ((directory "scripts" ())))
        (documentation
         ((text-file "README")
          (text-file "HACKING")
          (text-file "COPYING")
          (directory "doc" ((texi-file "config")
                            (texi-file "fdl-1.3")))))
        (infrastructure
         ((scheme-file "guix") (scheme-file "hall")))))
