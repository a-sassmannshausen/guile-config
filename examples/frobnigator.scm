(use-modules (config)
             (config api)
             (config parser sexp)
             (config licenses))

(define config
  (configuration
   (name 'frobnigator)
   (synopsis "The root configuration for frobnigator.")
   (keywords
    (list
     ;; A setting (either #t or #f) that can be passed as a command-line param
     ;; or set as a value in the configuration file.  In either case config
     ;; will ensure the value is either #t or #f.  Getopt-long will ensure in
     ;; command-line usage no value is specified.
     (setting
      (name 'verbose) (synopsis "Be chatty.")
      (default #f) (test boolean?))
     ;; A switch that can only be specified on the command-line.  It has no
     ;; short version, and a value must be specified, though it defaults to a
     ;; value.  The value is expected to be a white space delimited string
     ;; which is then turned into a file name using handler.
     (switch
      (name 'lines)
      (synopsis "Maximum lines to emit, or infinite if #f (default).")
      (default #f) (test (lambda (v) (or (not v) (number? v))))
      (example "36")
      (handler string->number))))
   (arguments
    (list
     (argument
      (name 'file) (optional? #f)
      (test file-exists?) (handler identity)
      (example "/tmp/foo"))))
   (subcommands
    (list
     ;; A very simple sub-configuration only inheriting the 'file positional
     ;; command-line argument.
     (configuration
      (name 'make)
      (synopsis "Configuration for the make sub-command.")
      (wanted '((arguments . (file)))))
     (configuration
      (name 'complex-inheritance)
      (alias 'cmp)
      (synopsis "We inherit everything")
      (wanted '((arguments . (file))
                (keywords . (verbose lines)))))))
   (directory (list (in-home ".frobnigator")
                    (path (given "/tmp/frobnigator/")
                          (eager? #t))))
   (parser sexp-parser)
   (copyright '(2015 2016))
   (version "0.1")
   (license agpl3+)
   (author "Alex Sassmannshausen")))

(define (main cmd-line)
  (let ((options (getopt-config-auto cmd-line config)))
    (emit-help options)))

(main (command-line))
