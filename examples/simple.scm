(use-modules (config)
             (config api))

(define config
  (configuration
   (name 'frobnigator)
   (synopsis "The root configuration for frobnigator.")
   (keywords
    (list
     (switch
      (name 'log-level)
      (synopsis "The verbosity of the command.")
      ;; Explicit short form.
      (character #\l)
      (example "3")
      (default 1)
      (test integer?)
      (handler string->number))
     (switch
      ;; Implicit short form, defaults to first character of name.
      (name 'silent)
      (synopsis "Should we not emit to stdout?")
      ;; Boolean flag
      (default #f) (test boolean?))))))

(define (main cmd-line)
  (let ((options (getopt-config-auto cmd-line config)))
    (emit-help options)))

(main (command-line))
