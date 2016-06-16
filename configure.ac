dnl -*- Autoconf -*-

AC_INIT(Guile-Config, 0.2)
AC_CONFIG_SRCDIR(conf)
AC_CONFIG_AUX_DIR([build-aux])
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.19.7])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile
                 po/Makefile.in
                 conf/spec.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env], [chmod +x test-env])

GUILE_PROGS([2.0.11])

AC_OUTPUT
