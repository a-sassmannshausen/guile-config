## Guile-Config --- GNU Guile application cnofiguration
## Copyright © 2015 Alex Sassmannshausen <alex@pompo.co>
##
## This file is part of Guile-Config.
##
## Guile-Config is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Guile-Config is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-Config.  If not, see <http://www.gnu.org/licenses/>.

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES =					\
  config.scm					\
  config/api.scm				\
  config/getopt-long.scm			\
  config/helpers.scm				\
  config/licenses.scm				\
  config/parser/sexp.scm			\
  config/records.scm

TESTS =

TEST_EXTENSIONS = .scm
AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"
SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

info_TEXINFOS = doc/config.texi
dvi: # Don't build dvi docs

EXTRA_DIST +=					\
  pre-inst-env.in				\
  test-env.in					\
  README					\
  ChangeLog					\
  AUTHORS					\
  NEWS						\
  $(TESTS)

ACLOCAL_AMFLAGS = -I m4

gen-ChangeLog:
	$(AM_V_GEN)if test -d .git; then		\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;				\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

dist-hook: gen-ChangeLog
.PHONY: gen-ChangeLog

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
