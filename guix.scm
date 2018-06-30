;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2015-2018 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 12 July 2015
;;
;; This file is part of Guile-Config.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;; To install it locally from the checkout, run:
;;
;;  # in a guix environment
;;  ./bootstrap && ./configure && make distcheck
;;  # outside the guix environment
;;  guix package -f guix.scm
;;
;;; Code:

(use-modules
 (guix packages)
 (guix licenses)
 (guix download)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages pkg-config)
 (gnu packages texinfo))

(package
  (name "guile-config")
  (version "0.3")
  (source "./guile-config-0.3.tar.gz")
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-2.2)))
  (propagated-inputs `())
  (synopsis
   "Guile application configuration parsing library.")
  (description
   "Guile Config is a library providing a declarative approach to application configuration specification.  The library provides clean configuration declaration forms, and processors that take care of: configuration file creation; configuration file parsing; command-line parameter parsing using getopt-long; basic GNU command-line parameter generation (--help, --usage, --version); automatic output generation for the above command-line parameters.")
  (home-page
   "https://gitlab.com/a-sassmannshausen/guile-config")
  (license gpl3+))
