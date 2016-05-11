;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2015 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 12 July 2015
;;
;; This file is part of Guile-Config.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  ./bootstrap && ./configure && make check
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix download)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages gettext)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(package
  (name "guile-config")
  (version "0.1.1")
  (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://alex.pompo.co/software/" name "-" version
                  ".tar.gz"))
            ;; SHA is always out of date.
            (sha256
             "1b719bn192f9wg24rr0zx8jpmygsvyhfi35iy778pb5p392snrn8")))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("gettext" ,gnu-gettext)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs
   `(("guile" ,guile-2.0)))
  (synopsis "Guile application configuration parsing library.")
  (description "Guile-config is a library enhancing (ice-9 getopt-long).")
  (home-page "https://gitlab.com/guile-projects/guile-config")
  (license agpl3+))

;;; guix.scm ends here.
