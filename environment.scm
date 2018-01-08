;; Copyright (C) 2017, 2018  Roel Janssen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages bioinformatics)
             (gnu packages compression)
             (gnu packages curl)
             (gnu packages gnupg)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages statistics))

(define vcf2turtle
  (package
   (name "vcf2turtle")
   (version "0.0.1")
   (source #f)
   (build-system gnu-build-system)
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("htslib" ,htslib)
      ("libgcrypt" ,libgcrypt)))
   (home-page "")
   (synopsis "Data transformer from VCF to Turtle")
   (description "The vcf2turtle program takes Variant Call Format input and
outputs it in Turtle-format.")
   (license license:gpl3+)))

(define turtle2remote
  (package
   (name "turtle2remote")
   (version "0.0.1")
   (source #f)
   (build-system gnu-build-system)
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("curl" ,curl)))
   (home-page "")
   (synopsis "Turtle data uploader to a SPARQL endpoint")
   (description "The turtle2remote program takes Turtle-formatted input and
submits it to a specified SPARQL endpoint.")
   (license license:gpl3+)))

(define svplot
  (package
   (name "svplot")
   (version "0.0.1")
   (source #f)
   (build-system gnu-build-system)
   (inputs
    `(("r" ,r)))
   (propagated-inputs
    `(("r-ggplot2" ,r-ggplot2)
      ("r-sparql" ,r-sparql)))
   (home-page "")
   (synopsis "Plotting package for this project.")
   (description "Plotting package for this project.")
   (license license:gpl3+)))

(define sparqling-svs-web
  (package
   (name "sparqling-svs-web")
   (version "0.0.1")
   (source #f)
   (build-system gnu-build-system)
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("guile" ,guile-2.2)
      ("guile-commonmark" ,guile-commonmark)
      ("guile-json" ,guile-json)))
   (home-page "")
   (synopsis "")
   (description "")
   (license license:gpl3+)))

(define sparqling-svs
  (package
   (name "sparqling-svs")
   (version "0.0.1")
   (source #f)
   (build-system gnu-build-system)
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)))
   (inputs
    `(("pkg-config" ,pkg-config)
      ("zlib" ,zlib)
      ("xz" ,xz)
      ,@(package-inputs vcf2turtle)
      ,@(package-inputs turtle2remote)
      ,@(package-inputs sparqling-svs-web)
      ,@(package-inputs svplot)))
   (propagated-inputs
    `(,@(package-propagated-inputs svplot)))
   (home-page "")
   (synopsis "")
   (description "")
   (license license:gpl3+)))

;; Evaluate to the complete recipe, so that the development
;; environment has everything to start from scratch.
sparqling-svs
