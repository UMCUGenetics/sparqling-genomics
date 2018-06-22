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
             (gnu packages statistics)
             (gnu packages rdf))

(define vcf2rdf
  (package
   (name "vcf2rdf")
   (version "0.0.1")
   (source #f)
   (build-system gnu-build-system)
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("htslib" ,htslib)
      ("libgcrypt" ,libgcrypt)
      ("raptor" ,raptor2)))
   (home-page "https://github.com/UMCUGenetics/sparqling-svs")
   (synopsis "Data transformer from VCF to RDF")
   (description "The vcf2rdf program takes Variant Call Format input and
outputs it in various RDF formats.  It uses Raptor2 for serialization.")
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
      ("raptor2" ,raptor2)
      ("redland" ,redland)
      ("rasqal" ,rasqal)))
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
      ,@(package-inputs vcf2rdf)
      ,@(package-inputs sparqling-svs-web)))
   (home-page "https://github.com/UMCUGenetics/sparqling-svs")
   (synopsis "Tools to use SPARQL to analyze genomic structural variation")
   (description "This package provides various tools to extract RDF triples
from genomic data formats.")
   (license license:gpl3+)))

;; Evaluate to the complete recipe, so that the development
;; environment has everything to start from scratch.
sparqling-svs
