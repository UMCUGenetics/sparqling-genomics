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
             (gnu packages autotools)
             (gnu packages bioinformatics)
             (gnu packages compression)
             (gnu packages curl)
             (gnu packages gnupg)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages rdf)
             (gnu packages tls)
             (gnu packages tex)
             (gnu packages)
             (guix build utils)
             (guix build-system gnu)
             (guix download)
             (guix git-download)
             (guix packages)
             (ice-9 format)
             (ice-9 rdelim))

(define sparqling-genomics
  (package
   (name "sparqling-genomics")
   (version "0.99.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/sparqling-genomics/"
                  "releases/download/" version "/sparqling-genomics-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1lmjvglbjiq4n9a56ic0kwwwip3y1f6wsksdjylf5hggaf5bhmpr"))))
   (build-system gnu-build-system)
   (arguments
    `(#:parallel-build? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'setup-static-resources
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out        (assoc-ref outputs "out"))
                   (web-root   (string-append
                                out "/share/sparqing-genomics/sg-web"))
                   (static-dir (string-append web-root "/static")))
              (mkdir-p static-dir)
              (copy-recursively "web/static" static-dir))))
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (guile-load-path
                    (string-append out "/share/guile/site/2.2:"
                                   (getenv "GUILE_LOAD_PATH")))
                   (guile-load-compiled-path
                    (string-append out "/lib/guile/2.2/site-ccache:"
                                   (getenv "GUILE_LOAD_COMPILED_PATH")))
                   (web-root (string-append
                              out "/share/sparqing-genomics/sg-web")))
              (wrap-program (string-append out "/bin/sg-web")
                `("GUILE_LOAD_PATH" ":" prefix (,guile-load-path))
                `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                  (,guile-load-compiled-path))
                `("SG_WEB_ROOT" ":" prefix (,web-root)))))))))
   (native-inputs
    `(("texlive" ,texlive)))
   (inputs
    `(("guile" ,guile-2.2)
      ("guile-fibers" ,guile-fibers)
      ("htslib" ,htslib)
      ("libgcrypt" ,libgcrypt)
      ("pkg-config" ,pkg-config)
      ("raptor2" ,raptor2)
      ("xz" ,xz)
      ("zlib" ,zlib)))
   (propagated-inputs
    `(("gnutls" ,gnutls))) ; Needed to query HTTPS endpoints.
   (home-page "https://github.com/UMCUGenetics/sparqling-genomics")
   (synopsis "Tools to use SPARQL to analyze genomics data")
   (description "This package provides various tools to extract RDF triples
from genomic data formats, and a web interface to query SPARQL endpoints.")
   (license license:gpl3+)))

;; Evaluate to the complete recipe, so that the development
;; environment has everything to start from scratch.
sparqling-genomics
