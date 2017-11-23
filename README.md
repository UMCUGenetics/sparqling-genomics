# SPARQLing structural variation

More documentation is available in the `doc/` directory.

## Dependencies

The programs in this repository require:
- GNU Autoconf, GNU Automake, pkg-config and GNU Make
- HTSlib
- Libgcrypt
- Libcurl
- GNU Guile, guile-commonmark, and guile-json
- R, ggplot2, and r-sparql

## Installation and development environment with GNU Guix

The following command creates an environment wherein all required
tools to construct the project's programs are available:
```
$ guix environment -l guix.scm
```

Do the usual autotools dance to build everything:
```
[env]$ autoreconf -vfi
[env]$ ./configure
[env]$ make
```

Or change the last step to build individual components, like `vcf2turtle`:
```
[env]$ make -C tools/vcf2turtle
```

## Starting the web interface

To start the web interface, run:
```
[env]$ sh web/run.sh
```

## Further reading

Please read the manual in the `doc/` directory.  A PDF can be built using:
```
$ guix environment -l guix.scm --ad-hoc texlive
[env]$ make doc
```
