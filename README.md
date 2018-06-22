# SPARQLing structural variation

More documentation is available in the `doc/` directory.

## Dependencies

To build the programs in this repository, the following tools are required:
- GNU Autoconf, GNU Automake, pkg-config and GNU Make
- HTSlib
- Libgcrypt
- Raptor2
- GNU Guile

## Installation and development environment with GNU Guix

The following command creates an environment wherein all required
tools to construct the project's programs are available:
```
$ guix environment -l environment.scm
```

Do the usual autotools dance to build everything:
```
[env]$ autoreconf -vfi
[env]$ ./configure
[env]$ make
```

Or change the last step to build individual components, like `vcf2rdf`:
```
[env]$ make -C tools/vcf2rdf
```

## Starting the web interface

To start the web interface, run:
```
[env]$ web/run
```

![Web interface screenshot](https://github.com/UMCUGenetics/sparqling-svs/blob/master/doc/figures/web-interface.png)

## Further reading

Please read the manual in the `doc/` directory.  A PDF can be built using:
```
$ guix environment -l environment.scm --ad-hoc texlive
[env]$ make doc
```
