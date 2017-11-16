# SPARQLing structural variation

Extended documentation is available in the `doc/` directory.

## Installation and development environment

Create a development environment.  The following command creates
an environment wherein all required tools to construct the project's
programs are available:
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

To start the web interface, do:
```
[env]$ sh web/run.sh
```

## Further reading

Please read the manual in the `doc/` directory.  A PDF can be built using:
```
$ guix environment -l guix.scm --ad-hoc texlive
[env]$ make doc
```
