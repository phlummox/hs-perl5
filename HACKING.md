
# Hacking hs-perl5

## Useful Perl FFI documentation

["How to embed perl in your C program"][perlembed]

* a good starting point on the Perl C API

["perlguts -- Introduction to the Perl API"][perlguts]

* more detail

["Perl API"][perlapi]

* reference documentation

[perlembed]: https://perldoc.perl.org/perlembed
[perlguts]: https://perldoc.perl.org/perlguts
[perlapi]: https://perldoc.perl.org/perlapi

## C source files

`cbits/p5embed.c` largely contains thin wrappers around Perl C API functions
(documented in <https://perldoc.perl.org/perlapi>), adding a `perl5_` namespace to them.

`cbits/p5embed.h`, unsurprisingly, contains prototypes for the functions in
`cbits/p5embed.c`.

`cbits/perlxsi.h` contains an `xs_init()` function, required for the Perl interpreter
to be initialized.

