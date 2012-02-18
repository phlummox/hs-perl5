#!/usr/bin/perl -w

# Copyright (c) 2002 Mattia Barbon.
# Copyright (c) 2002 Audrey Tang.
# This package is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

use strict;
use FindBin;
use lib "$FindBin::Bin/../lib";
use File::Basename;
use Getopt::Long;

my $chunk_size = 0;
my $strip_pod = 0;
my $compress = 0;
my $name;

GetOptions(
    "c|chunk-size=i"    => \$chunk_size,
    "s|strip"           => \$strip_pod,
    "z|compress"        => \$compress,
    "n|name=s"          => \$name)
    && @ARGV == 3
        or die "Usage: $0 [-c CHUNK][-n NAME][-s][-z] file.pl file.c c_variable\n";
my ($pl_file, $c_file, $c_var) = @ARGV;
$name = basename($pl_file) unless defined $name;

my $pl_text = do           # a scalar reference
{
    open my $in, "<", $pl_file or die "open input file '$pl_file': $!";
    binmode $in;
    local $/ = undef;
    my $slurp = <$in>;
    close $in;
    \$slurp;
};

open my $out, ">", $c_file or die "open output file '$c_file': $!";
binmode $out;

my $len = length($$pl_text);

# add a NUL byte so that chunk_${c_var} may be used as C string
print_chunk( $$pl_text, "" );    

print $out <<"...";
const char * text_$c_var () { return((const char *)$c_var); }
const int size_$c_var () { return $len; }
...

close $out;

exit 0;


sub print_chunk 
{
    my ($chunk, $suffix) = @_;

    my $len = length($chunk);
    print $out <<"...";
static unsigned char ${c_var}[] = {
...

    for (my $i = 0; $i < $len; $i++) {
        printf $out "0x%02x,", ord(substr($chunk, $i, 1));
        print $out "\n" if $i % 16 == 15;
    }

    print $out "};\n";
}

# local variables:
# mode: cperl
# end:
