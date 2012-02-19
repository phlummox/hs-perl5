#!/usr/bin/env perl
exec "ulimit -t 30; ulimit -v 1260720; nice -20 ./perl6 @ARGV";
