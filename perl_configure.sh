#!/usr/bin/env bash

set -ex

cc_options="$(perl -MExtUtils::Embed -e ccopts)"
ld_options="$(perl -MExtUtils::Embed -e ldopts)"
OUTFILE=my-perl5.buildinfo

echo > $OUTFILE
echo "cc-options: " $cc_options   >>  $OUTFILE
echo "ld-options: " $ld_options   >>  $OUTFILE

#echo "result buildinfo is: " >&2
#cat -n $OUTFILE >&2

