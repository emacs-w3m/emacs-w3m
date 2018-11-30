#!/bin/sh

# This script is designed to clear Git commit revision string which
# was embedded by revisioner-embed.sh.  In order to call this script,
# you must invoke the following command.
#
#     git config filter.revisioner.clean "sh revisioner-clean.sh"

sed 's/\$Revision:.*\$/$Revision$/'
