#!/bin/sh

# This script is designed to replace $Revision$ with Git commit
# revision string.  In order to call this script, you must invoke the
# following command.
#
#     git config filter.revisioner.smudge "sh revisioner-embed.sh"

sed 's/\$Revision\$/$Revision: '`git log --oneline -n 1 .|cut -f1 -d' '`'$/'
