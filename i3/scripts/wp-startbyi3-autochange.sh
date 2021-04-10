#!/bin/bash

# if ! mkdir ~/.config/i3/scripts/wp-autochange.lock; then
#     printf "Failed to acquire lock.\n" >&2
#     exit 1
# fi
# trap 'rm -rf ~/.config/i3/scripts/wp-autochange.lock' EXIT

flock -n ~/.config/i3/scripts/wp-autochange.lock -c "/bin/bash ~/.config/i3/scripts/wp-autochange.sh" &
