#!/usr/bin/bash

noteFilename="$HOME/workspace/note/note/note-$(date +%Y-%m-%d).md"

if [ ! -f $noteFilename ]; then
    echo "# Notes for $(date +%Y-%m-%d)" > $noteFilename
fi

/home/xc/.config/sandbox/nvim-nightly/nvim-linux64/bin/nvim -c "norm Go" \
    -c "norm Go## $(date +%H:%M)" \
    -c "norm G2o" \
    -c "norm zz" \
    -c "startinsert" $noteFilename
