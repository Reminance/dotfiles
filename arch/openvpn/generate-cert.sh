##!/bin/bash

OVPN_FILE="$1"

get_crt() {
        local file_name="$1"
        local ca_file="${file_name}-ca.crt"
        start_line=`grep -n '<ca>' "$file_name" |awk -F':' '{ print $1 }'`
        end_line=`grep -n '</ca>' "$file_name" |awk -F':' '{ print $1 }'`
        end_line=`expr $end_line - 1`
        count=`expr $end_line - $start_line`
        head -$end_line "$file_name" |tail -$count > "$ca_file"
        echo "$ca_file"
}

ca_file=`get_crt "$OVPN_FILE"`
