#!/bin/bash
set -e

mins="$1"
for ((i=1; i<=$mins; i++))
do
    echo "$i min(s)..."
    sleep 60
done