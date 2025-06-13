#!/bin/bash

# Find all greger-tools-execute calls that need fixing
echo "Finding calls that need to be fixed:"
grep -n "greger-tools-execute \"" test/test-greger-tools.el | while read line; do
    echo "$line"
done
