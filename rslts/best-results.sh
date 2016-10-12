ls *.log | xargs tail -n 1  | awk 'match($0, /^==> mut([^-]+)-([^.]+)/, ary) {printf "%s\t%s\t", ary[2], ary[1]}; /\s+0/ {print $2}'
