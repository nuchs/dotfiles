#!/bin/bash

function usage {
  echo "Runs the specified command anytime a file under the current directory"
  echo "changes. Ignores the .gitignore file, .git directory & node modules"
  echo "directory."
  echo ""
  echo "Usage: $0 [-h] <command>"
}

while getopts "h" opt; do
  case $opt in
    h)
      usage 0
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      usage 1
      ;;
  esac
done

shift $((OPTIND - 1))

inotifywait -mre CLOSE_WRITE --exclude ".git.*|node_modules"  . | while read -r filename; do
  echo "$filename changed, running $@"
  $@
done
