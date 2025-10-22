#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: git-ignore [-t [NAME]] [PATTERN...]

With no arguments, display the current repository's .gitignore using bat (or cat).

Options:
  -t [NAME]   Replace the repo's .gitignore with NAME.gitignore from $MYTEMPLATES.
              If NAME is omitted, fzf will be used to choose a template.

When PATTERN arguments are provided, append them to .gitignore (if absent) and
remove any tracked files that match the pattern from the index.
USAGE
}

command_exists() {
  command -v "$1" >/dev/null 2>&1
}

main() {
  local copied_template=false
  if [[ ${1:-} == "-h" || ${1:-} == "--help" ]]; then
    usage
    exit 0
  fi

  if ! command_exists git; then
    echo "git-ignore: git is required" >&2
    exit 1
  fi

  if ! git_root=$(git rev-parse --show-toplevel 2>/dev/null); then
    echo "git-ignore: not inside a git repository" >&2
    exit 1
  fi

  local gitignore="$git_root/.gitignore"

  if [[ $# -eq 0 ]]; then
    if [[ -f "$gitignore" ]]; then
      if command_exists bat; then
        bat --paging=never "$gitignore"
      else
        cat "$gitignore"
      fi
    else
      echo "git-ignore: no .gitignore found in repository" >&2
      exit 1
    fi
    exit 0
  fi

  if [[ ${1:-} == "-t" ]]; then
    shift
    local templates_dir=${MYTEMPLATES:-}
    if [[ -z "$templates_dir" ]]; then
      echo "git-ignore: MYTEMPLATES is not set" >&2
      exit 1
    fi
    if [[ ! -d "$templates_dir" ]]; then
      echo "git-ignore: template directory not found: $templates_dir" >&2
      exit 1
    fi

    local template_path
    if [[ $# -gt 0 && -n ${1:-} ]]; then
      template_path="$templates_dir/$1.gitignore"
      shift
    else
      if ! command_exists fzf; then
        echo "git-ignore: fzf is required when no template name is provided" >&2
        exit 1
      fi
      mapfile -t candidates < <(find "$templates_dir" -type f -name '*.gitignore' -print)
      if [[ ${#candidates[@]} -eq 0 ]]; then
        echo "git-ignore: no .gitignore templates found in $templates_dir" >&2
        exit 1
      fi
      template_path="$templates_dir/$(ls -1 $templates_dir | fzf)"
      if [[ -z "$template_path" ]]; then
        echo "git-ignore: template selection cancelled" >&2
        exit 1
      fi
    fi

    if [[ ! -f "$template_path" ]]; then
      echo "git-ignore: template not found: $template_path" >&2
      exit 1
    fi

    mkdir -p "$(dirname "$gitignore")"
    cp "$template_path" "$gitignore"
    echo "git-ignore: copied template $(basename "$template_path") to $gitignore"
    copied_template=true
    if [[ $# -eq 0 ]]; then
      exit 0
    fi
  fi

  mkdir -p "$(dirname "$gitignore")"
  touch "$gitignore"

  local pattern
  for pattern in "$@"; do
    if [[ -z "$pattern" ]]; then
      continue
    fi
    if ! grep -Fxq "$pattern" "$gitignore"; then
      printf '%s\n' "$pattern" >> "$gitignore"
    fi
    if git ls-files --error-unmatch -- "$pattern" >/dev/null 2>&1; then
      git rm --cached -r --ignore-unmatch -- "$pattern"
    fi
  done

  if [[ $copied_template == true ]]; then
    echo "git-ignore: appended patterns to copied template"
  else
    echo "git-ignore: updated $gitignore"
  fi
}

main "$@"
