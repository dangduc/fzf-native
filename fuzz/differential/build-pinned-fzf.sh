#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -eu

pin=1372d04f79bde0daa3bab4b96a068baafa808e67
go_pin=go1.27.1

if [ "$#" -ne 2 ]; then
  echo "usage: $0 FZF_SOURCE OUTPUT" >&2
  exit 2
fi

source_dir=$1
output=$2
source_commit=$(git -C "$source_dir" rev-parse HEAD)

if [ "$source_commit" != "$pin" ]; then
  echo "fzf source is at $source_commit; expected $pin" >&2
  exit 2
fi

go_version=$(go env GOVERSION)
if [ "$go_version" != "$go_pin" ]; then
  echo "Go version is $go_version; expected $go_pin" >&2
  exit 2
fi

case "$output" in
  /*) ;;
  *) output="$PWD/$output" ;;
esac

temp_dir=$(mktemp -d "${TMPDIR:-/tmp}/fzf-pinned-cli.XXXXXX")
trap 'rm -rf "$temp_dir"' EXIT HUP INT TERM

git -C "$source_dir" archive "$pin" | tar -x -C "$temp_dir"
cd "$temp_dir"
GOWORK=off go build -mod=readonly -trimpath \
  -ldflags "-X main.revision=$pin" -o "$output" .
