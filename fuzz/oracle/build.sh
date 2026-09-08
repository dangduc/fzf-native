#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -eu

pin=1372d04f79bde0daa3bab4b96a068baafa808e67
go_pin=go1.27.1

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo "usage: $0 FZF_SOURCE [OUTPUT]" >&2
  exit 2
fi

source_dir=$(CDPATH= cd -- "$1" && pwd)
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
output=${2:-"$script_dir/fzf-raw-oracle"}
case "$output" in
  /*) ;;
  *) output="$PWD/$output" ;;
esac

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

temp_dir=$(mktemp -d "${TMPDIR:-/tmp}/fzf-raw-oracle.XXXXXX")
trap 'rm -rf "$temp_dir"' EXIT HUP INT TERM

build_dir=$temp_dir/build
"$script_dir/prepare.sh" "$source_dir" "$build_dir"

cd "$build_dir"
GOWORK=off go build -mod=vendor -trimpath -buildvcs=false \
  -ldflags=-buildid= -o "$output" .
