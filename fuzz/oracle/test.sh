#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -eu

pin=1372d04f79bde0daa3bab4b96a068baafa808e67
go_pin=go1.27.1

if [ "$#" -ne 1 ]; then
  echo "usage: $0 FZF_SOURCE" >&2
  exit 2
fi

source_dir=$(CDPATH= cd -- "$1" && pwd)
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

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

temp_dir=$(mktemp -d "${TMPDIR:-/tmp}/fzf-raw-oracle-test.XXXXXX")
trap 'rm -rf "$temp_dir"' EXIT HUP INT TERM

build_dir=$temp_dir/build
"$script_dir/prepare.sh" "$source_dir" "$build_dir"

cd "$build_dir"
GOWORK=off go test -count=1 -timeout=60s -mod=vendor -trimpath \
  -buildvcs=false .
GOWORK=off go vet -mod=vendor .

first=$temp_dir/oracle-first
second=$temp_dir/oracle-second
"$script_dir/build.sh" "$source_dir" "$first"
"$script_dir/build.sh" "$source_dir" "$second"
if ! cmp -s "$first" "$second"; then
  echo "two oracle builds from the same source differ" >&2
  cksum "$first" "$second" >&2
  exit 1
fi

if go version -m "$first" | grep -q '=>'; then
  echo "oracle module data contains a local replacement" >&2
  go version -m "$first" >&2
  exit 1
fi
