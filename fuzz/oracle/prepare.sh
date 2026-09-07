#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

set -eu

if [ "$#" -ne 2 ]; then
  echo "usage: $0 FZF_SOURCE BUILD_DIRECTORY" >&2
  exit 2
fi

source_dir=$(CDPATH= cd -- "$1" && pwd)
build_dir=$2
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
upstream_dir=$build_dir/upstream

mkdir -p "$build_dir" "$upstream_dir"
git -C "$source_dir" archive 1372d04f79bde0daa3bab4b96a068baafa808e67 |
  tar -x -C "$upstream_dir"

cp "$script_dir"/*.go "$script_dir/go.mod" "$build_dir/"
cp "$upstream_dir/go.sum" "$build_dir/go.sum"

cd "$build_dir"
go mod edit -replace="github.com/junegunn/fzf=$upstream_dir"
GOWORK=off go mod tidy
GOWORK=off go mod vendor

# The local replacement makes the build independent of the network. Remove
# its temporary path from the final module data after Go copies the source.
awk -v path="$upstream_dir" '
  $0 == "# github.com/junegunn/fzf => " path { next }
  index($0, "# github.com/junegunn/fzf ") == 1 {
    suffix = " => " path
    if (length($0) >= length(suffix) &&
        substr($0, length($0) - length(suffix) + 1) == suffix) {
      print substr($0, 1, length($0) - length(suffix))
      next
    }
  }
  { print }
' vendor/modules.txt >vendor/modules.txt.new
mv vendor/modules.txt.new vendor/modules.txt
go mod edit -dropreplace=github.com/junegunn/fzf
