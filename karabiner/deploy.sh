#!/usr/bin/env bash
set -euo pipefail

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
SOURCE_DIR="$(cd -P "$( dirname "$SOURCE" )" && pwd)"

mkdir -p ~/.config/karabiner/assets/complex_modifications
ln -fs "$SOURCE_DIR/MyMappings.json"     ~/.config/karabiner/assets/complex_modifications/
ln -fs "$SOURCE_DIR/UnusedMappings.json" ~/.config/karabiner/assets/complex_modifications/
