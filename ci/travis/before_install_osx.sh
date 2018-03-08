#!/bin/sh
set -e

brew install openssl libffi
PKG_CONFIG_PATH+=:/usr/local/opt/openssl/lib/pkgconfig
PKG_CONFIG_PATH+=:/usr/local/opt/libffi/lib/pkgconfig
export PKG_CONFIG_PATH
