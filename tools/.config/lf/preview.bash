#!/usr/bin/env bash

set -e

path="$1"
# The original width goes to the edge of the terminal, so we subtract one to
# keep it inside lf's drawn boxes.
width="$(($2 - 1))"
height="$3"
x="$4"
y="$5"

icat() {
    kitty +kitten icat \
        --place "${width}x${height}@${x}x${y}" \
        --silent \
        --transfer-mode file \
        "$@"
}

# Check file extension
case $(echo "$path" | awk '{ print tolower($0) }') in
    *.tar | *.tar.gz | *.tar.xz)
        tar -tf "$path"
        exit 0
        ;;
esac

# Check mime type
case $(file --mime-type -bL "$path") in
    application/java-archive)
        unzip -l "$path"
        exit 0
        ;;
    application/json)
        jq . "$path"
        exit 0
        ;;
    application/pdf)
        pdftotext -l 5 -layout -q "$path" -
        exit 0
        ;;
    application/vnd.oasis.opendocument.*)
        odt2txt "$path"
        exit 0
        ;;
    application/x-tar)
        tar -tf "$path"
        exit 0
        ;;
    application/zip)
        unzip -l "$path"
        exit 0
        ;;
    image/*)
        icat "$path"
        exit 1
        ;;
    text/*)
        cat "$path"
        exit 0
        ;;
    video/*)
        ffmpegthumbnailer -i "$path" -s 0 -c jpeg -o - | icat --stdin yes
        exit 1
        ;;
esac

exit 1
