#!/bin/sh

PROJECT=caradoc

set -e

usage () {
    [ -n "$1" ] && echo "Error: $1" >&2
    echo "Usage: ./mkdeb.sh version [commit]" >&2
    echo >&2
    echo "       version must correspond to an existing v<version> tag in git" >&2
    echo "       or a commit number must be provided." >&2
    echo >&2
    echo "       The script must be called from within the $PROJECT root directory" >&2
    exit 1
}


# Sanity check
if ! [ -d .git -a -d debian -a "$(dirname $0)" = "." ];
then usage "Please call mkdeb.sh from a correct git working copy";
fi


# Parameters
VERSION="$1"
if [ -n "$2" ]
then COMMIT="$2"
else COMMIT="v$PROJECT"
fi


# Directory preparation
TMPDIR=$(mktemp -d debian.XXXXXX)
echo "Using $TMPDIR directory" >&2
git archive --format tar.gz -o "$TMPDIR/${PROJECT}_$VERSION.orig.tar.gz" --prefix "${PROJECT}-$VERSION/" "$COMMIT"
cd "$TMPDIR"
tar xvzf "${PROJECT}_$VERSION.orig.tar.gz"
cd "${PROJECT}-$VERSION"


# Package construction
debuild -us -uc
