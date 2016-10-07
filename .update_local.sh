#!/bin/bash
# locally update (but not commit, push etc.) the gh-pages branch files from docs on the master

# create a temporary directory
TMPDIR=`mktemp -dp /tmp`

# checkout master
git checkout master

# copy docs to the directory
cp -R ./docs/* $TMPDIR/

# checkout gh-pages
git checkout gh-pages

# copy the contents of docs back here
cp -r ${TMPDIR}/* ./

# delete the temporary directory
rm -rf $TMPDIR

