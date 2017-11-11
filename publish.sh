#!/bin/sh

set -e

DIR=$(cd `dirname $0` && pwd)

cd "${DIR}"

git clean -fdx html
./build.sh

cd "${DIR}/html"
tar cvfz "${DIR}"/site.tar.gz ./

TEMP="/tmp/divvy.$$"

git clone git@github.com:zuercher/divvy.git "${TEMP}"
cd "${TEMP}"
git checkout --track origin/gh-pages

tar xvfz "${DIR}/site.tar.gz"

git add .
git commit -m "update: $(date)"
git push

cd ${DIR}
rm -rf ${TEMP}
