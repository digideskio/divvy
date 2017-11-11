#!/bin/bash

DIR=$(cd `dirname $0`; pwd)
cd $DIR

if ! sbt divvyJS/fastOptJS; then
    exit 1
fi

mkdir -p html/scripts
cp js/target/scala-2.10/*.js html/scripts/
