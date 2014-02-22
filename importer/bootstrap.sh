#!/bin/sh

BASEDIR=$(dirname $0)
docker build -rm -t hermes/importer $BASEDIR/docker
