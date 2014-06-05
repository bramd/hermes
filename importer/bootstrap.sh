#!/bin/sh

BASEDIR=$(dirname $0)
docker build -t hermes/importer $BASEDIR/docker
