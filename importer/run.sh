#!/bin/sh

docker run -v `pwd`:/tmp/output hermes/importer $1
