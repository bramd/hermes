#!/bin/sh

docker run -b `pwd`:/tmp/output hermes/importer $1
