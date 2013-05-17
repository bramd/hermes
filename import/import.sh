#!/bin/sh

ogr2ogr -f SQLite -dsco spatialite=yes features.db $1 -gt 20000 -progress --config OGR_SQLITE_SYNCHRONOUS OFF
spatialite features.db vacuum
spatialite_osm_net -tf import/spatialite_osm_net.cfg -jo -T paths -d graph.db -o $1
spatialite_network -d graph.db -T paths -f node_from -t node_to -g geometry -o routing
spatialite graph.db <import/graph.sql
spatialite graph.db vacuum
