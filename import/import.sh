#!/bin/sh

ogr2ogr -f SQLite -dsco spatialite=yes features.db $1 -gt 20000 -progress --config OGR_SQLITE_SYNCHRONOUS OFF
spatialite features.db "create index idx_lines_osm_id on lines(osm_id)"
spatialite features.db vacuum
spatialite_osm_net -tf import/spatialite_osm_net.cfg -jo -T paths -d graph.db -o $1
spatialite_network -d graph.db -T paths -f node_from -t node_to -g geometry -o routing
spatialite graph.db "CREATE INDEX idx_paths_node_from on paths(node_from)"
spatialite graph.db "CREATE INDEX idx_paths_node_to on paths(node_to)"
spatialite graph.db "SELECT CreateSpatialIndex('paths_nodes', 'geometry')"
spatialite_osm_net -tf import/spatialite_osm_net_water.cfg -jo -T waterways -d graph.db -o $1
spatialite_network -d graph.db -T waterways -f node_from -t node_to -g geometry -o routing_water
spatialite graph.db "CREATE INDEX idx_waterways_node_from on waterways(node_from)"
spatialite graph.db "CREATE INDEX idx_waterways_node_to on waterways(node_to)"
spatialite graph.db "SELECT CreateSpatialIndex('waterways_nodes', 'geometry')"
spatialite graph.db vacuum
