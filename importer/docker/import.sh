#!/bin/sh

curl $1 -o /tmp/map.osm.pbf
ogr2ogr -f SQLite -dsco spatialite=yes /tmp/output/features.db /tmp/map.osm.pbf -gt 20000 -progress --config OGR_SQLITE_SYNCHRONOUS OFF
spatialite /tmp/output/features.db "create index idx_lines_osm_id on lines(osm_id)"
spatialite /tmp/output/features.db vacuum
spatialite_osm_net -tf /etc/spatialite_osm_net.cfg -jo -T paths -d /tmp/output/graph.db -o /tmp/map.osm.pbf
spatialite_network -d /tmp/output/graph.db -T paths -f node_from -t node_to -g geometry -o routing
spatialite /tmp/output/graph.db "CREATE INDEX idx_paths_node_from on paths(node_from)"
spatialite /tmp/output/graph.db "CREATE INDEX idx_paths_node_to on paths(node_to)"
spatialite /tmp/output/graph.db "SELECT CreateSpatialIndex('paths_nodes', 'geometry')"
spatialite_osm_net -tf /etc/spatialite_osm_net_water.cfg -jo -T waterways -d /tmp/output/graph.db -o /tmp/map.osm.pbf
spatialite_network -d /tmp/output/graph.db -T waterways -f node_from -t node_to -g geometry -o routing_water
spatialite /tmp/output/graph.db "CREATE INDEX idx_waterways_node_from on waterways(node_from)"
spatialite /tmp/output/graph.db "CREATE INDEX idx_waterways_node_to on waterways(node_to)"
spatialite /tmp/output/graph.db "SELECT CreateSpatialIndex('waterways_nodes', 'geometry')"
spatialite /tmp/output/graph.db vacuum
