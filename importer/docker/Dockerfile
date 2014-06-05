FROM ubuntu:14.04
ENV GDAL_VERSION 1.11.0
RUN apt-get update && \
  apt-get install -y build-essential curl libsqlite3-dev zlib1g-dev libproj-dev libgeos-dev libexpat-dev libfreexl-dev libreadosm-dev pkg-config libpq-dev libmysqlclient-dev && \
  apt-get clean && \
  curl http://www.gaia-gis.it/gaia-sins/libspatialite-4.1.1.tar.gz >/usr/local/src/libspatialite.tar.gz && \
  cd /usr/local/src && \
  tar xfz libspatialite.tar.gz && \
  cd libspatialite-4.1.1 && \
  ./configure  && make && make install && \
  curl http://www.gaia-gis.it/gaia-sins/spatialite-tools-4.1.1.tar.gz >/usr/local/src/spatialite-tools.tar.gz && \
  cd /usr/local/src && \
  tar xfz spatialite-tools.tar.gz && \
  cd spatialite-tools-4.1.1 && \
  ./configure && make && make install && \
  curl http://download.osgeo.org/gdal/CURRENT/gdal-$GDAL_VERSION.tar.gz >/usr/local/src/gdal.tar.gz && \
  cd /usr/local/src && \
  tar xfz gdal.tar.gz && \
  cd /usr/local/src/gdal-$GDAL_VERSION && \
  ./configure --with-spatialite=yes --with-mysql=/usr/bin/mysql_config && make && make install && ldconfig && \
  rm /usr/local/src/*gz
ADD import.sh /usr/bin/import.sh
ADD spatialite_osm_net.cfg /etc/spatialite_osm_net.cfg
ADD spatialite_osm_net_water.cfg /etc/spatialite_osm_net_water.cfg
ENTRYPOINT ["/usr/bin/import.sh"]
