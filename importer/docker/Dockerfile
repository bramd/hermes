FROM base
RUN apt-get update
RUN apt-get install -y build-essential curl libsqlite3-dev zlib1g-dev libproj-dev libgeos-dev libexpat-dev pkg-config
RUN curl http://www.gaia-gis.it/gaia-sins/libspatialite-4.1.1.tar.gz >/usr/local/src/libspatialite.tar.gz
RUN cd /usr/local/src; tar xfz libspatialite.tar.gz
RUN cd /usr/local/src/libspatialite-4.1.1; ./configure --disable-freexl; make; make install
RUN curl http://www.gaia-gis.it/gaia-sins/readosm-1.0.0b.tar.gz >/usr/local/src/readosm.tar.gz
RUN cd /usr/local/src; tar xfz readosm.tar.gz
RUN cd /usr/local/src/readosm-1.0.0b; ./configure; make; make install
RUN curl http://www.gaia-gis.it/gaia-sins/spatialite-tools-4.1.1.tar.gz >/usr/local/src/spatialite-tools.tar.gz
RUN cd /usr/local/src; tar xfz spatialite-tools.tar.gz
RUN cd /usr/local/src/spatialite-tools-4.1.1; ./configure --disable-freexl; make; make install
RUN curl http://download.osgeo.org/gdal/CURRENT/gdal-1.10.0.tar.gz >/usr/local/src/gdal.tar.gz
RUN cd /usr/local/src; tar xfz gdal.tar.gz
RUN cd /usr/local/src/gdal-1.10.0; ./configure --with-spatialite=yes; make; make install; ldconfig
ADD import.sh /usr/bin/import.sh
ADD spatialite_osm_net.cfg /etc/spatialite_osm_net.cfg
ADD spatialite_osm_net_water.cfg /etc/spatialite_osm_net_water.cfg
ENTRYPOINT ["/usr/bin/import.sh"]
