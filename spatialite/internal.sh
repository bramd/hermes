#!/bin/sh
cd /usr/local/src
git clone https://code.google.com/p/spatialite-android/
cd spatialite-android/spatialite-android-library/jni/
wget http://download.osgeo.org/proj/proj-4.8.0.tar.gz 
wget http://download.osgeo.org/geos/geos-3.3.6.tar.bz2 
wget http://www.sqlite.org/2013/sqlite-amalgamation-3071602.zip
wget http://www.gaia-gis.it/gaia-sins/libspatialite-sources/libspatialite-4.0.0.tar.gz 
tar -xvzf proj-4.8.0.tar.gz 
tar -xvjf geos-3.3.6.tar.bz2 
unzip sqlite-amalgamation-3071602.zip 
tar -xvzf libspatialite-4.0.0.tar.gz 
cd proj-4.8.0/ 
./configure --build=x86_64-pc-linux-gnu --host=arm-linux-eabi 
cd .. 
cd geos-3.3.6 
./configure --build=x86_64-pc-linux-gnu --host=arm-linux-eabi 
cd .. 
cd libspatialite-4.0.0/ 
./configure --build=x86_64-pc-linux-gnu --host=arm-linux-eabi 
cd .. 
/opt/android/ndk/ndk-build -j10
cd ..
rm src/jsqlite/*.class
ant release
