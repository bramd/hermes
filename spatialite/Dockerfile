FROM ubuntu:quantal
# We need universe for maven
RUN echo "deb http://archive.ubuntu.com/ubuntu quantal main universe" > /etc/apt/sources.list
RUN apt-get update
RUN apt-get upgrade -y
RUN dpkg-divert --local --rename --add /sbin/initctl && ln -s /bin/true /sbin/initctl
# Fake a fuse install
RUN apt-get install libfuse2
RUN cd /tmp ; apt-get download fuse
RUN cd /tmp ; dpkg-deb -x fuse_* .
RUN cd /tmp ; dpkg-deb -e fuse_*
RUN cd /tmp ; rm fuse_*.deb
RUN cd /tmp ; echo -en '#!/bin/bash\nexit 0\n' > DEBIAN/postinst
RUN cd /tmp ; dpkg-deb -b . /fuse.deb
RUN cd /tmp ; dpkg -i /fuse.deb
RUN apt-get install --no-install-recommends -y curl default-jdk ant-contrib maven expect build-essential wget git unzip lib32z1 libc6-i386 lib32stdc++6 lib32gcc1 lib32ncurses5
RUN mkdir -p /opt/android/sdk; mkdir -p /opt/android/ndk
RUN curl http://dl.google.com/android/android-sdk_r22.3-linux.tgz | tar xz -C /opt/android/sdk --strip-components 1
ENV ANDROID_HOME /opt/android/sdk
ENV PATH $ANDROID_HOME/tools:/opt/android/ndk:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ADD install-android-sdk.sh /install-android-sdk.sh
RUN /install-android-sdk.sh; rm /install-android-sdk.sh
RUN curl http://dl.google.com/android/ndk/android-ndk-r9-linux-x86_64.tar.bz2 | tar xj -C /opt/android/ndk --strip-components 1
ENV ANDROID_NDK_HOME /opt/android/ndk
ENV PATH /opt/android/ndk:$PATH
ADD internal.sh /internal.sh
CMD ["/internal.sh"]
