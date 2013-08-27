FROM ubuntu:quantal
RUN dpkg --add-architecture i386
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get -y install fuse  || :
RUN rm -rf /var/lib/dpkg/info/fuse.postinst
RUN apt-get -y install fuse
RUN dpkg-divert --local --rename --add /sbin/initctl && ln -s /bin/true /sbin/initctl
RUN apt-get install --no-install-recommends -y ia32-libs curl default-jdk ant-contrib maven expect build-essential wget git unzip
RUN mkdir -p /opt/android/sdk; mkdir -p /opt/android/ndk
RUN curl http://dl.google.com/android/android-sdk_r22.0.5-linux.tgz | tar xz -C /opt/android/sdk --strip-components 1
ENV ANDROID_HOME /opt/android/sdk
ENV PATH $ANDROID_HOME/tools:/opt/android/ndk:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ADD install-android-sdk.sh /install-android-sdk.sh
RUN /install-android-sdk.sh; rm /install-android-sdk.sh
RUN curl http://dl.google.com/android/ndk/android-ndk-r9-linux-x86_64.tar.bz2 | tar xj -C /opt/android/ndk --strip-components 1
ENV ANDROID_NDK_HOME /opt/android/ndk
ENV PATH /opt/android/ndk:$PATH
ADD internal.sh /internal.sh
RUN /internal.sh
