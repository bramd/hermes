FROM ubuntu:13.10
RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y software-properties-common git && \
  add-apt-repository ppa:chris-lea/node.js && \
  apt-get update && \
  apt-get install -y nodejs && \
  apt-get clean && \
  adduser --system app --home /srv/app --shell /bin/sh
WORKDIR /srv/app
ENV HOME /srv/app
ENV NODE_ENV production
EXPOSE 9778
ADD . /srv/app
RUN chown -R app $HOME
USER app
RUN npm install >/dev/null && \
  node_modules/bower/bin/bower install
CMD npm start
