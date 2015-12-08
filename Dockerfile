FROM node:4

ENV PURESCRIPT_DOWNLOAD_SHA1 b635631de5821468b99016f1e3e333f8bcfbf001

RUN cd /opt \
	&& wget https://github.com/purescript/purescript/releases/download/v0.7.6.1/linux64.tar.gz \
	&& tar -xvf linux64.tar.gz \
	&& rm /opt/linux64.tar.gz

ENV PATH /opt/purescript:$PATH

RUN npm install -g bower pulp
RUN echo '{ "allow_root": true }' > /root/.bowerrc

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

ENV NODE_PATH=/usr/src/app/output

COPY package.json /usr/src/app/
COPY bower.json /usr/src/app/
RUN bower i && npm install
COPY . /usr/src/app
RUN pulp build


ENTRYPOINT ["node"]
CMD ["app"]
