FROM java

RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - && \
    apt-get install -y nodejs

USER root

RUN mkdir ~/.npm-global
ENV NPM_CONFIG_PREFIX=~/.npm-global

RUN mkdir /app
WORKDIR /app

COPY . .
RUN pwd
#RUN cp ./package.json ~/.npm-global
#RUN cd /app

RUN npm install -g elm@0.18

RUN rm -Rf ./node_modules

RUN npm install
RUN elm-install
CMD [ "npm",  "start" ]


#RUN npm run build

#ADD /build/Main.min.js /gen



#ADD start.sh gen/start.sh

#ADD server.js gen/server.js

#CMD sh start.sh

#RUN npm install -g elm

#CMD python /app/app.py