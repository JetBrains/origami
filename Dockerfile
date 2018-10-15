FROM node:8

RUN mkdir /app
WORKDIR /app
RUN cd /app

COPY package.json /app

RUN npm install

COPY elm-package.json /app

RUN node_modules/.bin/elm-package install -y
#RUN npm run build
COPY . .
EXPOSE  8080
CMD ["npm", "start"]


#RUN npm install -g elm@0.18
#
#RUN npm install
#RUN elm-install
#CMD [ "npm",  "start" ]
##RUN npm run build
#
##ADD /build/Main.min.js /gen



#ADD start.sh gen/start.sh

#ADD server.js gen/server.js

#CMD sh start.sh

#RUN npm install -g elm

#CMD python /app/app.py