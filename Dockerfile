FROM node:8

RUN mkdir /app
WORKDIR /app

COPY . /app

RUN cd /app


RUN npm install


RUN node_modules/.bin/elm-package install -y

RUN npm run build:player

EXPOSE 8080

CMD ["npm", "start"]
#CMD ["./node_modules/.bin/webpack-dev-server", "--mode=production"]
