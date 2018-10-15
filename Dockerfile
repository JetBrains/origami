FROM node:8

RUN mkdir /app
WORKDIR /app
RUN cd /app

COPY package.json /app

RUN npm install

COPY elm-package.json /app

RUN node_modules/.bin/elm-package install -y

COPY . /app
EXPOSE 8080
CMD ["npm", "start"]
