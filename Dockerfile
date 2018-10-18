FROM node:8

RUN mkdir /app

WORKDIR /app

COPY . /app

RUN npm install && ./node_modules/.bin/elm-package install -y

RUN npm run build && npm run build:player


FROM nginx:1.15

COPY --from=0 /app /usr/share/nginx/html
