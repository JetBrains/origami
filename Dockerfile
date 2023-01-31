FROM node:current-alpine

RUN mkdir /app

WORKDIR /app

COPY . /app

RUN apk add binutils && wget -qO - "https://github.com/elm/compiler/releases/download/0.19.1/binaries-for-linux.tar.gz" | tar -zxC /usr/local/bin/ && strip /usr/local/bin/elm && apk del binutils

RUN npm install

RUN npm run build && npm run build:player


FROM nginx:1.15

COPY --from=0 /app /usr/share/nginx/html
COPY ./nginx.conf /etc/nginx/conf.d/default.conf
