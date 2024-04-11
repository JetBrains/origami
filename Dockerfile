FROM node:14-buster

RUN mkdir /app

RUN apt-get update && apt-get install -y binutils sudo && wget -qO - "https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz" | gzip -d > /usr/local/bin/elm
#RUN apt-get update && apt-get install -y binutils sudo && wget -qO - "https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz" | gzip -d > /usr/local/bin/elm && strip /usr/local/bin/elm
#RUN apt-get update && apt-get install -y binutils sudo

RUN useradd -m docker && echo "docker:docker" | chpasswd && adduser docker sudo

RUN passwd -d docker

RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> \
/etc/sudoers

USER docker

RUN sudo chmod +x /usr/local/bin/elm
RUN sudo chown docker:docker /app

WORKDIR /app

COPY /src/ /app
COPY package*.json /app/

RUN npm install

RUN sudo npm run build && sudo npm run build:player


FROM nginx:1.15

COPY --from=0 /app /usr/share/nginx/html
COPY ./nginx.conf /etc/nginx/conf.d/default.conf
