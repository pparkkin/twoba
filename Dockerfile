FROM haskell:8.0.2
MAINTAINER Paavo Parkkinen <pparkkin@gmail.com>

COPY ./stack-bootstrap .
RUN stack install \
  --resolver lts-8.15 \
  $(cat stack-bootstrap)


COPY . /opt/twoba
WORKDIR /opt/twoba

RUN stack install

EXPOSE 3000

CMD twoba-server

