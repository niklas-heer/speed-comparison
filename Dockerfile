FROM alpine:3.7

RUN  apk add --update \
    python3 \
    php7 \
    rust \
    nodejs \
    go \
  && rm -rf /var/cache/apk/*

ADD . /usr/src/app
WORKDIR /usr/src/app
ENV PYTHONPATH /usr/src/app

COPY ./comparison.sh /
CMD ["/comparison.sh"]
