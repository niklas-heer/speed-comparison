FROM alpine:edge

RUN wget http://public.portalier.com/alpine/julien@portalier.com-56dab02e.rsa.pub -O /etc/apk/keys/julien@portalier.com-56dab02e.rsa.pub
RUN echo http://public.portalier.com/alpine/testing >> /etc/apk/repositories
RUN echo @testing http://mirror1.hs-esslingen.de/pub/Mirrors/alpine/edge/testing >> /etc/apk/repositories
RUN  apk add --update \
    python3 \
    php7 \
    rust \
    nodejs \
    go \
    bash \
    crystal \
    gcc \
    shards \
    ruby \
    lua5.3 \
    julia \
    coreutils \
    nim@testing \
  && rm -rf /var/cache/apk/*

ADD . /usr/src/app
WORKDIR /usr/src/app
ENV PYTHONPATH /usr/src/app

COPY ./comparison.sh /
CMD ["/comparison.sh"]
