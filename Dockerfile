# GnuCOBOL 3.1
# Cloned from http://mirror.bibleonline.ru/gnu/gnucobol/gnucobol-3.1.1.tar.gz
# Merged with from https://github.com/DaveGamble/cJSON
# Run: docker build --tag olegkunitsyn/gnucobol:3.1 .
# Image size: 220MB
# https://hub.docker.com/repository/docker/olegkunitsyn/gnucobol
FROM alpine:latest

COPY . /opt/gnucobol
WORKDIR /opt/gnucobol

RUN apk add --update --no-cache \
    file \
    gdb \
    gmp \
    gmp-dev \
    libltdl \
    ncurses \
    ncurses-dev \
    db-dev \
    gcc \
    libgcc \
    libc-dev \
    gettext-dev \
    libxml2-dev \
    && apk add --update --virtual .tmp --no-cache \
    make \
    && wget https://github.com/OlegKunitsyn/cobolget/raw/master/bin/cobolget -O /usr/bin/cobolget && chmod +x /usr/bin/cobolget \
    && ./configure && make install && rm -rf * \
    && apk del .tmp

# FIXME 6 `make check` tests failed
# TODO make image smaller

ENTRYPOINT ["/bin/ash"]
