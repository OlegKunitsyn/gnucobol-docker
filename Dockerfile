# GnuCOBOL 3.1 dev
# Cloned from https://sourceforge.net/projects/open-cobol/files/gnucobol/nightly_snapshots/
# Merged with from https://github.com/DaveGamble/cJSON
# Run: docker build --tag olegkunitsyn/gnucobol:3.1-dev .
# Image size: 225MB
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
    && cd /opt/gnucobol && ./configure && make install && rm -rf * \
    && apk del .tmp

# FIXME 6 `make check` tests failed
# TODO make image smaller

ENTRYPOINT ["/bin/ash"]
