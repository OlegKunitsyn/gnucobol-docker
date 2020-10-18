# GnuCOBOL 1.1
# Cloned from https://sourceforge.net/projects/gnucobol/files/open-cobol/1.1/
# Run: docker build --tag olegkunitsyn/gnucobol:1.1 .
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
    && ./configure && make check && rm -rf * \
    && apk del .tmp

# FIXME 2 `make check` tests failed

ENTRYPOINT ["/bin/ash"]
