# GnuCOBOL 2.2
# Cloned from https://sourceforge.net/projects/gnucobol/files/gnucobol/2.2/gnucobol-2.2.tar.xz/download
# Merged with from https://github.com/DaveGamble/cJSON
# Run: docker build --tag olegkunitsyn/gnucobol:2.2 .
# Image size: 223MB
# https://hub.docker.com/r/olegkunitsyn/gnucobol
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
    && cd /opt/gnucobol \
    && wget https://github.com/OlegKunitsyn/cobolget/raw/master/bin/cobolget -O /usr/bin/cobolget && chmod +x /usr/bin/cobolget \
    && ./configure && make install && rm -rf * \
    && apk del .tmp

# FIXME 5 `make check` tests failed

ENTRYPOINT ["/bin/ash"]
