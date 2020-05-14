# GnuCOBOL 3.1 dev
# Cloned from https://sourceforge.net/projects/open-cobol/files/gnucobol/nightly_snapshots/
# Cloned from https://sourceforge.net/projects/cjson/files/
# Run: docker build --tag gnucobol:3.1-dev .
# Image size: 25MB
FROM alpine:latest

COPY . /opt/gnucobol

RUN apk add --update --no-cache \
    file \
    gdb \
    gmp \
    gmp-dev \
    libltdl \
    ncurses \
    ncurses-dev \
    db-dev \
    make \
    gcc \
    libgcc \
    libc-dev \
    gettext-dev \
    libxml2-dev \
    && cd /opt/gnucobol && ./configure && make check && make install && rm -rf *

WORKDIR /var/cobol
