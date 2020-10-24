# OpenCOBOL 1.1
# Cloned from https://sourceforge.net/projects/gnucobol/files/open-cobol/1.1/
# https://hub.docker.com/r/olegkunitsyn/opencobol
# Run: 
#  docker build --tag olegkunitsyn/opencobol:1.1 .
#  docker run -d -i --name opencobol olegkunitsyn/opencobol:1.1
#  docker exec -i opencobol cobc -V
#  docker exec -i opencobol gdb -v
#  docker push olegkunitsyn/opencobol:1.1
FROM alpine:latest

COPY . /opt/opencobol
WORKDIR /opt/opencobol

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
    && ./configure && make install && rm -rf * \
    && apk del .tmp

# FIXME 2 `make check` tests failed

ENTRYPOINT ["/bin/ash"]
