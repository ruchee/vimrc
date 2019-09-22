FROM ubuntu:18.04

MAINTAINER Stan Angeloff "stanimir@psp-webtech.co.uk"

ENV UBUNTU_RELEASE=bionic \
    ONDREJ_PHP_KEY="14AA40EC0831756756D7F66C4F4EA0AAE5267A6C"

RUN apt-get update && apt-get install -y gnupg2 && \
    /bin/echo -e "\n\ndeb http://ppa.launchpad.net/ondrej/php/ubuntu $UBUNTU_RELEASE main\ndeb-src http://ppa.launchpad.net/ondrej/php/ubuntu $UBUNTU_RELEASE main" >> /etc/apt/sources.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys "$ONDREJ_PHP_KEY" && \
    apt-get update

RUN apt-get install -y \
        \
        php5.6-cli \
        php5.6-bcmath \
        php5.6-bz2 \
        php5.6-curl \
        php5.6-dom \
        php5.6-gd \
        php5.6-gettext \
        php5.6-iconv \
        php5.6-json \
        php5.6-mbstring \
        php5.6-mcrypt \
        php5.6-mysql \
        php5.6-mysqli \
        php5.6-pdo \
        php5.6-pgsql \
        php5.6-phar \
        php5.6-simplexml \
        php5.6-soap \
        php5.6-sockets \
        php5.6-sqlite3 \
        php5.6-tokenizer \
        php5.6-wddx \
        php5.6-xml \
        php5.6-xmlreader \
        php5.6-xmlwriter \
        php5.6-zip \
        \
        php7.0-cli \
        php7.0-bcmath \
        php7.0-bz2 \
        php7.0-curl \
        php7.0-dom \
        php7.0-gd \
        php7.0-gettext \
        php7.0-iconv \
        php7.0-json \
        php7.0-mbstring \
        php7.0-mcrypt \
        php7.0-mysql \
        php7.0-mysqli \
        php7.0-pdo \
        php7.0-pgsql \
        php7.0-phar \
        php7.0-simplexml \
        php7.0-soap \
        php7.0-sockets \
        php7.0-sqlite3 \
        php7.0-tokenizer \
        php7.0-wddx \
        php7.0-xml \
        php7.0-xmlreader \
        php7.0-xmlwriter \
        php7.0-zip \
        \
        php7.1-cli \
        php7.1-bcmath \
        php7.1-bz2 \
        php7.1-curl \
        php7.1-dom \
        php7.1-gd \
        php7.1-gettext \
        php7.1-iconv \
        php7.1-json \
        php7.1-mbstring \
        php7.1-mcrypt \
        php7.1-mysql \
        php7.1-mysqli \
        php7.1-pdo \
        php7.1-pgsql \
        php7.1-phar \
        php7.1-simplexml \
        php7.1-soap \
        php7.1-sockets \
        php7.1-sqlite3 \
        php7.1-tokenizer \
        php7.1-wddx \
        php7.1-xml \
        php7.1-xmlreader \
        php7.1-xmlwriter \
        php7.1-zip \
        \
        php7.2-cli \
        php7.2-bcmath \
        php7.2-bz2 \
        php7.2-curl \
        php7.2-dom \
        php7.2-gd \
        php7.2-gettext \
        php7.2-iconv \
        php7.2-json \
        php7.2-mbstring \
        php7.2-mysql \
        php7.2-mysqli \
        php7.2-pdo \
        php7.2-pgsql \
        php7.2-phar \
        php7.2-simplexml \
        php7.2-soap \
        php7.2-sockets \
        php7.2-sqlite3 \
        php7.2-tokenizer \
        php7.2-wddx \
        php7.2-xml \
        php7.2-xmlreader \
        php7.2-xmlwriter \
        php7.2-zip \
        \
        php7.3-cli \
        php7.3-bcmath \
        php7.3-bz2 \
        php7.3-curl \
        php7.3-dom \
        php7.3-gd \
        php7.3-gettext \
        php7.3-iconv \
        php7.3-json \
        php7.3-mbstring \
        php7.3-mysql \
        php7.3-mysqli \
        php7.3-pdo \
        php7.3-pgsql \
        php7.3-phar \
        php7.3-simplexml \
        php7.3-soap \
        php7.3-sockets \
        php7.3-sqlite3 \
        php7.3-tokenizer \
        php7.3-wddx \
        php7.3-xml \
        php7.3-xmlreader \
        php7.3-xmlwriter \
        php7.3-zip \
        \
        ;

ADD attic/ /var/php

WORKDIR /var/php

CMD ["/bin/sh", "/var/php/update.sh"]
