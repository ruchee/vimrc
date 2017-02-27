FROM stanangeloff/php:7.1-cli

MAINTAINER Stan Angeloff "stanimir@psp-webtech.co.uk"

RUN apt-get update -q && \
		apt-get install -y -qq \
			php7.1-bz2 \
			php7.1-gd \
			php7.1-memcache \
			php7.1-mysql \
			php7.1-pgsql \
		;

ADD scripts/ /build

WORKDIR /build

ENTRYPOINT ["/usr/bin/php"]
CMD ["update-vim-syntax.php"]
