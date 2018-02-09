FROM stanangeloff/php:7.2-cli

MAINTAINER Stan Angeloff "stanimir@psp-webtech.co.uk"

RUN apt-get update -q && \
		apt-get install -y -qq \
			php7.2-bz2 \
			php7.2-gd \
			php7.2-memcache \
			php7.2-mysql \
			php7.2-pgsql \
		;

ADD scripts/ /build

WORKDIR /build

ENTRYPOINT ["/usr/bin/php"]
CMD ["update-vim-syntax.php"]
