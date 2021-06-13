# vim: ts=8 sts=8 sw=8
# Author:   Lifepillar
# License:  Vim license
MAKEFLAGS += --warn-undefined-variables
SHELL = /bin/sh

PSQLBIN  := psql
DBHOST   ?= 127.0.0.1
DBUSER   ?= postgres
DBNAME   ?= vim_pgsql_syntax
COMMON   := common.sql
SRCFILE  := pgsql.sql
SYNFILE  := pgsql.vim
TESTFILE := test.sql
KEYWORDS := keywords.sql
DESTDIR  := ../syntax

PSQL := PGOPTIONS='--client-min-messages=warning' $(PSQLBIN) -X --pset pager=off --quiet -v ON_ERROR_STOP=1 -h $(DBHOST) -U $(DBUSER)

.PHONY: all
all: $(SYNFILE) $(KEYWORDS)

.PHONY: createdb
createdb:
	@- echo "select 'create database $(DBNAME)' where not exists (select 1 from pg_database where datname = '$(DBNAME)')\gexec" | $(PSQL) -d template1

.PHONY: dropdb
dropdb:
	@- $(PSQL) -d template1 -c 'drop database $(DBNAME)' 2>/dev/null || true

errcodes.txt:
	curl -fsSL https://rawgit.com/postgres/postgres/master/src/backend/utils/errcodes.txt >errcodes.txt

$(SYNFILE): createdb errcodes.txt $(COMMON) $(SRCFILE)
	@ $(PSQL) -d $(DBNAME) -f $(SRCFILE)

$(KEYWORDS): $(TESTFILE) $(COMMON)
	@ $(PSQL) -d $(DBNAME) -f $<

.PHONY: install
install: $(SYNFILE) $(KEYWORDS)
	cp $< $(DESTDIR)/$(SYNFILE)

.PHONY: clean
clean:
	@- $(RM) $(SYNFILE)

.PHONY: distclean
distclean: clean dropdb
	@- $(RM) errcodes.txt
