PSQLBIN  := psql
DBUSER   := postgres
DBNAME   := vim_pgsql_syntax
COMMON   := common.sql
SRCFILE  := pgsql.sql
SYNFILE  := pgsql.vim
TESTFILE := test.sql
KEYWORDS := keywords.sql
DESTDIR  := ../syntax

PSQL := PGOPTIONS='--client-min-messages=warning' $(PSQLBIN) -X --pset pager=off --quiet -v ON_ERROR_STOP=1 -U $(DBUSER)

.PHONY: all
all: $(SYNFILE) $(KEYWORDS)

createdb:
	@- $(PSQL) -tc "select 1 from pg_database where datname = '$(DBNAME)'" | grep -q 1 || $(PSQL) -c "create database $(DBNAME)"
	touch createdb

.PHONY: dropdb
dropdb:
	@- dropdb $(DBNAME) 2>/dev/null || true
	@- $(RM) createdb

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
	@- $(RM) keywords.sql
