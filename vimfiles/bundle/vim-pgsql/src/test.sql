-- Author:   Lifepillar
-- License:  Vim license

-- Generate keywords.sql.

\o
\set QUIET 1
\pset border 0
\pset columns 0
\pset expanded off
\pset fieldsep ' '
\pset footer off
\pset format unaligned
\pset null ''
\pset numericlocale off
\pset pager off
\pset recordsep '\n'
\pset title
\pset tuples_only on
\i common.sql

start transaction;

set local client_min_messages to 'warning';
set local schema 'public';

create or replace function vim_extensions()
returns setof text
language plpgsql stable
set search_path to "public" as
$$
declare
  _ext record;
begin
  for _ext in select extname from extension_names() loop

    return query
    select format('-- Extension: %s', _ext.extname);

    return query
    select regexp_replace(synkeyword, '^\w+\.|"', '', 'g')       ||
           case when synclass = 'function' then '()' else '' end ||
           ' -- ' || synclass
      from get_extension_objects(_ext.extname);

  end loop;

  for _ext in select extname from legacy_extension_names() loop

    return query
    select format('-- Extension: %s', _ext.extname);

    return query
    select regexp_replace(synkeyword, '^\w+\.|"', '', 'g')       ||
           case when synclass = 'function' then '()' else '' end ||
           ' -- ' || synclass
      from get_legacy_extension_objects(_ext.extname);

  end loop;
  return;
end;
$$;

select 'Generating the test file...';

\o keywords.sql
select '-- Statements';
select stm from get_statements() order by stm;
select '-- Types';
select "type" from get_types() order by "type";
select '-- Additional types';
select "type" from get_additional_types() order by "type";
select 'pg_toast_1234';
select '-- Built-in functions';
select synfunction || '()' from get_builtin_functions() order by synfunction;
select vim_extensions();
select '-- Extensions names';
select extname from extension_names() where not extname ~* '-' order by extname;
select '-- Legacy extensions names';
select extname from legacy_extension_names() where not extname ~* '-' order by extname;
select '-- Catalog tables';
select table_name from get_catalog_tables() order by table_name;
select '-- Built-in keywords';
select keyword from get_keywords() order by keyword;
select '-- Additional constants';
select keyword from get_additional_constants() order by keyword;
select '-- Operators';
select keyword from get_operators() order by keyword;
select '-- Error codes';
select errcode from get_errcodes() order by errcode;

select
$HERE$
--
-- More checks
--

-- See https://github.com/lifepillar/pgsql.vim/issues/4
where cast(t.data #>> '{user,id_str}' as bigint) = 123;

-- Backslashes and quotes are not special in identifiers:
select "a\" from T;
select "a\\" from T;
select "a""" from T;
select "a'" from T;
select "a\'" from T;

-- Backslashes and quotes are not special in strings:
select '\';
select '\\';
select '\''';
select '''' || 'ok';
select '"';
select '""';
select '\"';

-- Quoted identifiers including escaped Unicode characters identified by their code points
select U&"\0441\043B\043E\043D" from T;
select U&"d!0061t!+000061" uescape '!' from T;

-- String constants with C-style escapes
select E'\b\f\n\r\t''abc\FF\'abc\'';

-- Datetime expressions (https://www.postgresql.org/docs/current/functions-datetime.html)
select make_interval(days => 10);
select timestamp with time zone '2005-04-02 12:00-07' + interval '1 day';
select timestamp without time zone '2005-04-02 12:00-07' + interval '1 day'
select extract(epoch from timestamptz '2013-07-01 12:00:00') -
       extract(epoch from timestamptz '2013-03-01 12:00:00');
select extract(century from timestamp '2000-12-16 12:21:13');
select extract(day from timestamp '2000-12-16 12:21:13');
select extract(decade from timestamp '2000-12-16 12:21:13');
select extract(dow from timestamp '2000-12-16 12:21:13');
select extract(doy from timestamp '2000-12-16 12:21:13');
select extract(isodow from timestamp '2000-12-16 12:21:13');
select extract(isoyear from timestamp '2000-12-16 12:21:13');
select extract(microseconds from timestamp '2000-12-16 12:21:13');
select extract(millennium from timestamp '2000-12-16 12:21:13');
select extract(milliseconds from timestamp '2000-12-16 12:21:13');
select extract(quarter from timestamp '2000-12-16 12:21:13');
select extract(timezone from timestamp '2000-12-16 12:21:13');
select extract(timezone_hour from timestamp '2000-12-16 12:21:13');
select extract(timezone_minute from timestamp '2000-12-16 12:21:13');
select extract(week from timestamp '2000-12-16 12:21:13');
select timestamp '2001-02-16 20:38:40' at time zone 'America/Denver';
$HERE$;



\o
select 'done!';
commit;
\o
