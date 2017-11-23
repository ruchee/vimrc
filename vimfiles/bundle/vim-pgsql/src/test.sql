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
    select format('-- Extension: ', _ext.extname);

    return query
    select regexp_replace(synkeyword, '^\w+\.|"', '', 'g')       ||
           case when synclass = 'function' then '()' else '' end ||
           ' -- ' || synclass
      from get_extension_objects(_ext.extname);

  end loop;

  return;
end;
$$;

select 'Generating the test file...';

\o keywords.sql
select '-- Statements';
select stm from get_statements();
select '-- Types';
select "type" from get_types();
select 'pg_toast_1234';
select '-- Built-in functions';
select synfunction || '()' from get_builtin_functions();
select vim_extensions();
select '-- Extensions names';
select extname from extension_names() where not extname ~* '-';
select '-- Catalog tables';
select table_name from get_catalog_tables();
select '-- Built-in keywords';
select keyword from get_keywords();
select '-- Additional keywords';
select keyword from get_additional_keywords();
select '-- Additional constants';
select keyword from get_additional_constants();
select '-- Error codes';
select errcode from get_errcodes();

\o
select 'done!';
commit;
\o
