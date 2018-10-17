start transaction;

set local client_min_messages to 'warning';
set local schema 'public';

select 'Populating the schema...';

drop table if exists errcodes;

create table errcodes (
  "errcode" text primary key
);

\copy errcodes from program 'cat errcodes.txt | awk -F "[ ]+" ''{ if ($1 != "#" && $4 != "-" && $4 != "") { print $4 } }'' | sort | uniq'

create or replace function legacy_extension_names()
returns table (extname name)
language sql immutable as
$$
  values ('plpythonu'::name),
         ('plpython2u'::name),
         ('hstore_plpythonu'::name),
         ('hstore_plpython2u'::name),
         ('ltree_plpythonu'::name),
         ('ltree_plpython2u'::name);
$$;


create or replace function extension_names()
returns table (
          extname    name,
          extversion text
        )
language sql stable
set search_path to "pg_catalog" as
$$
  select name, default_version from pg_available_extensions()
   where name not in ( -- Extensions to skip
                       'citus',
                       'plr' -- Not available for PostgreSQL 9.6 or later?
                     )
     and name not in (select extname::name from public.legacy_extension_names());
$$;


create or replace function recommended_extensions()
returns table (extname text)
language sql immutable as
$$
  values ('pgrouting'), ('pgtap'), ('pldbgapi'), ('postgis'), ('postgis_topology');
$$;


create or replace function create_extensions()
returns setof void
language plpgsql volatile
set search_path to "public", "pg_catalog"
set client_min_messages to 'error' as
$$
declare
  _fn name;
begin
  for _fn in select extname from extension_names() loop
    execute format('create extension if not exists "%s" cascade', _fn);
  end loop;
  return;
end;
$$;


-- Among the keywords, we distinguish those corresponding to 'statements', as
-- other SQL syntax types do.
create or replace function get_statements()
returns table (stm text)
language sql immutable as
$$
  values ('create'), ('select'), ('abort'), ('alter'), ('analyze'), ('begin'),
         ('checkpoint'), ('close'), ('cluster'), ('comment'), ('commit'), ('constraints'),
         ('copy'), ('deallocate'), ('declare'), ('delete'), ('discard'),
         ('do'), ('drop'), ('end'), ('execute'), ('explain'), ('fetch'), ('grant'),
         ('import'), ('insert'), ('label'), ('listen'), ('load'), ('lock'), ('move'),
         ('notify'), ('prepare'), ('prepared'), ('reassign'), ('reindex'), ('refresh'), ('release'),
         ('reset'), ('revoke'), ('rollback'), ('savepoint'), ('security'),
         ('select'), ('set'), ('show'), ('start'), ('transaction'), ('truncate'),
         ('unlisten'), ('update'), ('vacuum'), ('values'), ('work');
$$;


create or replace function get_builtin_functions()
returns table (synfunction text)
language sql stable
set search_path to "information_schema" as
$$
  select distinct routine_name::text
    from routines
   where specific_schema = 'pg_catalog';
$$;


create or replace function get_catalog_tables()
returns table (table_name text)
language sql stable
set search_path to "information_schema" as
$$
  select table_name::text
    from tables
   where table_catalog = 'vim_pgsql_syntax' -- database name
     and table_name not like '\_%'
     and table_schema in ('pg_catalog', 'information_schema');
$$;


create or replace function get_operators()
returns table(keyword text)
language sql stable
set search_path to "pg_catalog", "public" as
$$
  -- Query adapted from \doS
  select distinct o.oprname::text as keyword
    from pg_catalog.pg_operator o
    left join pg_catalog.pg_namespace n
      on n.oid = o.oprnamespace
   where pg_catalog.pg_operator_is_visible(o.oid)
   order by keyword;
$$;

create or replace function get_types()
returns table ("type" text)
language sql stable
set search_path to "pg_catalog", "public" as
$$
  select distinct typname::text
    from pg_type
   where typname not like '\_%'
     and typname not like 'pg_toast_%'
     and typname not in ('any', 'name', 'pg_buffercache', 'pg_stat_statements', 'trigger', 'unknown')
     and typname not in (select get_catalog_tables());
$$;


-- Keywords that cannot be extracted from system catalogs (AFAIK)
create or replace function get_additional_types()
returns table ("type" text)
language sql immutable as
$$
  -- Serial types are not true types, but merely a notational convenience for creating unique identifier columns.
  -- See https://www.postgresql.org/docs/current/static/datatype-numeric.html#DATATYPE-SERIAL
  values ('smallserial'), ('serial'), ('bigserial'), ('serial2'), ('serial4'), ('serial8'),
         ('array'), ('bigint'), ('bit'), ('boolean'), ('char'), ('character'), ('cube'), ('decimal'),
         ('double'), ('int'), ('integer'),
         ('interval'), ('numeric'), ('precision'), ('real'), ('smallint'), ('text'), ('timestamp'),
         ('varchar'), ('varying'), ('xml'), ('zone');
$$;


-- Built-in keywords (except statements and types)
create or replace function get_keywords()
returns table (keyword text)
language sql stable
set search_path to "public", "pg_catalog" as
$$
  select word from pg_get_keywords()
  except
  (select stm from get_statements()
   union
   select "type" from get_types()
   union
   select "type" from get_additional_types());
$$;

create or replace function get_legacy_extension_objects(_extname name)
returns table (
          synclass text,
          synkeyword text
        )
language plpgsql immutable as
$$
begin
  case _extname
  when 'plpythonu' then
    return query
    values ('function', 'plpython_call_handler'),
           ('function', 'plpython_inline_handler'),
           ('function', 'plpython_validator');
  when 'plpython2u' then
    return query
    values ('function', 'plpython2_call_handler'),
           ('function', 'plpython2_inline_handler'),
           ('function', 'plpython2_validator');
  when 'hstore_plpythonu' then
    return query
    values ('function', 'hstore_to_plpython'),
           ('function', 'plpython_to_hstore');
  when 'hstore_plpython2u' then 
    return query
    values ('function', 'hstore_to_plpython2'),
           ('function', 'plpython2_to_hstore');
  when 'ltree_plpythonu' then
    return query
    values ('function', 'ltree_to_plpython');
  when 'ltree_plpython2u' then
    return query
    values ('function', 'ltree_to_plpython2');
  else
    raise exception 'Unknown legacy extension %', _extname;
  end case;
end;
$$;

-- Get the list of functions, tables, types and views installed by a given extension.
-- Query adapted from psql (\set ECHO_HIDDEN ON and \dx+ <extname> to see the query).
create or replace function get_extension_objects(_extname name)
returns table (
          synclass text,
          synkeyword text
        )
language sql stable
set search_path to "pg_catalog" as
$$
  select  distinct
          regexp_replace(pg_catalog.pg_describe_object(classid, objid, 0), '^(function|table|type|view).*', '\1') as synclass,
          regexp_replace(pg_catalog.pg_describe_object(classid, objid, 0), '^(function|table|type|view)\s+([^\(]+).*', '\2') as synkeyword
    from  pg_depend
   where  refclassid = 'pg_catalog.pg_extension'::pg_catalog.regclass
     and  refobjid = (select e.oid from pg_extension e where e.extname ~ format('^(%s)$', _extname))
     and  deptype = 'e'
     and  pg_describe_object(classid, objid, 0) ~* '^(function|table|type|view)\s+[^_]'
     and not pg_describe_object(classid, objid, 0) ~* '\w+\.\_'; -- Do not match things like 'public._some_func()';
$$;


-- Constants that cannot be extracted from system catalogs
create or replace function get_additional_constants()
returns table (keyword text)
language sql immutable as
$$
  values ('pg_catalog'), ('information_schema');
$$;


create or replace function get_errcodes()
returns table (errcode text)
language sql stable
set search_path to "public" as
$$
  select "errcode" from errcodes;
$$;


create or replace function preflight_requirements()
returns setof void
language plpgsql stable
set search_path to "public" as
$$
declare
  _missing text;
begin
  -- Refute to execute if db does not have the right name
  if current_database() <> 'vim_pgsql_syntax' then
    raise exception 'ERROR: Wrong database name!';
  end if;

  -- Print a warning if a recommended extension is missing
  for _missing in
    select extname from recommended_extensions()
    except
    select extname::text from extension_names()
  loop
    raise warning '% is missing. No syntax items will be generated for it.', _missing;
  end loop;
  return;
end;
$$;

select preflight_requirements();
select 'Creating extensions...';
select create_extensions();

commit;
