-- To generate the syntax file, proceed as follows:
--
-- 1. createdb -T template0 vim_pgsql_syntax
-- 2. psql -f pgsql.sql vim_pgsql_syntax
--
-- ...or simply use the provided Makefile.

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


-- Format keywords to use in a Vim syntax file.
-- _keywords is a list of keywords.
-- _kind is the highlight group (without the 'sql' prefix).
-- _wrap is the (approximate) number of columns after which a new line should start.
--
-- Example: select vim_format('[create, select]', 'Statement', 6).
create or replace function vim_format(
  _keywords text[],
  _kind text,
  _wrap integer default 60)
returns setof text
language plpgsql stable
set search_path to "public" as
$$
begin
  return query
    with T as (
      select sum(char_length(keyword)) over (order by keyword) as num, keyword
        from unnest(_keywords) K(keyword)
    )
    select format('syn keyword sql%s contained %s', _kind, string_agg(keyword, ' '))
      from T
     group by num / (_wrap - 1)
     order by num / (_wrap - 1);
  return;
end;
$$;

create or replace function vim_format_operators(
  _keywords text[],
  _kind text,
  _wrap integer default 50)
returns setof text
language plpgsql stable
set search_path to "public" as
$$
begin
  return query
    with T as (
      select sum(char_length(keyword)) over (order by char_length(keyword) desc, keyword) as num, keyword
        from unnest(_keywords) K(keyword)
    )
    select format('syn match sql%s contained "\%%(%s\)\ze\%%([^!?~#^@<=>%%&|*/+-]\|$\)"',
                   _kind,
                   string_agg(regexp_replace(keyword, '[~^*#]', E'\\\\\\&', 'g'), '\|'))
      from T
     group by num / (_wrap - 1)
     order by num / (_wrap - 1);
  return;
end;
$$;

-- Define keywords for all extensions
create or replace function vim_format_extensions(
  _wrap integer default 60
)
returns setof text
language plpgsql stable
set search_path to "public" as
$$
declare
  _ext record;
begin
  for _ext in select extname, extversion from extension_names() loop

    return query
    select format('" Extension: %s (v%s)', _ext.extname, _ext.extversion);

    return query
    select format('if index(get(g:, ''pgsql_disabled_extensions'', []), ''%s'') == -1', _ext.extname);

    return query
    with T as (
      select sum(char_length(synkeyword)) over (partition by synclass order by synkeyword) num, synclass, synkeyword
        from get_extension_objects(_ext.extname)
      )
      select format('  syn keyword sql%s contained %s', initcap(synclass), string_agg(regexp_replace(synkeyword, '^\w+\.|"', '', 'g'), ' ')) -- remove schema name and double quotes
        from T
      group by synclass, num / (_wrap - 1)
      order by synclass, num / (_wrap - 1);

      if _ext.extname = 'postgis' then
        return query
        select '  syn keyword sqlFunction contained geometry_eq pgis_abs_in pgis_abs_out pgis_abs';
      end if;

      if _ext.extname = 'cube' then
        return query
        select '  syn keyword sqlFunction contained g_cube_compress g_cube_decompress';
      end if;

      if _ext.extname = 'seg' then
        return query
        select '  syn keyword sqlFunction contained gseg_compress gseg_decompress';
      end if;

    return query
      select format('endif " %s', _ext.extname);

  end loop;

  for _ext in select extname from legacy_extension_names() loop

    return query
    select format('" Extension: %s', _ext.extname);

    return query
    select format('if index(get(g:, ''pgsql_disabled_extensions'', []), ''%s'') == -1', _ext.extname);

    return query
    with T as (
      select sum(char_length(synkeyword)) over (partition by synclass order by synkeyword) num, synclass, synkeyword
        from get_legacy_extension_objects(_ext.extname)
      )
      select format('  syn keyword sql%s contained %s', initcap(synclass), string_agg(regexp_replace(synkeyword, '^\w+\.|"', '', 'g'), ' ')) -- remove schema name and double quotes
        from T
      group by synclass, num / (_wrap - 1)
      order by synclass, num / (_wrap - 1);

    return query
      select format('endif " %s', _ext.extname);

  end loop;

  return;
end;
$$;

-------------------------------------------------------------------------------

select 'Generating syntax file...';

\o pgsql.vim

select
$HERE$" Vim syntax file
" Language:     SQL (PostgreSQL dialect), PL/pgSQL, PL/…, PostGIS, …
" Maintainer:   Lifepillar
" Version:      2.2.0
" License:      This file is placed in the public domain.
$HERE$;

select '" Based on ' || substring(version() from 'PostgreSQL \d+\.\d+\.?\d*');
select '" Automatically generated on ' || current_date || ' at ' || localtime(0);

select
$HERE$
if exists("b:current_syntax")
  finish
endif

syn case ignore
syn sync minlines=100
syn iskeyword @,48-57,192-255,_

syn match sqlIsKeyword  /\<\h\w*\>/   contains=sqlStatement,sqlKeyword,sqlCatalog,sqlConstant,sqlSpecial,sqlOption,sqlErrorCode,sqlType,sqlTable,sqlView
syn match sqlIsFunction /\<\h\w*\ze(/ contains=sqlFunction,sqlKeyword
syn region sqlIsPsql    start=/^\s*\\/ end=/\n/ oneline contains=sqlPsqlCommand,sqlPsqlKeyword,sqlNumber,sqlString

syn keyword sqlSpecial contained false null true
$HERE$;

select '" Statements';
select vim_format(array(select get_statements()), 'Statement');
select '" Types';
select vim_format(array(select get_types()), 'Type');
select 'syn match sqlType /pg_toast_\d\+/';
select '" Additional types';
select vim_format(array(select get_additional_types()), 'Type');
select '" Keywords';
select vim_format(array(select get_keywords()), 'Keyword');
select vim_format(array(select get_additional_constants()), 'Constant');
select '" Built-in functions';
select vim_format(array(select get_builtin_functions()), 'Function');
select '" Extensions names';
select vim_format(array(select extname from extension_names() where not extname ~* '-'), 'Constant');
select '" Legacy extensions names';
select vim_format(array(select extname from legacy_extension_names() where not extname ~* '-'), 'Constant');
select vim_format_extensions();
select '" Catalog tables';
select vim_format(array(select get_catalog_tables()), 'Catalog');
select  '" Error codes (Appendix A, Table A-1)';
select vim_format(array(select get_errcodes()), 'ErrorCode');

select
$HERE$
" Legacy keywords
syn keyword sqlFunction contained gist_box_compress gist_box_decompress gist_box_fetch
syn keyword sqlFunction contained gtsquery_decompress inet_gist_decompress
syn keyword sqlFunction contained pg_file_length pg_file_read pg_logfile_rotate
syn keyword sqlFunction contained range_gist_compress range_gist_decompress range_gist_fetch

" Legacy error codes
syn keyword sqlErrorCode contained invalid_preceding_following_size

" Numbers
syn match sqlNumber "\<\d*\.\=[0-9_]\>"

" Strings
if get(g:, 'pgsql_backslash_quote', 0)
  syn region sqlString start=+E\?'+ skip=+\\\\\|\\'\|''+ end=+'+ contains=@Spell
else
  syn region sqlString start=+E'+ skip=+\\\\\|\\'\|''+ end=+'+ contains=@Spell
  syn region sqlString start=+'+ skip=+''+ end=+'+ contains=@Spell
endif
" Multi-line strings ("here" documents)
syn region sqlString start='\$\z(\w\+\)\$' end='\$\z1\$' contains=@Spell

" Escape String Constants
" Identifiers
syn region sqlIdentifier start=+\%(U&\)\?"+ end=+"+
syn keyword sqlConstant UESCAPE

" Operators
syn match sqlIsOperator "\%(^\|[^!?~#^@<=>%&|*/+-]\)\zs[!?~#^@<=>%&|*/+-]\+" contains=sqlOperator
$HERE$;

select vim_format_operators(array(select get_operators()), 'Operator');

select
$HERE$
" Comments
syn region sqlComment    start="/\*" end="\*/" contains=sqlTodo,@Spell
syn match  sqlComment    "#\s.*$"              contains=sqlTodo,@Spell
syn match  sqlComment    "--.*$"               contains=sqlTodo,@Spell

" Options
syn keyword sqlOption contained client_min_messages search_path

syntax case match

" Psql Keywords
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\[aCfHhortTxz]\>\|\\[?!]/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\c\%(\%(d\|onnect\|onninfo\|opy\%(right\)\?\|rosstabview\)\?\)\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\d\>\|\\dS\>+\?\|\\d[ao]S\?\>\|\\d[cDgiLmnOstTuvE]\%(\>\|S\>+\?\)/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\d[AbClx]\>+\?\|\\d[py]\>\|\\dd[pS]\>\?\|\\de[tsuw]\>+\?\|\\df[antw]\?S\?\>+\?\|\\dF[dpt]\?\>+\?\|\\drds\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\e\%(cho\|[fv]\|ncoding\|rrverbose\)\?\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\g\%(exec\|set\)\?\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\ir\?\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\l\>+\?\|\\lo_\%(export\|import\|list\|unlink\)\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\p\%(assword\|rompt\|set\)\?\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\q\%(echo\)\?\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\s\>\|\\s[fv]\>+\?\|\\set\%(env\)\?\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\t\%(iming\)\?\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\unset\>/
syn match sqlPsqlCommand contained nextgroup=sqlPsqlKeyword,sqlNumber,sqlString /\\w\%(atch\)\?\>/
syn keyword sqlPsqlKeyword contained format border columns expanded fieldsep fieldsep_zero footer null
syn keyword sqlPsqlKeyword contained numericlocale recordsep recordsep_zero tuples_only title tableattr pages
syn keyword sqlPsqlKeyword contained unicode_border_linestyle unicode_column_linestyle unicode_header_linestyle
syn keyword sqlPsqlKeyword contained on off auto unaligned pager
syn keyword sqlPsqlKeyword contained AUTOCOMMIT HISTCONTROL PROMPT VERBOSITY SHOW_CONTEXT VERSION
syn keyword sqlPsqlKeyword contained DBNAME USER HOST PORT ENCODING HISTSIZE QUIET

" Todo
syn keyword sqlTodo contained TODO FIXME XXX DEBUG NOTE

syntax case ignore

" PL/pgSQL
syn keyword sqlPlpgsqlKeyword contained alias all array as begin by case close collate column constant
syn keyword sqlPlpgsqlKeyword contained constraint continue current current cursor datatype declare
syn keyword sqlPlpgsqlKeyword contained detail diagnostics else elsif end errcode exception execute
syn keyword sqlPlpgsqlKeyword contained exit fetch for foreach forward found from get hint if
syn keyword sqlPlpgsqlKeyword contained into last loop message move next no notice open perform prepare
syn keyword sqlPlpgsqlKeyword contained query raise relative return reverse rowtype schema
syn keyword sqlPlpgsqlKeyword contained scroll slice sqlstate stacked strict table tg_argv tg_event
syn keyword sqlPlpgsqlKeyword contained tg_level tg_name tg_nargs tg_op tg_relid tg_relname
syn keyword sqlPlpgsqlKeyword contained tg_table_name tg_table_schema tg_tag tg_when then type using
syn keyword sqlPlpgsqlKeyword contained while

" Variables (identifiers conventionally starting with an underscore)
syn match sqlPlpgsqlVariable "\<_[A-Za-z0-9][A-Za-z0-9_]*\>" contained
" Numbered arguments
syn match sqlPlpgsqlVariable "\$\d\+" contained
" @ arguments
syn match sqlPlpgsqlVariable ".\zs@[A-z0-9_]\+" contained

syn region plpgsql matchgroup=sqlString start=+\$pgsql\$+ end=+\$pgsql\$+ keepend contains=ALL
syn region plpgsql matchgroup=sqlString start=+\$body\$+ end=+\$body\$+ keepend contains=ALL
if get(g:, 'pgsql_dollar_strings', 0)
  syn region sqlString start=+\$\$+ end=+\$\$+ contains=@Spell
else
  syn region plpgsql matchgroup=sqlString start=+\$\$+ end=+\$\$+ keepend contains=ALL
endif

" PL/<any other language>
fun! s:add_syntax(s)
  execute 'syn include @PL' . a:s . ' syntax/' . a:s . '.vim'
  unlet b:current_syntax
  execute 'syn region pgsqlpl' . a:s . ' matchgroup=sqlString start=+\$' . a:s . '\$+ end=+\$' . a:s . '\$+ keepend contains=@PL' . a:s
endf

for pl in get(b:, 'pgsql_pl', get(g:, 'pgsql_pl', []))
  call s:add_syntax(pl)
endfor

" Default highlighting
hi def link sqlCatalog        Constant
hi def link sqlComment        Comment
hi def link sqlConstant       Constant
hi def link sqlErrorCode      Special
hi def link sqlFunction       Function
hi def link sqlIdentifier     Identifier
hi def link sqlKeyword        sqlSpecial
hi def link sqlPlpgsqlKeyword sqlSpecial
hi def link sqlPlpgsqlVariable Identifier
hi def link sqlNumber         Number
hi def link sqlOperator       sqlStatement
hi def link sqlOption         Define
hi def link sqlSpecial        Special
hi def link sqlStatement      Statement
hi def link sqlString         String
hi def link sqlTable          Identifier
hi def link sqlType           Type
hi def link sqlView           sqlTable
hi def link sqlTodo           Todo
hi def link sqlPsqlCommand    SpecialKey
hi def link sqlPsqlKeyword    Keyword

let b:current_syntax = "sql"
$HERE$;

\o
select 'done!';
commit;
\o
