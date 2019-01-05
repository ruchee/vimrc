-- Statements
abort
alter
analyze
begin
checkpoint
close
cluster
comment
commit
constraints
copy
create
deallocate
declare
delete
discard
do
drop
end
execute
explain
fetch
grant
import
insert
label
listen
load
lock
move
notify
prepare
prepared
reassign
refresh
reindex
release
reset
revoke
rollback
savepoint
security
select
select
set
show
start
transaction
truncate
unlisten
update
vacuum
values
work
-- Types
abstime
aclitem
addbandarg
addr
addr_gid_seq
addrfeat
addrfeat_gid_seq
agg_count
agg_samealignment
anyarray
anyelement
anyenum
anynonarray
anyrange
bg
bg_gid_seq
bit
bool
box
box2d
box2df
box3d
bpchar
bytea
cardinal_number
char
character_data
cid
cidr
circle
citext
county
county_gid_seq
county_lookup
countysub_lookup
cousub
cousub_gid_seq
cstring
cube
date
daterange
dblink_pkey_results
direction_lookup
ean13
earth
edges
edges_gid_seq
errcodes
event_trigger
faces
faces_gid_seq
fdw_handler
featnames
featnames_gid_seq
float4
float8
gbtreekey16
gbtreekey32
gbtreekey4
gbtreekey8
gbtreekey_var
geocode_settings
geocode_settings_default
geography
geography_columns
geometry
geometry_columns
geometry_dump
geomval
getfaceedges_returntype
ghstore
gidx
gtrgm
gtsvector
hstore
index_am_handler
inet
int2
int2vector
int4
int4range
int8
int8range
intbig_gkey
internal
interval
isbn
isbn13
ismn
ismn13
issn
issn13
json
jsonb
language_handler
layer
line
lo
loader_lookuptables
loader_platform
loader_variables
lquery
lseg
ltree
ltree_gist
ltxtquery
macaddr
macaddr8
money
norm_addy
numeric
numrange
oid
oidvector
opaque
pagc_gaz
pagc_gaz_id_seq
pagc_lex
pagc_lex_id_seq
pagc_rules
pagc_rules_id_seq
path
pg_all_foreign_keys
pg_ddl_command
pg_dependencies
pg_lsn
pg_ndistinct
pg_node_tree
pgr_costresult
pgr_costresult3
pgr_geomresult
place
place_gid_seq
place_lookup
point
polygon
query_int
rastbandarg
raster
raster_columns
raster_overviews
reclassarg
record
refcursor
regclass
regconfig
regdictionary
regnamespace
regoper
regoperator
regproc
regprocedure
regrole
regtype
reltime
secondary_unit_lookup
seg
smgr
spatial_ref_sys
spheroid
sql_identifier
state
state_gid_seq
state_lookup
stdaddr
street_type_lookup
summarystats
tabblock
tabblock_gid_seq
tablefunc_crosstab_2
tablefunc_crosstab_3
tablefunc_crosstab_4
tap_funky
text
tid
time
time_stamp
timestamp
timestamptz
timetz
tinterval
topoelement
topoelementarray
topogeometry
topology
topology_id_seq
tract
tract_gid_seq
tsm_handler
tsquery
tsrange
tstzrange
tsvector
txid_snapshot
unionarg
upc
us_gaz
us_gaz_id_seq
us_lex
us_lex_id_seq
us_rules
us_rules_id_seq
uuid
valid_detail
validatetopology_returntype
varbit
varchar
void
xid
xml
yes_or_no
zcta5
zcta5_gid_seq
zip_lookup
zip_lookup_all
zip_lookup_base
zip_state
zip_state_loc
-- Additional types
array
bigint
bigserial
bit
boolean
char
character
cube
decimal
double
int
integer
interval
numeric
precision
real
serial
serial2
serial4
serial8
smallint
smallserial
text
timestamp
varchar
varying
xml
zone
pg_toast_1234
-- Built-in functions
RI_FKey_cascade_del()
RI_FKey_cascade_upd()
RI_FKey_check_ins()
RI_FKey_check_upd()
RI_FKey_noaction_del()
RI_FKey_noaction_upd()
RI_FKey_restrict_del()
RI_FKey_restrict_upd()
RI_FKey_setdefault_del()
RI_FKey_setdefault_upd()
RI_FKey_setnull_del()
RI_FKey_setnull_upd()
abbrev()
abs()
abstime()
abstimeeq()
abstimege()
abstimegt()
abstimein()
abstimele()
abstimelt()
abstimene()
abstimeout()
abstimerecv()
abstimesend()
aclcontains()
acldefault()
aclexplode()
aclinsert()
aclitemeq()
aclitemin()
aclitemout()
aclremove()
acos()
acosd()
age()
amvalidate()
any_in()
any_out()
anyarray_in()
anyarray_out()
anyarray_recv()
anyarray_send()
anyelement_in()
anyelement_out()
anyenum_in()
anyenum_out()
anynonarray_in()
anynonarray_out()
anyrange_in()
anyrange_out()
anytextcat()
area()
areajoinsel()
areasel()
array_agg()
array_agg_array_finalfn()
array_agg_array_transfn()
array_agg_finalfn()
array_agg_transfn()
array_append()
array_cat()
array_dims()
array_eq()
array_fill()
array_ge()
array_gt()
array_in()
array_larger()
array_le()
array_length()
array_lower()
array_lt()
array_ndims()
array_ne()
array_out()
array_position()
array_positions()
array_prepend()
array_recv()
array_remove()
array_replace()
array_send()
array_smaller()
array_to_json()
array_to_string()
array_to_tsvector()
array_typanalyze()
array_upper()
arraycontained()
arraycontains()
arraycontjoinsel()
arraycontsel()
arrayoverlap()
ascii()
ascii_to_mic()
ascii_to_utf8()
asin()
asind()
atan()
atan2()
atan2d()
atand()
avg()
bernoulli()
big5_to_euc_tw()
big5_to_mic()
big5_to_utf8()
binary_upgrade_create_empty_extension()
binary_upgrade_set_missing_value()
binary_upgrade_set_next_array_pg_type_oid()
binary_upgrade_set_next_heap_pg_class_oid()
binary_upgrade_set_next_index_pg_class_oid()
binary_upgrade_set_next_pg_authid_oid()
binary_upgrade_set_next_pg_enum_oid()
binary_upgrade_set_next_pg_type_oid()
binary_upgrade_set_next_toast_pg_class_oid()
binary_upgrade_set_next_toast_pg_type_oid()
binary_upgrade_set_record_init_privs()
bit()
bit_and()
bit_in()
bit_length()
bit_or()
bit_out()
bit_recv()
bit_send()
bitand()
bitcat()
bitcmp()
biteq()
bitge()
bitgt()
bitle()
bitlt()
bitne()
bitnot()
bitor()
bitshiftleft()
bitshiftright()
bittypmodin()
bittypmodout()
bitxor()
bool()
bool_accum()
bool_accum_inv()
bool_alltrue()
bool_and()
bool_anytrue()
bool_or()
booland_statefunc()
booleq()
boolge()
boolgt()
boolin()
boolle()
boollt()
boolne()
boolor_statefunc()
boolout()
boolrecv()
boolsend()
bound_box()
box()
box_above()
box_above_eq()
box_add()
box_below()
box_below_eq()
box_center()
box_contain()
box_contain_pt()
box_contained()
box_distance()
box_div()
box_eq()
box_ge()
box_gt()
box_in()
box_intersect()
box_le()
box_left()
box_lt()
box_mul()
box_out()
box_overabove()
box_overbelow()
box_overlap()
box_overleft()
box_overright()
box_recv()
box_right()
box_same()
box_send()
box_sub()
bpchar()
bpchar_larger()
bpchar_pattern_ge()
bpchar_pattern_gt()
bpchar_pattern_le()
bpchar_pattern_lt()
bpchar_smaller()
bpchar_sortsupport()
bpcharcmp()
bpchareq()
bpcharge()
bpchargt()
bpchariclike()
bpcharicnlike()
bpcharicregexeq()
bpcharicregexne()
bpcharin()
bpcharle()
bpcharlike()
bpcharlt()
bpcharne()
bpcharnlike()
bpcharout()
bpcharrecv()
bpcharregexeq()
bpcharregexne()
bpcharsend()
bpchartypmodin()
bpchartypmodout()
brin_desummarize_range()
brin_inclusion_add_value()
brin_inclusion_consistent()
brin_inclusion_opcinfo()
brin_inclusion_union()
brin_minmax_add_value()
brin_minmax_consistent()
brin_minmax_opcinfo()
brin_minmax_union()
brin_summarize_new_values()
brin_summarize_range()
brinhandler()
broadcast()
btabstimecmp()
btarraycmp()
btboolcmp()
btbpchar_pattern_cmp()
btbpchar_pattern_sortsupport()
btcharcmp()
btfloat48cmp()
btfloat4cmp()
btfloat4sortsupport()
btfloat84cmp()
btfloat8cmp()
btfloat8sortsupport()
bthandler()
btint24cmp()
btint28cmp()
btint2cmp()
btint2sortsupport()
btint42cmp()
btint48cmp()
btint4cmp()
btint4sortsupport()
btint82cmp()
btint84cmp()
btint8cmp()
btint8sortsupport()
btnamecmp()
btnamesortsupport()
btoidcmp()
btoidsortsupport()
btoidvectorcmp()
btrecordcmp()
btrecordimagecmp()
btreltimecmp()
btrim()
bttext_pattern_cmp()
bttext_pattern_sortsupport()
bttextcmp()
bttextsortsupport()
bttidcmp()
bttintervalcmp()
bytea_sortsupport()
bytea_string_agg_finalfn()
bytea_string_agg_transfn()
byteacat()
byteacmp()
byteaeq()
byteage()
byteagt()
byteain()
byteale()
bytealike()
bytealt()
byteane()
byteanlike()
byteaout()
bytearecv()
byteasend()
cardinality()
cash_cmp()
cash_div_cash()
cash_div_flt4()
cash_div_flt8()
cash_div_int2()
cash_div_int4()
cash_div_int8()
cash_eq()
cash_ge()
cash_gt()
cash_in()
cash_le()
cash_lt()
cash_mi()
cash_mul_flt4()
cash_mul_flt8()
cash_mul_int2()
cash_mul_int4()
cash_mul_int8()
cash_ne()
cash_out()
cash_pl()
cash_recv()
cash_send()
cash_words()
cashlarger()
cashsmaller()
cbrt()
ceil()
ceiling()
center()
char()
char_length()
character_length()
chareq()
charge()
chargt()
charin()
charle()
charlt()
charne()
charout()
charrecv()
charsend()
chr()
cideq()
cidin()
cidout()
cidr()
cidr_in()
cidr_out()
cidr_recv()
cidr_send()
cidrecv()
cidsend()
circle()
circle_above()
circle_add_pt()
circle_below()
circle_center()
circle_contain()
circle_contain_pt()
circle_contained()
circle_distance()
circle_div_pt()
circle_eq()
circle_ge()
circle_gt()
circle_in()
circle_le()
circle_left()
circle_lt()
circle_mul_pt()
circle_ne()
circle_out()
circle_overabove()
circle_overbelow()
circle_overlap()
circle_overleft()
circle_overright()
circle_recv()
circle_right()
circle_same()
circle_send()
circle_sub_pt()
clock_timestamp()
close_lb()
close_ls()
close_lseg()
close_pb()
close_pl()
close_ps()
close_sb()
close_sl()
col_description()
concat()
concat_ws()
contjoinsel()
contsel()
convert()
convert_from()
convert_to()
corr()
cos()
cosd()
cot()
cotd()
count()
covar_pop()
covar_samp()
cstring_in()
cstring_out()
cstring_recv()
cstring_send()
cume_dist()
cume_dist_final()
current_database()
current_query()
current_schema()
current_schemas()
current_setting()
current_user()
currtid()
currtid2()
currval()
cursor_to_xml()
cursor_to_xmlschema()
database_to_xml()
database_to_xml_and_xmlschema()
database_to_xmlschema()
date()
date_cmp()
date_cmp_timestamp()
date_cmp_timestamptz()
date_eq()
date_eq_timestamp()
date_eq_timestamptz()
date_ge()
date_ge_timestamp()
date_ge_timestamptz()
date_gt()
date_gt_timestamp()
date_gt_timestamptz()
date_in()
date_larger()
date_le()
date_le_timestamp()
date_le_timestamptz()
date_lt()
date_lt_timestamp()
date_lt_timestamptz()
date_mi()
date_mi_interval()
date_mii()
date_ne()
date_ne_timestamp()
date_ne_timestamptz()
date_out()
date_part()
date_pl_interval()
date_pli()
date_recv()
date_send()
date_smaller()
date_sortsupport()
date_trunc()
daterange()
daterange_canonical()
daterange_subdiff()
datetime_pl()
datetimetz_pl()
dcbrt()
decode()
degrees()
dense_rank()
dense_rank_final()
dexp()
diagonal()
diameter()
dispell_init()
dispell_lexize()
dist_cpoint()
dist_cpoly()
dist_lb()
dist_pb()
dist_pc()
dist_pl()
dist_polyp()
dist_ppath()
dist_ppoly()
dist_ps()
dist_sb()
dist_sl()
div()
dlog1()
dlog10()
domain_in()
domain_recv()
dpow()
dround()
dsimple_init()
dsimple_lexize()
dsnowball_init()
dsnowball_lexize()
dsqrt()
dsynonym_init()
dsynonym_lexize()
dtrunc()
elem_contained_by_range()
encode()
enum_cmp()
enum_eq()
enum_first()
enum_ge()
enum_gt()
enum_in()
enum_larger()
enum_last()
enum_le()
enum_lt()
enum_ne()
enum_out()
enum_range()
enum_recv()
enum_send()
enum_smaller()
eqjoinsel()
eqsel()
euc_cn_to_mic()
euc_cn_to_utf8()
euc_jis_2004_to_shift_jis_2004()
euc_jis_2004_to_utf8()
euc_jp_to_mic()
euc_jp_to_sjis()
euc_jp_to_utf8()
euc_kr_to_mic()
euc_kr_to_utf8()
euc_tw_to_big5()
euc_tw_to_mic()
euc_tw_to_utf8()
event_trigger_in()
event_trigger_out()
every()
exp()
factorial()
family()
fdw_handler_in()
fdw_handler_out()
first_value()
float4()
float48div()
float48eq()
float48ge()
float48gt()
float48le()
float48lt()
float48mi()
float48mul()
float48ne()
float48pl()
float4_accum()
float4abs()
float4div()
float4eq()
float4ge()
float4gt()
float4in()
float4larger()
float4le()
float4lt()
float4mi()
float4mul()
float4ne()
float4out()
float4pl()
float4recv()
float4send()
float4smaller()
float4um()
float4up()
float8()
float84div()
float84eq()
float84ge()
float84gt()
float84le()
float84lt()
float84mi()
float84mul()
float84ne()
float84pl()
float8_accum()
float8_avg()
float8_combine()
float8_corr()
float8_covar_pop()
float8_covar_samp()
float8_regr_accum()
float8_regr_avgx()
float8_regr_avgy()
float8_regr_combine()
float8_regr_intercept()
float8_regr_r2()
float8_regr_slope()
float8_regr_sxx()
float8_regr_sxy()
float8_regr_syy()
float8_stddev_pop()
float8_stddev_samp()
float8_var_pop()
float8_var_samp()
float8abs()
float8div()
float8eq()
float8ge()
float8gt()
float8in()
float8larger()
float8le()
float8lt()
float8mi()
float8mul()
float8ne()
float8out()
float8pl()
float8recv()
float8send()
float8smaller()
float8um()
float8up()
floor()
flt4_mul_cash()
flt8_mul_cash()
fmgr_c_validator()
fmgr_internal_validator()
fmgr_sql_validator()
format()
format_type()
gb18030_to_utf8()
gbk_to_utf8()
generate_series()
generate_subscripts()
get_bit()
get_byte()
get_current_ts_config()
getdatabaseencoding()
getpgusername()
gin_clean_pending_list()
gin_cmp_prefix()
gin_cmp_tslexeme()
gin_compare_jsonb()
gin_consistent_jsonb()
gin_consistent_jsonb_path()
gin_extract_jsonb()
gin_extract_jsonb_path()
gin_extract_jsonb_query()
gin_extract_jsonb_query_path()
gin_extract_tsquery()
gin_extract_tsvector()
gin_triconsistent_jsonb()
gin_triconsistent_jsonb_path()
gin_tsquery_consistent()
gin_tsquery_triconsistent()
ginarrayconsistent()
ginarrayextract()
ginarraytriconsistent()
ginhandler()
ginqueryarrayextract()
gist_box_consistent()
gist_box_penalty()
gist_box_picksplit()
gist_box_same()
gist_box_union()
gist_circle_compress()
gist_circle_consistent()
gist_circle_distance()
gist_point_compress()
gist_point_consistent()
gist_point_distance()
gist_point_fetch()
gist_poly_compress()
gist_poly_consistent()
gist_poly_distance()
gisthandler()
gtsquery_compress()
gtsquery_consistent()
gtsquery_penalty()
gtsquery_picksplit()
gtsquery_same()
gtsquery_union()
gtsvector_compress()
gtsvector_consistent()
gtsvector_decompress()
gtsvector_penalty()
gtsvector_picksplit()
gtsvector_same()
gtsvector_union()
gtsvectorin()
gtsvectorout()
has_any_column_privilege()
has_column_privilege()
has_database_privilege()
has_foreign_data_wrapper_privilege()
has_function_privilege()
has_language_privilege()
has_schema_privilege()
has_sequence_privilege()
has_server_privilege()
has_table_privilege()
has_tablespace_privilege()
has_type_privilege()
hash_aclitem()
hash_aclitem_extended()
hash_array()
hash_array_extended()
hash_numeric()
hash_numeric_extended()
hash_range()
hash_range_extended()
hashbpchar()
hashbpcharextended()
hashchar()
hashcharextended()
hashenum()
hashenumextended()
hashfloat4()
hashfloat4extended()
hashfloat8()
hashfloat8extended()
hashhandler()
hashinet()
hashinetextended()
hashint2()
hashint2extended()
hashint4()
hashint4extended()
hashint8()
hashint8extended()
hashmacaddr()
hashmacaddr8()
hashmacaddr8extended()
hashmacaddrextended()
hashname()
hashnameextended()
hashoid()
hashoidextended()
hashoidvector()
hashoidvectorextended()
hashtext()
hashtextextended()
hashvarlena()
hashvarlenaextended()
height()
host()
hostmask()
iclikejoinsel()
iclikesel()
icnlikejoinsel()
icnlikesel()
icregexeqjoinsel()
icregexeqsel()
icregexnejoinsel()
icregexnesel()
in_range()
index_am_handler_in()
index_am_handler_out()
inet_client_addr()
inet_client_port()
inet_gist_compress()
inet_gist_consistent()
inet_gist_fetch()
inet_gist_penalty()
inet_gist_picksplit()
inet_gist_same()
inet_gist_union()
inet_in()
inet_merge()
inet_out()
inet_recv()
inet_same_family()
inet_send()
inet_server_addr()
inet_server_port()
inet_spg_choose()
inet_spg_config()
inet_spg_inner_consistent()
inet_spg_leaf_consistent()
inet_spg_picksplit()
inetand()
inetmi()
inetmi_int8()
inetnot()
inetor()
inetpl()
initcap()
int2()
int24div()
int24eq()
int24ge()
int24gt()
int24le()
int24lt()
int24mi()
int24mul()
int24ne()
int24pl()
int28div()
int28eq()
int28ge()
int28gt()
int28le()
int28lt()
int28mi()
int28mul()
int28ne()
int28pl()
int2_accum()
int2_accum_inv()
int2_avg_accum()
int2_avg_accum_inv()
int2_mul_cash()
int2_sum()
int2abs()
int2and()
int2div()
int2eq()
int2ge()
int2gt()
int2in()
int2int4_sum()
int2larger()
int2le()
int2lt()
int2mi()
int2mod()
int2mul()
int2ne()
int2not()
int2or()
int2out()
int2pl()
int2recv()
int2send()
int2shl()
int2shr()
int2smaller()
int2um()
int2up()
int2vectorin()
int2vectorout()
int2vectorrecv()
int2vectorsend()
int2xor()
int4()
int42div()
int42eq()
int42ge()
int42gt()
int42le()
int42lt()
int42mi()
int42mul()
int42ne()
int42pl()
int48div()
int48eq()
int48ge()
int48gt()
int48le()
int48lt()
int48mi()
int48mul()
int48ne()
int48pl()
int4_accum()
int4_accum_inv()
int4_avg_accum()
int4_avg_accum_inv()
int4_avg_combine()
int4_mul_cash()
int4_sum()
int4abs()
int4and()
int4div()
int4eq()
int4ge()
int4gt()
int4in()
int4inc()
int4larger()
int4le()
int4lt()
int4mi()
int4mod()
int4mul()
int4ne()
int4not()
int4or()
int4out()
int4pl()
int4range()
int4range_canonical()
int4range_subdiff()
int4recv()
int4send()
int4shl()
int4shr()
int4smaller()
int4um()
int4up()
int4xor()
int8()
int82div()
int82eq()
int82ge()
int82gt()
int82le()
int82lt()
int82mi()
int82mul()
int82ne()
int82pl()
int84div()
int84eq()
int84ge()
int84gt()
int84le()
int84lt()
int84mi()
int84mul()
int84ne()
int84pl()
int8_accum()
int8_accum_inv()
int8_avg()
int8_avg_accum()
int8_avg_accum_inv()
int8_avg_combine()
int8_avg_deserialize()
int8_avg_serialize()
int8_mul_cash()
int8_sum()
int8abs()
int8and()
int8dec()
int8dec_any()
int8div()
int8eq()
int8ge()
int8gt()
int8in()
int8inc()
int8inc_any()
int8inc_float8_float8()
int8larger()
int8le()
int8lt()
int8mi()
int8mod()
int8mul()
int8ne()
int8not()
int8or()
int8out()
int8pl()
int8pl_inet()
int8range()
int8range_canonical()
int8range_subdiff()
int8recv()
int8send()
int8shl()
int8shr()
int8smaller()
int8um()
int8up()
int8xor()
integer_pl_date()
inter_lb()
inter_sb()
inter_sl()
internal_in()
internal_out()
interval()
interval_accum()
interval_accum_inv()
interval_avg()
interval_cmp()
interval_combine()
interval_div()
interval_eq()
interval_ge()
interval_gt()
interval_hash()
interval_hash_extended()
interval_in()
interval_larger()
interval_le()
interval_lt()
interval_mi()
interval_mul()
interval_ne()
interval_out()
interval_pl()
interval_pl_date()
interval_pl_time()
interval_pl_timestamp()
interval_pl_timestamptz()
interval_pl_timetz()
interval_recv()
interval_send()
interval_smaller()
interval_transform()
interval_um()
intervaltypmodin()
intervaltypmodout()
intinterval()
isclosed()
isempty()
isfinite()
ishorizontal()
iso8859_1_to_utf8()
iso8859_to_utf8()
iso_to_koi8r()
iso_to_mic()
iso_to_win1251()
iso_to_win866()
isopen()
isparallel()
isperp()
isvertical()
johab_to_utf8()
json_agg()
json_agg_finalfn()
json_agg_transfn()
json_array_element()
json_array_element_text()
json_array_elements()
json_array_elements_text()
json_array_length()
json_build_array()
json_build_object()
json_each()
json_each_text()
json_extract_path()
json_extract_path_text()
json_in()
json_object()
json_object_agg()
json_object_agg_finalfn()
json_object_agg_transfn()
json_object_field()
json_object_field_text()
json_object_keys()
json_out()
json_populate_record()
json_populate_recordset()
json_recv()
json_send()
json_strip_nulls()
json_to_record()
json_to_recordset()
json_to_tsvector()
json_typeof()
jsonb_agg()
jsonb_agg_finalfn()
jsonb_agg_transfn()
jsonb_array_element()
jsonb_array_element_text()
jsonb_array_elements()
jsonb_array_elements_text()
jsonb_array_length()
jsonb_build_array()
jsonb_build_object()
jsonb_cmp()
jsonb_concat()
jsonb_contained()
jsonb_contains()
jsonb_delete()
jsonb_delete_path()
jsonb_each()
jsonb_each_text()
jsonb_eq()
jsonb_exists()
jsonb_exists_all()
jsonb_exists_any()
jsonb_extract_path()
jsonb_extract_path_text()
jsonb_ge()
jsonb_gt()
jsonb_hash()
jsonb_hash_extended()
jsonb_in()
jsonb_insert()
jsonb_le()
jsonb_lt()
jsonb_ne()
jsonb_object()
jsonb_object_agg()
jsonb_object_agg_finalfn()
jsonb_object_agg_transfn()
jsonb_object_field()
jsonb_object_field_text()
jsonb_object_keys()
jsonb_out()
jsonb_populate_record()
jsonb_populate_recordset()
jsonb_pretty()
jsonb_recv()
jsonb_send()
jsonb_set()
jsonb_strip_nulls()
jsonb_to_record()
jsonb_to_recordset()
jsonb_to_tsvector()
jsonb_typeof()
justify_days()
justify_hours()
justify_interval()
koi8r_to_iso()
koi8r_to_mic()
koi8r_to_utf8()
koi8r_to_win1251()
koi8r_to_win866()
koi8u_to_utf8()
lag()
language_handler_in()
language_handler_out()
last_value()
lastval()
latin1_to_mic()
latin2_to_mic()
latin2_to_win1250()
latin3_to_mic()
latin4_to_mic()
lead()
left()
length()
like()
like_escape()
likejoinsel()
likesel()
line()
line_distance()
line_eq()
line_horizontal()
line_in()
line_interpt()
line_intersect()
line_out()
line_parallel()
line_perp()
line_recv()
line_send()
line_vertical()
ln()
lo_close()
lo_creat()
lo_create()
lo_export()
lo_from_bytea()
lo_get()
lo_import()
lo_lseek()
lo_lseek64()
lo_open()
lo_put()
lo_tell()
lo_tell64()
lo_truncate()
lo_truncate64()
lo_unlink()
log()
loread()
lower()
lower_inc()
lower_inf()
lowrite()
lpad()
lseg()
lseg_center()
lseg_distance()
lseg_eq()
lseg_ge()
lseg_gt()
lseg_horizontal()
lseg_in()
lseg_interpt()
lseg_intersect()
lseg_le()
lseg_length()
lseg_lt()
lseg_ne()
lseg_out()
lseg_parallel()
lseg_perp()
lseg_recv()
lseg_send()
lseg_vertical()
ltrim()
macaddr()
macaddr8()
macaddr8_and()
macaddr8_cmp()
macaddr8_eq()
macaddr8_ge()
macaddr8_gt()
macaddr8_in()
macaddr8_le()
macaddr8_lt()
macaddr8_ne()
macaddr8_not()
macaddr8_or()
macaddr8_out()
macaddr8_recv()
macaddr8_send()
macaddr8_set7bit()
macaddr_and()
macaddr_cmp()
macaddr_eq()
macaddr_ge()
macaddr_gt()
macaddr_in()
macaddr_le()
macaddr_lt()
macaddr_ne()
macaddr_not()
macaddr_or()
macaddr_out()
macaddr_recv()
macaddr_send()
macaddr_sortsupport()
make_date()
make_interval()
make_time()
make_timestamp()
make_timestamptz()
makeaclitem()
masklen()
max()
md5()
mic_to_ascii()
mic_to_big5()
mic_to_euc_cn()
mic_to_euc_jp()
mic_to_euc_kr()
mic_to_euc_tw()
mic_to_iso()
mic_to_koi8r()
mic_to_latin1()
mic_to_latin2()
mic_to_latin3()
mic_to_latin4()
mic_to_sjis()
mic_to_win1250()
mic_to_win1251()
mic_to_win866()
min()
mktinterval()
mod()
mode()
mode_final()
money()
mul_d_interval()
mxid_age()
name()
nameeq()
namege()
namegt()
nameiclike()
nameicnlike()
nameicregexeq()
nameicregexne()
namein()
namele()
namelike()
namelt()
namene()
namenlike()
nameout()
namerecv()
nameregexeq()
nameregexne()
namesend()
neqjoinsel()
neqsel()
netmask()
network()
network_cmp()
network_eq()
network_ge()
network_gt()
network_larger()
network_le()
network_lt()
network_ne()
network_overlap()
network_smaller()
network_sub()
network_subeq()
network_sup()
network_supeq()
networkjoinsel()
networksel()
nextval()
nlikejoinsel()
nlikesel()
notlike()
now()
npoints()
nth_value()
ntile()
num_nonnulls()
num_nulls()
numeric()
numeric_abs()
numeric_accum()
numeric_accum_inv()
numeric_add()
numeric_avg()
numeric_avg_accum()
numeric_avg_combine()
numeric_avg_deserialize()
numeric_avg_serialize()
numeric_cmp()
numeric_combine()
numeric_deserialize()
numeric_div()
numeric_div_trunc()
numeric_eq()
numeric_exp()
numeric_fac()
numeric_ge()
numeric_gt()
numeric_in()
numeric_inc()
numeric_larger()
numeric_le()
numeric_ln()
numeric_log()
numeric_lt()
numeric_mod()
numeric_mul()
numeric_ne()
numeric_out()
numeric_poly_avg()
numeric_poly_combine()
numeric_poly_deserialize()
numeric_poly_serialize()
numeric_poly_stddev_pop()
numeric_poly_stddev_samp()
numeric_poly_sum()
numeric_poly_var_pop()
numeric_poly_var_samp()
numeric_power()
numeric_recv()
numeric_send()
numeric_serialize()
numeric_smaller()
numeric_sortsupport()
numeric_sqrt()
numeric_stddev_pop()
numeric_stddev_samp()
numeric_sub()
numeric_sum()
numeric_transform()
numeric_uminus()
numeric_uplus()
numeric_var_pop()
numeric_var_samp()
numerictypmodin()
numerictypmodout()
numnode()
numrange()
numrange_subdiff()
obj_description()
octet_length()
oid()
oideq()
oidge()
oidgt()
oidin()
oidlarger()
oidle()
oidlt()
oidne()
oidout()
oidrecv()
oidsend()
oidsmaller()
oidvectoreq()
oidvectorge()
oidvectorgt()
oidvectorin()
oidvectorle()
oidvectorlt()
oidvectorne()
oidvectorout()
oidvectorrecv()
oidvectorsend()
oidvectortypes()
on_pb()
on_pl()
on_ppath()
on_ps()
on_sb()
on_sl()
opaque_in()
opaque_out()
ordered_set_transition()
ordered_set_transition_multi()
overlaps()
overlay()
parse_ident()
path()
path_add()
path_add_pt()
path_center()
path_contain_pt()
path_distance()
path_div_pt()
path_in()
path_inter()
path_length()
path_mul_pt()
path_n_eq()
path_n_ge()
path_n_gt()
path_n_le()
path_n_lt()
path_npoints()
path_out()
path_recv()
path_send()
path_sub_pt()
pclose()
percent_rank()
percent_rank_final()
percentile_cont()
percentile_cont_float8_final()
percentile_cont_float8_multi_final()
percentile_cont_interval_final()
percentile_cont_interval_multi_final()
percentile_disc()
percentile_disc_final()
percentile_disc_multi_final()
pg_advisory_lock()
pg_advisory_lock_shared()
pg_advisory_unlock()
pg_advisory_unlock_all()
pg_advisory_unlock_shared()
pg_advisory_xact_lock()
pg_advisory_xact_lock_shared()
pg_available_extension_versions()
pg_available_extensions()
pg_backend_pid()
pg_backup_start_time()
pg_blocking_pids()
pg_cancel_backend()
pg_char_to_encoding()
pg_client_encoding()
pg_collation_actual_version()
pg_collation_for()
pg_collation_is_visible()
pg_column_is_updatable()
pg_column_size()
pg_conf_load_time()
pg_config()
pg_control_checkpoint()
pg_control_init()
pg_control_recovery()
pg_control_system()
pg_conversion_is_visible()
pg_create_logical_replication_slot()
pg_create_physical_replication_slot()
pg_create_restore_point()
pg_current_logfile()
pg_current_wal_flush_lsn()
pg_current_wal_insert_lsn()
pg_current_wal_lsn()
pg_cursor()
pg_database_size()
pg_ddl_command_in()
pg_ddl_command_out()
pg_ddl_command_recv()
pg_ddl_command_send()
pg_dependencies_in()
pg_dependencies_out()
pg_dependencies_recv()
pg_dependencies_send()
pg_describe_object()
pg_drop_replication_slot()
pg_encoding_max_length()
pg_encoding_to_char()
pg_event_trigger_ddl_commands()
pg_event_trigger_dropped_objects()
pg_event_trigger_table_rewrite_oid()
pg_event_trigger_table_rewrite_reason()
pg_export_snapshot()
pg_extension_config_dump()
pg_extension_update_paths()
pg_file_rename()
pg_file_unlink()
pg_file_write()
pg_filenode_relation()
pg_function_is_visible()
pg_get_constraintdef()
pg_get_expr()
pg_get_function_arg_default()
pg_get_function_arguments()
pg_get_function_identity_arguments()
pg_get_function_result()
pg_get_functiondef()
pg_get_indexdef()
pg_get_keywords()
pg_get_multixact_members()
pg_get_object_address()
pg_get_partition_constraintdef()
pg_get_partkeydef()
pg_get_publication_tables()
pg_get_replica_identity_index()
pg_get_replication_slots()
pg_get_ruledef()
pg_get_serial_sequence()
pg_get_statisticsobjdef()
pg_get_triggerdef()
pg_get_userbyid()
pg_get_viewdef()
pg_has_role()
pg_hba_file_rules()
pg_identify_object()
pg_identify_object_as_address()
pg_import_system_collations()
pg_index_column_has_property()
pg_index_has_property()
pg_indexam_has_property()
pg_indexes_size()
pg_is_in_backup()
pg_is_in_recovery()
pg_is_other_temp_schema()
pg_is_wal_replay_paused()
pg_isolation_test_session_is_blocked()
pg_jit_available()
pg_last_committed_xact()
pg_last_wal_receive_lsn()
pg_last_wal_replay_lsn()
pg_last_xact_replay_timestamp()
pg_listening_channels()
pg_lock_status()
pg_logdir_ls()
pg_logical_emit_message()
pg_logical_slot_get_binary_changes()
pg_logical_slot_get_changes()
pg_logical_slot_peek_binary_changes()
pg_logical_slot_peek_changes()
pg_ls_dir()
pg_ls_logdir()
pg_ls_waldir()
pg_lsn_cmp()
pg_lsn_eq()
pg_lsn_ge()
pg_lsn_gt()
pg_lsn_hash()
pg_lsn_hash_extended()
pg_lsn_in()
pg_lsn_le()
pg_lsn_lt()
pg_lsn_mi()
pg_lsn_ne()
pg_lsn_out()
pg_lsn_recv()
pg_lsn_send()
pg_my_temp_schema()
pg_ndistinct_in()
pg_ndistinct_out()
pg_ndistinct_recv()
pg_ndistinct_send()
pg_node_tree_in()
pg_node_tree_out()
pg_node_tree_recv()
pg_node_tree_send()
pg_notification_queue_usage()
pg_notify()
pg_opclass_is_visible()
pg_operator_is_visible()
pg_opfamily_is_visible()
pg_options_to_table()
pg_postmaster_start_time()
pg_prepared_statement()
pg_prepared_xact()
pg_read_binary_file()
pg_read_file()
pg_read_file_old()
pg_relation_filenode()
pg_relation_filepath()
pg_relation_is_publishable()
pg_relation_is_updatable()
pg_relation_size()
pg_reload_conf()
pg_replication_origin_advance()
pg_replication_origin_create()
pg_replication_origin_drop()
pg_replication_origin_oid()
pg_replication_origin_progress()
pg_replication_origin_session_is_setup()
pg_replication_origin_session_progress()
pg_replication_origin_session_reset()
pg_replication_origin_session_setup()
pg_replication_origin_xact_reset()
pg_replication_origin_xact_setup()
pg_replication_slot_advance()
pg_rotate_logfile()
pg_rotate_logfile_old()
pg_safe_snapshot_blocking_pids()
pg_sequence_last_value()
pg_sequence_parameters()
pg_show_all_file_settings()
pg_show_all_settings()
pg_show_replication_origin_status()
pg_size_bytes()
pg_size_pretty()
pg_sleep()
pg_sleep_for()
pg_sleep_until()
pg_start_backup()
pg_stat_clear_snapshot()
pg_stat_file()
pg_stat_get_activity()
pg_stat_get_analyze_count()
pg_stat_get_archiver()
pg_stat_get_autoanalyze_count()
pg_stat_get_autovacuum_count()
pg_stat_get_backend_activity()
pg_stat_get_backend_activity_start()
pg_stat_get_backend_client_addr()
pg_stat_get_backend_client_port()
pg_stat_get_backend_dbid()
pg_stat_get_backend_idset()
pg_stat_get_backend_pid()
pg_stat_get_backend_start()
pg_stat_get_backend_userid()
pg_stat_get_backend_wait_event()
pg_stat_get_backend_wait_event_type()
pg_stat_get_backend_xact_start()
pg_stat_get_bgwriter_buf_written_checkpoints()
pg_stat_get_bgwriter_buf_written_clean()
pg_stat_get_bgwriter_maxwritten_clean()
pg_stat_get_bgwriter_requested_checkpoints()
pg_stat_get_bgwriter_stat_reset_time()
pg_stat_get_bgwriter_timed_checkpoints()
pg_stat_get_blocks_fetched()
pg_stat_get_blocks_hit()
pg_stat_get_buf_alloc()
pg_stat_get_buf_fsync_backend()
pg_stat_get_buf_written_backend()
pg_stat_get_checkpoint_sync_time()
pg_stat_get_checkpoint_write_time()
pg_stat_get_db_blk_read_time()
pg_stat_get_db_blk_write_time()
pg_stat_get_db_blocks_fetched()
pg_stat_get_db_blocks_hit()
pg_stat_get_db_conflict_all()
pg_stat_get_db_conflict_bufferpin()
pg_stat_get_db_conflict_lock()
pg_stat_get_db_conflict_snapshot()
pg_stat_get_db_conflict_startup_deadlock()
pg_stat_get_db_conflict_tablespace()
pg_stat_get_db_deadlocks()
pg_stat_get_db_numbackends()
pg_stat_get_db_stat_reset_time()
pg_stat_get_db_temp_bytes()
pg_stat_get_db_temp_files()
pg_stat_get_db_tuples_deleted()
pg_stat_get_db_tuples_fetched()
pg_stat_get_db_tuples_inserted()
pg_stat_get_db_tuples_returned()
pg_stat_get_db_tuples_updated()
pg_stat_get_db_xact_commit()
pg_stat_get_db_xact_rollback()
pg_stat_get_dead_tuples()
pg_stat_get_function_calls()
pg_stat_get_function_self_time()
pg_stat_get_function_total_time()
pg_stat_get_last_analyze_time()
pg_stat_get_last_autoanalyze_time()
pg_stat_get_last_autovacuum_time()
pg_stat_get_last_vacuum_time()
pg_stat_get_live_tuples()
pg_stat_get_mod_since_analyze()
pg_stat_get_numscans()
pg_stat_get_progress_info()
pg_stat_get_snapshot_timestamp()
pg_stat_get_subscription()
pg_stat_get_tuples_deleted()
pg_stat_get_tuples_fetched()
pg_stat_get_tuples_hot_updated()
pg_stat_get_tuples_inserted()
pg_stat_get_tuples_returned()
pg_stat_get_tuples_updated()
pg_stat_get_vacuum_count()
pg_stat_get_wal_receiver()
pg_stat_get_wal_senders()
pg_stat_get_xact_blocks_fetched()
pg_stat_get_xact_blocks_hit()
pg_stat_get_xact_function_calls()
pg_stat_get_xact_function_self_time()
pg_stat_get_xact_function_total_time()
pg_stat_get_xact_numscans()
pg_stat_get_xact_tuples_deleted()
pg_stat_get_xact_tuples_fetched()
pg_stat_get_xact_tuples_hot_updated()
pg_stat_get_xact_tuples_inserted()
pg_stat_get_xact_tuples_returned()
pg_stat_get_xact_tuples_updated()
pg_stat_reset()
pg_stat_reset_shared()
pg_stat_reset_single_function_counters()
pg_stat_reset_single_table_counters()
pg_statistics_obj_is_visible()
pg_stop_backup()
pg_switch_wal()
pg_table_is_visible()
pg_table_size()
pg_tablespace_databases()
pg_tablespace_location()
pg_tablespace_size()
pg_terminate_backend()
pg_timezone_abbrevs()
pg_timezone_names()
pg_total_relation_size()
pg_trigger_depth()
pg_try_advisory_lock()
pg_try_advisory_lock_shared()
pg_try_advisory_xact_lock()
pg_try_advisory_xact_lock_shared()
pg_ts_config_is_visible()
pg_ts_dict_is_visible()
pg_ts_parser_is_visible()
pg_ts_template_is_visible()
pg_type_is_visible()
pg_typeof()
pg_wal_lsn_diff()
pg_wal_replay_pause()
pg_wal_replay_resume()
pg_walfile_name()
pg_walfile_name_offset()
pg_xact_commit_timestamp()
phraseto_tsquery()
pi()
plainto_tsquery()
plperl_call_handler()
plperl_inline_handler()
plperl_validator()
plperlu_call_handler()
plperlu_inline_handler()
plperlu_validator()
plpgsql_call_handler()
plpgsql_inline_handler()
plpgsql_validator()
plpython3_call_handler()
plpython3_inline_handler()
plpython3_validator()
pltcl_call_handler()
pltclu_call_handler()
point()
point_above()
point_add()
point_below()
point_distance()
point_div()
point_eq()
point_horiz()
point_in()
point_left()
point_mul()
point_ne()
point_out()
point_recv()
point_right()
point_send()
point_sub()
point_vert()
poly_above()
poly_below()
poly_center()
poly_contain()
poly_contain_pt()
poly_contained()
poly_distance()
poly_in()
poly_left()
poly_npoints()
poly_out()
poly_overabove()
poly_overbelow()
poly_overlap()
poly_overleft()
poly_overright()
poly_recv()
poly_right()
poly_same()
poly_send()
polygon()
popen()
position()
positionjoinsel()
positionsel()
postgresql_fdw_validator()
pow()
power()
prefixjoinsel()
prefixsel()
prsd_end()
prsd_headline()
prsd_lextype()
prsd_nexttoken()
prsd_start()
pt_contained_circle()
pt_contained_poly()
query_to_xml()
query_to_xml_and_xmlschema()
query_to_xmlschema()
querytree()
quote_ident()
quote_literal()
quote_nullable()
radians()
radius()
random()
range_adjacent()
range_after()
range_before()
range_cmp()
range_contained_by()
range_contains()
range_contains_elem()
range_eq()
range_ge()
range_gist_consistent()
range_gist_penalty()
range_gist_picksplit()
range_gist_same()
range_gist_union()
range_gt()
range_in()
range_intersect()
range_le()
range_lt()
range_merge()
range_minus()
range_ne()
range_out()
range_overlaps()
range_overleft()
range_overright()
range_recv()
range_send()
range_typanalyze()
range_union()
rangesel()
rank()
rank_final()
record_eq()
record_ge()
record_gt()
record_image_eq()
record_image_ge()
record_image_gt()
record_image_le()
record_image_lt()
record_image_ne()
record_in()
record_le()
record_lt()
record_ne()
record_out()
record_recv()
record_send()
regclass()
regclassin()
regclassout()
regclassrecv()
regclasssend()
regconfigin()
regconfigout()
regconfigrecv()
regconfigsend()
regdictionaryin()
regdictionaryout()
regdictionaryrecv()
regdictionarysend()
regexeqjoinsel()
regexeqsel()
regexnejoinsel()
regexnesel()
regexp_match()
regexp_matches()
regexp_replace()
regexp_split_to_array()
regexp_split_to_table()
regnamespacein()
regnamespaceout()
regnamespacerecv()
regnamespacesend()
regoperatorin()
regoperatorout()
regoperatorrecv()
regoperatorsend()
regoperin()
regoperout()
regoperrecv()
regopersend()
regprocedurein()
regprocedureout()
regprocedurerecv()
regproceduresend()
regprocin()
regprocout()
regprocrecv()
regprocsend()
regr_avgx()
regr_avgy()
regr_count()
regr_intercept()
regr_r2()
regr_slope()
regr_sxx()
regr_sxy()
regr_syy()
regrolein()
regroleout()
regrolerecv()
regrolesend()
regtypein()
regtypeout()
regtyperecv()
regtypesend()
reltime()
reltimeeq()
reltimege()
reltimegt()
reltimein()
reltimele()
reltimelt()
reltimene()
reltimeout()
reltimerecv()
reltimesend()
repeat()
replace()
reverse()
right()
round()
row_number()
row_security_active()
row_to_json()
rpad()
rtrim()
satisfies_hash_partition()
scalargejoinsel()
scalargesel()
scalargtjoinsel()
scalargtsel()
scalarlejoinsel()
scalarlesel()
scalarltjoinsel()
scalarltsel()
scale()
schema_to_xml()
schema_to_xml_and_xmlschema()
schema_to_xmlschema()
session_user()
set_bit()
set_byte()
set_config()
set_masklen()
setseed()
setval()
setweight()
sha224()
sha256()
sha384()
sha512()
shell_in()
shell_out()
shift_jis_2004_to_euc_jis_2004()
shift_jis_2004_to_utf8()
shobj_description()
sign()
similar_escape()
sin()
sind()
sjis_to_euc_jp()
sjis_to_mic()
sjis_to_utf8()
slope()
smgreq()
smgrin()
smgrne()
smgrout()
spg_bbox_quad_config()
spg_box_quad_choose()
spg_box_quad_config()
spg_box_quad_inner_consistent()
spg_box_quad_leaf_consistent()
spg_box_quad_picksplit()
spg_kd_choose()
spg_kd_config()
spg_kd_inner_consistent()
spg_kd_picksplit()
spg_poly_quad_compress()
spg_quad_choose()
spg_quad_config()
spg_quad_inner_consistent()
spg_quad_leaf_consistent()
spg_quad_picksplit()
spg_range_quad_choose()
spg_range_quad_config()
spg_range_quad_inner_consistent()
spg_range_quad_leaf_consistent()
spg_range_quad_picksplit()
spg_text_choose()
spg_text_config()
spg_text_inner_consistent()
spg_text_leaf_consistent()
spg_text_picksplit()
spghandler()
split_part()
sqrt()
starts_with()
statement_timestamp()
stddev()
stddev_pop()
stddev_samp()
string_agg()
string_agg_finalfn()
string_agg_transfn()
string_to_array()
strip()
strpos()
substr()
substring()
sum()
suppress_redundant_updates_trigger()
system()
table_to_xml()
table_to_xml_and_xmlschema()
table_to_xmlschema()
tan()
tand()
text()
text_ge()
text_gt()
text_larger()
text_le()
text_lt()
text_pattern_ge()
text_pattern_gt()
text_pattern_le()
text_pattern_lt()
text_smaller()
textanycat()
textcat()
texteq()
texticlike()
texticnlike()
texticregexeq()
texticregexne()
textin()
textlen()
textlike()
textne()
textnlike()
textout()
textrecv()
textregexeq()
textregexne()
textsend()
thesaurus_init()
thesaurus_lexize()
tideq()
tidge()
tidgt()
tidin()
tidlarger()
tidle()
tidlt()
tidne()
tidout()
tidrecv()
tidsend()
tidsmaller()
time()
time_cmp()
time_eq()
time_ge()
time_gt()
time_hash()
time_hash_extended()
time_in()
time_larger()
time_le()
time_lt()
time_mi_interval()
time_mi_time()
time_ne()
time_out()
time_pl_interval()
time_recv()
time_send()
time_smaller()
time_transform()
timedate_pl()
timemi()
timenow()
timeofday()
timepl()
timestamp()
timestamp_cmp()
timestamp_cmp_date()
timestamp_cmp_timestamptz()
timestamp_eq()
timestamp_eq_date()
timestamp_eq_timestamptz()
timestamp_ge()
timestamp_ge_date()
timestamp_ge_timestamptz()
timestamp_gt()
timestamp_gt_date()
timestamp_gt_timestamptz()
timestamp_hash()
timestamp_hash_extended()
timestamp_in()
timestamp_izone_transform()
timestamp_larger()
timestamp_le()
timestamp_le_date()
timestamp_le_timestamptz()
timestamp_lt()
timestamp_lt_date()
timestamp_lt_timestamptz()
timestamp_mi()
timestamp_mi_interval()
timestamp_ne()
timestamp_ne_date()
timestamp_ne_timestamptz()
timestamp_out()
timestamp_pl_interval()
timestamp_recv()
timestamp_send()
timestamp_smaller()
timestamp_sortsupport()
timestamp_transform()
timestamp_zone_transform()
timestamptypmodin()
timestamptypmodout()
timestamptz()
timestamptz_cmp()
timestamptz_cmp_date()
timestamptz_cmp_timestamp()
timestamptz_eq()
timestamptz_eq_date()
timestamptz_eq_timestamp()
timestamptz_ge()
timestamptz_ge_date()
timestamptz_ge_timestamp()
timestamptz_gt()
timestamptz_gt_date()
timestamptz_gt_timestamp()
timestamptz_in()
timestamptz_larger()
timestamptz_le()
timestamptz_le_date()
timestamptz_le_timestamp()
timestamptz_lt()
timestamptz_lt_date()
timestamptz_lt_timestamp()
timestamptz_mi()
timestamptz_mi_interval()
timestamptz_ne()
timestamptz_ne_date()
timestamptz_ne_timestamp()
timestamptz_out()
timestamptz_pl_interval()
timestamptz_recv()
timestamptz_send()
timestamptz_smaller()
timestamptztypmodin()
timestamptztypmodout()
timetypmodin()
timetypmodout()
timetz()
timetz_cmp()
timetz_eq()
timetz_ge()
timetz_gt()
timetz_hash()
timetz_hash_extended()
timetz_in()
timetz_larger()
timetz_le()
timetz_lt()
timetz_mi_interval()
timetz_ne()
timetz_out()
timetz_pl_interval()
timetz_recv()
timetz_send()
timetz_smaller()
timetzdate_pl()
timetztypmodin()
timetztypmodout()
timezone()
tinterval()
tintervalct()
tintervalend()
tintervaleq()
tintervalge()
tintervalgt()
tintervalin()
tintervalle()
tintervalleneq()
tintervallenge()
tintervallengt()
tintervallenle()
tintervallenlt()
tintervallenne()
tintervallt()
tintervalne()
tintervalout()
tintervalov()
tintervalrecv()
tintervalrel()
tintervalsame()
tintervalsend()
tintervalstart()
to_ascii()
to_char()
to_date()
to_hex()
to_json()
to_jsonb()
to_number()
to_regclass()
to_regnamespace()
to_regoper()
to_regoperator()
to_regproc()
to_regprocedure()
to_regrole()
to_regtype()
to_timestamp()
to_tsquery()
to_tsvector()
transaction_timestamp()
translate()
trigger_in()
trigger_out()
trunc()
ts_debug()
ts_delete()
ts_filter()
ts_headline()
ts_lexize()
ts_match_qv()
ts_match_tq()
ts_match_tt()
ts_match_vq()
ts_parse()
ts_rank()
ts_rank_cd()
ts_rewrite()
ts_stat()
ts_token_type()
ts_typanalyze()
tsm_handler_in()
tsm_handler_out()
tsmatchjoinsel()
tsmatchsel()
tsq_mcontained()
tsq_mcontains()
tsquery_and()
tsquery_cmp()
tsquery_eq()
tsquery_ge()
tsquery_gt()
tsquery_le()
tsquery_lt()
tsquery_ne()
tsquery_not()
tsquery_or()
tsquery_phrase()
tsqueryin()
tsqueryout()
tsqueryrecv()
tsquerysend()
tsrange()
tsrange_subdiff()
tstzrange()
tstzrange_subdiff()
tsvector_cmp()
tsvector_concat()
tsvector_eq()
tsvector_ge()
tsvector_gt()
tsvector_le()
tsvector_lt()
tsvector_ne()
tsvector_to_array()
tsvector_update_trigger()
tsvector_update_trigger_column()
tsvectorin()
tsvectorout()
tsvectorrecv()
tsvectorsend()
txid_current()
txid_current_if_assigned()
txid_current_snapshot()
txid_snapshot_in()
txid_snapshot_out()
txid_snapshot_recv()
txid_snapshot_send()
txid_snapshot_xip()
txid_snapshot_xmax()
txid_snapshot_xmin()
txid_status()
txid_visible_in_snapshot()
uhc_to_utf8()
unique_key_recheck()
unknownin()
unknownout()
unknownrecv()
unknownsend()
unnest()
upper()
upper_inc()
upper_inf()
utf8_to_ascii()
utf8_to_big5()
utf8_to_euc_cn()
utf8_to_euc_jis_2004()
utf8_to_euc_jp()
utf8_to_euc_kr()
utf8_to_euc_tw()
utf8_to_gb18030()
utf8_to_gbk()
utf8_to_iso8859()
utf8_to_iso8859_1()
utf8_to_johab()
utf8_to_koi8r()
utf8_to_koi8u()
utf8_to_shift_jis_2004()
utf8_to_sjis()
utf8_to_uhc()
utf8_to_win()
uuid_cmp()
uuid_eq()
uuid_ge()
uuid_gt()
uuid_hash()
uuid_hash_extended()
uuid_in()
uuid_le()
uuid_lt()
uuid_ne()
uuid_out()
uuid_recv()
uuid_send()
uuid_sortsupport()
var_pop()
var_samp()
varbit()
varbit_in()
varbit_out()
varbit_recv()
varbit_send()
varbit_transform()
varbitcmp()
varbiteq()
varbitge()
varbitgt()
varbitle()
varbitlt()
varbitne()
varbittypmodin()
varbittypmodout()
varchar()
varchar_transform()
varcharin()
varcharout()
varcharrecv()
varcharsend()
varchartypmodin()
varchartypmodout()
variance()
version()
void_in()
void_out()
void_recv()
void_send()
websearch_to_tsquery()
width()
width_bucket()
win1250_to_latin2()
win1250_to_mic()
win1251_to_iso()
win1251_to_koi8r()
win1251_to_mic()
win1251_to_win866()
win866_to_iso()
win866_to_koi8r()
win866_to_mic()
win866_to_win1251()
win_to_utf8()
xideq()
xideqint4()
xidin()
xidneq()
xidneqint4()
xidout()
xidrecv()
xidsend()
xml()
xml_in()
xml_is_well_formed()
xml_is_well_formed_content()
xml_is_well_formed_document()
xml_out()
xml_recv()
xml_send()
xmlagg()
xmlcomment()
xmlconcat2()
xmlexists()
xmlvalidate()
xpath()
xpath_exists()
-- Extension: refint
check_foreign_key() -- function
check_primary_key() -- function
-- Extension: postgis
addauth() -- function
addgeometrycolumn() -- function
addoverviewconstraints() -- function
addrasterconstraints() -- function
box() -- function
box2d() -- function
box2d_in() -- function
box2d_out() -- function
box2df_in() -- function
box2df_out() -- function
box3d() -- function
box3d_in() -- function
box3d_out() -- function
box3dtobox() -- function
bytea() -- function
checkauth() -- function
checkauthtrigger() -- function
contains_2d() -- function
disablelongtransactions() -- function
dropgeometrycolumn() -- function
dropgeometrytable() -- function
dropoverviewconstraints() -- function
droprasterconstraints() -- function
enablelongtransactions() -- function
equals() -- function
find_srid() -- function
geog_brin_inclusion_add_value() -- function
geography() -- function
geography_analyze() -- function
geography_cmp() -- function
geography_distance_knn() -- function
geography_eq() -- function
geography_ge() -- function
geography_gist_compress() -- function
geography_gist_consistent() -- function
geography_gist_decompress() -- function
geography_gist_distance() -- function
geography_gist_penalty() -- function
geography_gist_picksplit() -- function
geography_gist_same() -- function
geography_gist_union() -- function
geography_gt() -- function
geography_in() -- function
geography_le() -- function
geography_lt() -- function
geography_out() -- function
geography_overlaps() -- function
geography_recv() -- function
geography_send() -- function
geography_typmod_in() -- function
geography_typmod_out() -- function
geom2d_brin_inclusion_add_value() -- function
geom3d_brin_inclusion_add_value() -- function
geom4d_brin_inclusion_add_value() -- function
geometry() -- function
geometry_above() -- function
geometry_analyze() -- function
geometry_below() -- function
geometry_cmp() -- function
geometry_contained_3d() -- function
geometry_contained_by_raster() -- function
geometry_contains() -- function
geometry_contains_3d() -- function
geometry_distance_box() -- function
geometry_distance_centroid() -- function
geometry_distance_centroid_nd() -- function
geometry_distance_cpa() -- function
geometry_eq() -- function
geometry_ge() -- function
geometry_gist_compress_2d() -- function
geometry_gist_compress_nd() -- function
geometry_gist_consistent_2d() -- function
geometry_gist_consistent_nd() -- function
geometry_gist_decompress_2d() -- function
geometry_gist_decompress_nd() -- function
geometry_gist_distance_2d() -- function
geometry_gist_distance_nd() -- function
geometry_gist_penalty_2d() -- function
geometry_gist_penalty_nd() -- function
geometry_gist_picksplit_2d() -- function
geometry_gist_picksplit_nd() -- function
geometry_gist_same_2d() -- function
geometry_gist_same_nd() -- function
geometry_gist_union_2d() -- function
geometry_gist_union_nd() -- function
geometry_gt() -- function
geometry_hash() -- function
geometry_in() -- function
geometry_le() -- function
geometry_left() -- function
geometry_lt() -- function
geometry_out() -- function
geometry_overabove() -- function
geometry_overbelow() -- function
geometry_overlaps() -- function
geometry_overlaps_3d() -- function
geometry_overlaps_nd() -- function
geometry_overleft() -- function
geometry_overright() -- function
geometry_raster_contain() -- function
geometry_raster_overlap() -- function
geometry_recv() -- function
geometry_right() -- function
geometry_same() -- function
geometry_same_3d() -- function
geometry_send() -- function
geometry_spgist_choose_2d() -- function
geometry_spgist_choose_3d() -- function
geometry_spgist_compress_2d() -- function
geometry_spgist_compress_3d() -- function
geometry_spgist_config_2d() -- function
geometry_spgist_config_3d() -- function
geometry_spgist_inner_consistent_2d() -- function
geometry_spgist_inner_consistent_3d() -- function
geometry_spgist_leaf_consistent_2d() -- function
geometry_spgist_leaf_consistent_3d() -- function
geometry_spgist_picksplit_2d() -- function
geometry_spgist_picksplit_3d() -- function
geometry_typmod_in() -- function
geometry_typmod_out() -- function
geometry_within() -- function
geometrytype() -- function
geomfromewkb() -- function
geomfromewkt() -- function
get_proj4_from_srid() -- function
gettransactionid() -- function
gidx_in() -- function
gidx_out() -- function
gserialized_gist_joinsel_2d() -- function
gserialized_gist_joinsel_nd() -- function
gserialized_gist_sel_2d() -- function
gserialized_gist_sel_nd() -- function
is_contained_2d() -- function
lockrow() -- function
longtransactionsenabled() -- function
overlaps_2d() -- function
overlaps_geog() -- function
overlaps_nd() -- function
path() -- function
pgis_asgeobuf_finalfn() -- function
pgis_asgeobuf_transfn() -- function
pgis_asmvt_combinefn() -- function
pgis_asmvt_deserialfn() -- function
pgis_asmvt_finalfn() -- function
pgis_asmvt_serialfn() -- function
pgis_asmvt_transfn() -- function
pgis_geometry_accum_finalfn() -- function
pgis_geometry_accum_transfn() -- function
pgis_geometry_clusterintersecting_finalfn() -- function
pgis_geometry_clusterwithin_finalfn() -- function
pgis_geometry_collect_finalfn() -- function
pgis_geometry_makeline_finalfn() -- function
pgis_geometry_polygonize_finalfn() -- function
pgis_geometry_union_finalfn() -- function
point() -- function
polygon() -- function
populate_geometry_columns() -- function
postgis_addbbox() -- function
postgis_cache_bbox() -- function
postgis_constraint_dims() -- function
postgis_constraint_srid() -- function
postgis_constraint_type() -- function
postgis_dropbbox() -- function
postgis_extensions_upgrade() -- function
postgis_full_version() -- function
postgis_gdal_version() -- function
postgis_geos_version() -- function
postgis_getbbox() -- function
postgis_hasbbox() -- function
postgis_lib_build_date() -- function
postgis_lib_version() -- function
postgis_libjson_version() -- function
postgis_liblwgeom_version() -- function
postgis_libprotobuf_version() -- function
postgis_libxml_version() -- function
postgis_noop() -- function
postgis_proj_version() -- function
postgis_raster_lib_build_date() -- function
postgis_raster_lib_version() -- function
postgis_raster_scripts_installed() -- function
postgis_scripts_build_date() -- function
postgis_scripts_installed() -- function
postgis_scripts_released() -- function
postgis_svn_version() -- function
postgis_transform_geometry() -- function
postgis_type_name() -- function
postgis_typmod_dims() -- function
postgis_typmod_srid() -- function
postgis_typmod_type() -- function
postgis_version() -- function
raster_above() -- function
raster_below() -- function
raster_contain() -- function
raster_contained() -- function
raster_contained_by_geometry() -- function
raster_eq() -- function
raster_geometry_contain() -- function
raster_geometry_overlap() -- function
raster_hash() -- function
raster_in() -- function
raster_left() -- function
raster_out() -- function
raster_overabove() -- function
raster_overbelow() -- function
raster_overlap() -- function
raster_overleft() -- function
raster_overright() -- function
raster_right() -- function
raster_same() -- function
spheroid_in() -- function
spheroid_out() -- function
st_3dclosestpoint() -- function
st_3ddfullywithin() -- function
st_3ddistance() -- function
st_3ddwithin() -- function
st_3dextent() -- function
st_3dintersects() -- function
st_3dlength() -- function
st_3dlength_spheroid() -- function
st_3dlongestline() -- function
st_3dmakebox() -- function
st_3dmaxdistance() -- function
st_3dperimeter() -- function
st_3dshortestline() -- function
st_accum() -- function
st_addband() -- function
st_addmeasure() -- function
st_addpoint() -- function
st_affine() -- function
st_angle() -- function
st_approxcount() -- function
st_approxhistogram() -- function
st_approxquantile() -- function
st_approxsummarystats() -- function
st_area() -- function
st_area2d() -- function
st_asbinary() -- function
st_asencodedpolyline() -- function
st_asewkb() -- function
st_asewkt() -- function
st_asgdalraster() -- function
st_asgeobuf() -- function
st_asgeojson() -- function
st_asgml() -- function
st_ashexewkb() -- function
st_ashexwkb() -- function
st_asjpeg() -- function
st_askml() -- function
st_aslatlontext() -- function
st_asmvt() -- function
st_asmvtgeom() -- function
st_aspect() -- function
st_aspng() -- function
st_asraster() -- function
st_assvg() -- function
st_astext() -- function
st_astiff() -- function
st_astwkb() -- function
st_aswkb() -- function
st_asx3d() -- function
st_azimuth() -- function
st_band() -- function
st_bandfilesize() -- function
st_bandfiletimestamp() -- function
st_bandisnodata() -- function
st_bandmetadata() -- function
st_bandnodatavalue() -- function
st_bandpath() -- function
st_bandpixeltype() -- function
st_bdmpolyfromtext() -- function
st_bdpolyfromtext() -- function
st_boundary() -- function
st_boundingdiagonal() -- function
st_box2dfromgeohash() -- function
st_buffer() -- function
st_buildarea() -- function
st_centroid() -- function
st_chaikinsmoothing() -- function
st_cleangeometry() -- function
st_clip() -- function
st_clipbybox2d() -- function
st_closestpoint() -- function
st_closestpointofapproach() -- function
st_clusterdbscan() -- function
st_clusterintersecting() -- function
st_clusterkmeans() -- function
st_clusterwithin() -- function
st_collect() -- function
st_collectionextract() -- function
st_collectionhomogenize() -- function
st_colormap() -- function
st_combine_bbox() -- function
st_combinebbox() -- function
st_concavehull() -- function
st_contains() -- function
st_containsproperly() -- function
st_convexhull() -- function
st_coorddim() -- function
st_count() -- function
st_countagg() -- function
st_coveredby() -- function
st_covers() -- function
st_cpawithin() -- function
st_createoverview() -- function
st_crosses() -- function
st_curvetoline() -- function
st_delaunaytriangles() -- function
st_dfullywithin() -- function
st_difference() -- function
st_dimension() -- function
st_disjoint() -- function
st_distance() -- function
st_distance_sphere() -- function
st_distance_spheroid() -- function
st_distancecpa() -- function
st_distancesphere() -- function
st_distancespheroid() -- function
st_distinct4ma() -- function
st_dump() -- function
st_dumpaspolygons() -- function
st_dumppoints() -- function
st_dumprings() -- function
st_dumpvalues() -- function
st_dwithin() -- function
st_endpoint() -- function
st_envelope() -- function
st_equals() -- function
st_estimated_extent() -- function
st_estimatedextent() -- function
st_expand() -- function
st_extent() -- function
st_exteriorring() -- function
st_filterbym() -- function
st_find_extent() -- function
st_findextent() -- function
st_flipcoordinates() -- function
st_force2d() -- function
st_force3d() -- function
st_force3dm() -- function
st_force3dz() -- function
st_force4d() -- function
st_force_2d() -- function
st_force_3d() -- function
st_force_3dm() -- function
st_force_3dz() -- function
st_force_4d() -- function
st_force_collection() -- function
st_forcecollection() -- function
st_forcecurve() -- function
st_forcepolygonccw() -- function
st_forcepolygoncw() -- function
st_forcerhr() -- function
st_forcesfs() -- function
st_frechetdistance() -- function
st_fromgdalraster() -- function
st_gdaldrivers() -- function
st_generatepoints() -- function
st_geogfromtext() -- function
st_geogfromwkb() -- function
st_geographyfromtext() -- function
st_geohash() -- function
st_geomcollfromtext() -- function
st_geomcollfromwkb() -- function
st_geometricmedian() -- function
st_geometryfromtext() -- function
st_geometryn() -- function
st_geometrytype() -- function
st_geomfromewkb() -- function
st_geomfromewkt() -- function
st_geomfromgeohash() -- function
st_geomfromgeojson() -- function
st_geomfromgml() -- function
st_geomfromkml() -- function
st_geomfromtext() -- function
st_geomfromtwkb() -- function
st_geomfromwkb() -- function
st_georeference() -- function
st_geotransform() -- function
st_gmltosql() -- function
st_grayscale() -- function
st_hasarc() -- function
st_hasnoband() -- function
st_hausdorffdistance() -- function
st_height() -- function
st_hillshade() -- function
st_histogram() -- function
st_interiorringn() -- function
st_interpolatepoint() -- function
st_intersection() -- function
st_intersects() -- function
st_invdistweight4ma() -- function
st_isclosed() -- function
st_iscollection() -- function
st_iscoveragetile() -- function
st_isempty() -- function
st_ispolygonccw() -- function
st_ispolygoncw() -- function
st_isring() -- function
st_issimple() -- function
st_isvalid() -- function
st_isvaliddetail() -- function
st_isvalidreason() -- function
st_isvalidtrajectory() -- function
st_length() -- function
st_length2d() -- function
st_length2d_spheroid() -- function
st_length2dspheroid() -- function
st_length_spheroid() -- function
st_lengthspheroid() -- function
st_line_interpolate_point() -- function
st_line_locate_point() -- function
st_line_substring() -- function
st_linecrossingdirection() -- function
st_linefromencodedpolyline() -- function
st_linefrommultipoint() -- function
st_linefromtext() -- function
st_linefromwkb() -- function
st_lineinterpolatepoint() -- function
st_lineinterpolatepoints() -- function
st_linelocatepoint() -- function
st_linemerge() -- function
st_linestringfromwkb() -- function
st_linesubstring() -- function
st_linetocurve() -- function
st_locate_along_measure() -- function
st_locate_between_measures() -- function
st_locatealong() -- function
st_locatebetween() -- function
st_locatebetweenelevations() -- function
st_longestline() -- function
st_m() -- function
st_makebox2d() -- function
st_makeemptycoverage() -- function
st_makeemptyraster() -- function
st_makeenvelope() -- function
st_makeline() -- function
st_makepoint() -- function
st_makepointm() -- function
st_makepolygon() -- function
st_makevalid() -- function
st_mapalgebra() -- function
st_mapalgebraexpr() -- function
st_mapalgebrafct() -- function
st_mapalgebrafctngb() -- function
st_max4ma() -- function
st_maxdistance() -- function
st_mean4ma() -- function
st_mem_size() -- function
st_memcollect() -- function
st_memsize() -- function
st_memunion() -- function
st_metadata() -- function
st_min4ma() -- function
st_minconvexhull() -- function
st_mindist4ma() -- function
st_minimumboundingcircle() -- function
st_minimumboundingradius() -- function
st_minimumclearance() -- function
st_minimumclearanceline() -- function
st_minpossiblevalue() -- function
st_mlinefromtext() -- function
st_mlinefromwkb() -- function
st_mpointfromtext() -- function
st_mpointfromwkb() -- function
st_mpolyfromtext() -- function
st_mpolyfromwkb() -- function
st_multi() -- function
st_multilinefromwkb() -- function
st_multilinestringfromtext() -- function
st_multipointfromtext() -- function
st_multipointfromwkb() -- function
st_multipolyfromwkb() -- function
st_multipolygonfromtext() -- function
st_ndims() -- function
st_nearestvalue() -- function
st_neighborhood() -- function
st_node() -- function
st_normalize() -- function
st_notsamealignmentreason() -- function
st_npoints() -- function
st_nrings() -- function
st_numbands() -- function
st_numgeometries() -- function
st_numinteriorring() -- function
st_numinteriorrings() -- function
st_numpatches() -- function
st_numpoints() -- function
st_offsetcurve() -- function
st_orderingequals() -- function
st_orientedenvelope() -- function
st_overlaps() -- function
st_patchn() -- function
st_perimeter() -- function
st_perimeter2d() -- function
st_pixelascentroid() -- function
st_pixelascentroids() -- function
st_pixelaspoint() -- function
st_pixelaspoints() -- function
st_pixelaspolygon() -- function
st_pixelaspolygons() -- function
st_pixelheight() -- function
st_pixelofvalue() -- function
st_pixelwidth() -- function
st_point() -- function
st_point_inside_circle() -- function
st_pointfromgeohash() -- function
st_pointfromtext() -- function
st_pointfromwkb() -- function
st_pointinsidecircle() -- function
st_pointn() -- function
st_pointonsurface() -- function
st_points() -- function
st_polyfromtext() -- function
st_polyfromwkb() -- function
st_polygon() -- function
st_polygonfromtext() -- function
st_polygonfromwkb() -- function
st_polygonize() -- function
st_project() -- function
st_quantile() -- function
st_quantizecoordinates() -- function
st_range4ma() -- function
st_rastertoworldcoord() -- function
st_rastertoworldcoordx() -- function
st_rastertoworldcoordy() -- function
st_rastfromhexwkb() -- function
st_rastfromwkb() -- function
st_reclass() -- function
st_relate() -- function
st_relatematch() -- function
st_removepoint() -- function
st_removerepeatedpoints() -- function
st_resample() -- function
st_rescale() -- function
st_resize() -- function
st_reskew() -- function
st_retile() -- function
st_reverse() -- function
st_rotate() -- function
st_rotatex() -- function
st_rotatey() -- function
st_rotatez() -- function
st_rotation() -- function
st_roughness() -- function
st_samealignment() -- function
st_scale() -- function
st_scalex() -- function
st_scaley() -- function
st_segmentize() -- function
st_setbandindex() -- function
st_setbandisnodata() -- function
st_setbandnodatavalue() -- function
st_setbandpath() -- function
st_seteffectivearea() -- function
st_setgeoreference() -- function
st_setgeotransform() -- function
st_setpoint() -- function
st_setrotation() -- function
st_setscale() -- function
st_setskew() -- function
st_setsrid() -- function
st_setupperleft() -- function
st_setvalue() -- function
st_setvalues() -- function
st_sharedpaths() -- function
st_shift_longitude() -- function
st_shiftlongitude() -- function
st_shortestline() -- function
st_simplify() -- function
st_simplifypreservetopology() -- function
st_simplifyvw() -- function
st_skewx() -- function
st_skewy() -- function
st_slope() -- function
st_snap() -- function
st_snaptogrid() -- function
st_split() -- function
st_srid() -- function
st_startpoint() -- function
st_stddev4ma() -- function
st_subdivide() -- function
st_sum4ma() -- function
st_summary() -- function
st_summarystats() -- function
st_summarystatsagg() -- function
st_swapordinates() -- function
st_symdifference() -- function
st_symmetricdifference() -- function
st_tile() -- function
st_touches() -- function
st_tpi() -- function
st_transform() -- function
st_translate() -- function
st_transscale() -- function
st_tri() -- function
st_unaryunion() -- function
st_union() -- function
st_upperleftx() -- function
st_upperlefty() -- function
st_value() -- function
st_valuecount() -- function
st_valuepercent() -- function
st_voronoilines() -- function
st_voronoipolygons() -- function
st_width() -- function
st_within() -- function
st_wkbtosql() -- function
st_wkttosql() -- function
st_worldtorastercoord() -- function
st_worldtorastercoordx() -- function
st_worldtorastercoordy() -- function
st_wrapx() -- function
st_x() -- function
st_xmax() -- function
st_xmin() -- function
st_y() -- function
st_ymax() -- function
st_ymin() -- function
st_z() -- function
st_zmax() -- function
st_zmflag() -- function
st_zmin() -- function
text() -- function
unlockrows() -- function
updategeometrysrid() -- function
updaterastersrid() -- function
spatial_ref_sys -- table
addbandarg -- type
agg_count -- type
agg_samealignment -- type
box2d -- type
box2df -- type
box3d -- type
geography -- type
geometry -- type
geometry_dump -- type
geomval -- type
gidx -- type
rastbandarg -- type
raster -- type
reclassarg -- type
spheroid -- type
summarystats -- type
unionarg -- type
valid_detail -- type
geography_columns -- view
geometry_columns -- view
raster_columns -- view
raster_overviews -- view
-- Extension: unaccent
unaccent() -- function
unaccent_init() -- function
unaccent_lexize() -- function
-- Extension: btree_gin
gin_btree_consistent() -- function
gin_compare_prefix_anyenum() -- function
gin_compare_prefix_bit() -- function
gin_compare_prefix_bool() -- function
gin_compare_prefix_bpchar() -- function
gin_compare_prefix_bytea() -- function
gin_compare_prefix_char() -- function
gin_compare_prefix_cidr() -- function
gin_compare_prefix_date() -- function
gin_compare_prefix_float4() -- function
gin_compare_prefix_float8() -- function
gin_compare_prefix_inet() -- function
gin_compare_prefix_int2() -- function
gin_compare_prefix_int4() -- function
gin_compare_prefix_int8() -- function
gin_compare_prefix_interval() -- function
gin_compare_prefix_macaddr() -- function
gin_compare_prefix_macaddr8() -- function
gin_compare_prefix_money() -- function
gin_compare_prefix_name() -- function
gin_compare_prefix_numeric() -- function
gin_compare_prefix_oid() -- function
gin_compare_prefix_text() -- function
gin_compare_prefix_time() -- function
gin_compare_prefix_timestamp() -- function
gin_compare_prefix_timestamptz() -- function
gin_compare_prefix_timetz() -- function
gin_compare_prefix_uuid() -- function
gin_compare_prefix_varbit() -- function
gin_enum_cmp() -- function
gin_extract_query_anyenum() -- function
gin_extract_query_bit() -- function
gin_extract_query_bool() -- function
gin_extract_query_bpchar() -- function
gin_extract_query_bytea() -- function
gin_extract_query_char() -- function
gin_extract_query_cidr() -- function
gin_extract_query_date() -- function
gin_extract_query_float4() -- function
gin_extract_query_float8() -- function
gin_extract_query_inet() -- function
gin_extract_query_int2() -- function
gin_extract_query_int4() -- function
gin_extract_query_int8() -- function
gin_extract_query_interval() -- function
gin_extract_query_macaddr() -- function
gin_extract_query_macaddr8() -- function
gin_extract_query_money() -- function
gin_extract_query_name() -- function
gin_extract_query_numeric() -- function
gin_extract_query_oid() -- function
gin_extract_query_text() -- function
gin_extract_query_time() -- function
gin_extract_query_timestamp() -- function
gin_extract_query_timestamptz() -- function
gin_extract_query_timetz() -- function
gin_extract_query_uuid() -- function
gin_extract_query_varbit() -- function
gin_extract_value_anyenum() -- function
gin_extract_value_bit() -- function
gin_extract_value_bool() -- function
gin_extract_value_bpchar() -- function
gin_extract_value_bytea() -- function
gin_extract_value_char() -- function
gin_extract_value_cidr() -- function
gin_extract_value_date() -- function
gin_extract_value_float4() -- function
gin_extract_value_float8() -- function
gin_extract_value_inet() -- function
gin_extract_value_int2() -- function
gin_extract_value_int4() -- function
gin_extract_value_int8() -- function
gin_extract_value_interval() -- function
gin_extract_value_macaddr() -- function
gin_extract_value_macaddr8() -- function
gin_extract_value_money() -- function
gin_extract_value_name() -- function
gin_extract_value_numeric() -- function
gin_extract_value_oid() -- function
gin_extract_value_text() -- function
gin_extract_value_time() -- function
gin_extract_value_timestamp() -- function
gin_extract_value_timestamptz() -- function
gin_extract_value_timetz() -- function
gin_extract_value_uuid() -- function
gin_extract_value_varbit() -- function
gin_numeric_cmp() -- function
-- Extension: plpython3u
plpython3_call_handler() -- function
plpython3_inline_handler() -- function
plpython3_validator() -- function
-- Extension: ltree
index() -- function
lca() -- function
lquery_in() -- function
lquery_out() -- function
lt_q_regex() -- function
lt_q_rregex() -- function
ltq_regex() -- function
ltq_rregex() -- function
ltree2text() -- function
ltree_addltree() -- function
ltree_addtext() -- function
ltree_cmp() -- function
ltree_compress() -- function
ltree_consistent() -- function
ltree_decompress() -- function
ltree_eq() -- function
ltree_ge() -- function
ltree_gist_in() -- function
ltree_gist_out() -- function
ltree_gt() -- function
ltree_in() -- function
ltree_isparent() -- function
ltree_le() -- function
ltree_lt() -- function
ltree_ne() -- function
ltree_out() -- function
ltree_penalty() -- function
ltree_picksplit() -- function
ltree_risparent() -- function
ltree_same() -- function
ltree_textadd() -- function
ltree_union() -- function
ltreeparentsel() -- function
ltxtq_exec() -- function
ltxtq_in() -- function
ltxtq_out() -- function
ltxtq_rexec() -- function
nlevel() -- function
subltree() -- function
subpath() -- function
text2ltree() -- function
lquery -- type
ltree -- type
ltree_gist -- type
ltxtquery -- type
-- Extension: tsm_system_rows
system_rows() -- function
-- Extension: temporal_tables
set_system_time() -- function
versioning() -- function
-- Extension: jsonb_plperl
jsonb_to_plperl() -- function
plperl_to_jsonb() -- function
-- Extension: adminpack
pg_file_rename() -- function
pg_file_unlink() -- function
pg_file_write() -- function
pg_logdir_ls() -- function
-- Extension: dict_xsyn
dxsyn_init() -- function
dxsyn_lexize() -- function
-- Extension: address_standardizer
parse_address() -- function
standardize_address() -- function
stdaddr -- type
-- Extension: hstore_plperlu
hstore_to_plperlu() -- function
plperlu_to_hstore() -- function
-- Extension: xml2
xml_encode_special_chars() -- function
xml_valid() -- function
xpath_bool() -- function
xpath_list() -- function
xpath_nodeset() -- function
xpath_number() -- function
xpath_string() -- function
xpath_table() -- function
xslt_process() -- function
-- Extension: hstore
akeys() -- function
avals() -- function
defined() -- function
delete() -- function
each() -- function
exist() -- function
exists_all() -- function
exists_any() -- function
fetchval() -- function
ghstore_compress() -- function
ghstore_consistent() -- function
ghstore_decompress() -- function
ghstore_in() -- function
ghstore_out() -- function
ghstore_penalty() -- function
ghstore_picksplit() -- function
ghstore_same() -- function
ghstore_union() -- function
gin_consistent_hstore() -- function
gin_extract_hstore() -- function
gin_extract_hstore_query() -- function
hs_concat() -- function
hs_contained() -- function
hs_contains() -- function
hstore() -- function
hstore_cmp() -- function
hstore_eq() -- function
hstore_ge() -- function
hstore_gt() -- function
hstore_hash() -- function
hstore_in() -- function
hstore_le() -- function
hstore_lt() -- function
hstore_ne() -- function
hstore_out() -- function
hstore_recv() -- function
hstore_send() -- function
hstore_to_array() -- function
hstore_to_json() -- function
hstore_to_json_loose() -- function
hstore_to_jsonb() -- function
hstore_to_jsonb_loose() -- function
hstore_to_matrix() -- function
hstore_version_diag() -- function
isdefined() -- function
isexists() -- function
populate_record() -- function
skeys() -- function
slice() -- function
slice_array() -- function
svals() -- function
tconvert() -- function
ghstore -- type
hstore -- type
-- Extension: pg_visibility
pg_check_frozen() -- function
pg_check_visible() -- function
pg_truncate_visibility_map() -- function
pg_visibility() -- function
pg_visibility_map() -- function
pg_visibility_map_summary() -- function
-- Extension: cube
cube() -- function
cube_cmp() -- function
cube_contained() -- function
cube_contains() -- function
cube_coord() -- function
cube_coord_llur() -- function
cube_dim() -- function
cube_distance() -- function
cube_enlarge() -- function
cube_eq() -- function
cube_ge() -- function
cube_gt() -- function
cube_in() -- function
cube_inter() -- function
cube_is_point() -- function
cube_le() -- function
cube_ll_coord() -- function
cube_lt() -- function
cube_ne() -- function
cube_out() -- function
cube_overlap() -- function
cube_size() -- function
cube_subset() -- function
cube_union() -- function
cube_ur_coord() -- function
distance_chebyshev() -- function
distance_taxicab() -- function
g_cube_consistent() -- function
g_cube_distance() -- function
g_cube_penalty() -- function
g_cube_picksplit() -- function
g_cube_same() -- function
g_cube_union() -- function
cube -- type
-- Extension: postgis_tiger_geocoder
count_words() -- function
create_census_base_tables() -- function
cull_null() -- function
diff_zip() -- function
drop_dupe_featnames_generate_script() -- function
drop_indexes_generate_script() -- function
drop_nation_tables_generate_script() -- function
drop_state_tables_generate_script() -- function
end_soundex() -- function
geocode() -- function
geocode_address() -- function
geocode_intersection() -- function
geocode_location() -- function
get_geocode_setting() -- function
get_last_words() -- function
get_tract() -- function
greatest_hn() -- function
includes_address() -- function
install_geocode_settings() -- function
install_missing_indexes() -- function
install_pagc_tables() -- function
interpolate_from_address() -- function
is_pretype() -- function
least_hn() -- function
levenshtein_ignore_case() -- function
loader_generate_census_script() -- function
loader_generate_nation_script() -- function
loader_generate_script() -- function
loader_load_staged_data() -- function
loader_macro_replace() -- function
location_extract() -- function
location_extract_countysub_exact() -- function
location_extract_countysub_fuzzy() -- function
location_extract_place_exact() -- function
location_extract_place_fuzzy() -- function
missing_indexes_generate_script() -- function
normalize_address() -- function
nullable_levenshtein() -- function
numeric_streets_equal() -- function
pagc_normalize_address() -- function
pprint_addy() -- function
rate_attributes() -- function
reverse_geocode() -- function
set_geocode_setting() -- function
setsearchpathforinstall() -- function
state_extract() -- function
topology_load_tiger() -- function
utmzone() -- function
zip_range() -- function
addr -- table
addrfeat -- table
bg -- table
county -- table
county_lookup -- table
countysub_lookup -- table
cousub -- table
direction_lookup -- table
edges -- table
faces -- table
featnames -- table
geocode_settings -- table
geocode_settings_default -- table
loader_lookuptables -- table
loader_platform -- table
loader_variables -- table
pagc_gaz -- table
pagc_lex -- table
pagc_rules -- table
place -- table
place_lookup -- table
secondary_unit_lookup -- table
state -- table
state_lookup -- table
street_type_lookup -- table
tabblock -- table
tract -- table
zcta5 -- table
zip_lookup -- table
zip_lookup_all -- table
zip_lookup_base -- table
zip_state -- table
zip_state_loc -- table
norm_addy -- type
-- Extension: seg
gseg_consistent() -- function
gseg_penalty() -- function
gseg_picksplit() -- function
gseg_same() -- function
gseg_union() -- function
seg_center() -- function
seg_cmp() -- function
seg_contained() -- function
seg_contains() -- function
seg_different() -- function
seg_ge() -- function
seg_gt() -- function
seg_in() -- function
seg_inter() -- function
seg_le() -- function
seg_left() -- function
seg_lower() -- function
seg_lt() -- function
seg_out() -- function
seg_over_left() -- function
seg_over_right() -- function
seg_overlap() -- function
seg_right() -- function
seg_same() -- function
seg_size() -- function
seg_union() -- function
seg_upper() -- function
seg -- type
-- Extension: intagg
int_agg_final_array() -- function
int_agg_state() -- function
int_array_aggregate() -- function
int_array_enum() -- function
-- Extension: tcn
triggered_change_notification() -- function
-- Extension: isn
btean13cmp() -- function
btisbn13cmp() -- function
btisbncmp() -- function
btismn13cmp() -- function
btismncmp() -- function
btissn13cmp() -- function
btissncmp() -- function
btupccmp() -- function
ean13_in() -- function
ean13_out() -- function
hashean13() -- function
hashisbn() -- function
hashisbn13() -- function
hashismn() -- function
hashismn13() -- function
hashissn() -- function
hashissn13() -- function
hashupc() -- function
is_valid() -- function
isbn() -- function
isbn13() -- function
isbn13_in() -- function
isbn_in() -- function
ismn() -- function
ismn13() -- function
ismn13_in() -- function
ismn_in() -- function
isn_out() -- function
isn_weak() -- function
isneq() -- function
isnge() -- function
isngt() -- function
isnle() -- function
isnlt() -- function
isnne() -- function
issn() -- function
issn13() -- function
issn13_in() -- function
issn_in() -- function
make_valid() -- function
upc() -- function
upc_in() -- function
ean13 -- type
isbn -- type
isbn13 -- type
ismn -- type
ismn13 -- type
issn -- type
issn13 -- type
upc -- type
-- Extension: tsm_system_time
system_time() -- function
-- Extension: lo
lo_manage() -- function
lo_oid() -- function
lo -- type
-- Extension: pgrowlocks
pgrowlocks() -- function
-- Extension: jsonb_plpython3u
jsonb_to_plpython3() -- function
plpython3_to_jsonb() -- function
-- Extension: sslinfo
ssl_cipher() -- function
ssl_client_cert_present() -- function
ssl_client_dn() -- function
ssl_client_dn_field() -- function
ssl_client_serial() -- function
ssl_extension_info() -- function
ssl_is_used() -- function
ssl_issuer_dn() -- function
ssl_issuer_field() -- function
ssl_version() -- function
-- Extension: pgstattuple
pg_relpages() -- function
pgstatginindex() -- function
pgstathashindex() -- function
pgstatindex() -- function
pgstattuple() -- function
pgstattuple_approx() -- function
-- Extension: autoinc
autoinc() -- function
-- Extension: address_standardizer_data_us
us_gaz -- table
us_lex -- table
us_rules -- table
-- Extension: hstore_plpython3u
hstore_to_plpython3() -- function
plpython3_to_hstore() -- function
-- Extension: postgis_topology
addedge() -- function
addface() -- function
addnode() -- function
addtopogeometrycolumn() -- function
addtosearchpath() -- function
asgml() -- function
astopojson() -- function
cleartopogeom() -- function
copytopology() -- function
createtopogeom() -- function
createtopology() -- function
droptopogeometrycolumn() -- function
droptopology() -- function
equals() -- function
geometry() -- function
geometrytype() -- function
getedgebypoint() -- function
getfacebypoint() -- function
getnodebypoint() -- function
getnodeedges() -- function
getringedges() -- function
gettopogeomelementarray() -- function
gettopogeomelements() -- function
gettopologyid() -- function
gettopologyname() -- function
gettopologysrid() -- function
intersects() -- function
layertrigger() -- function
polygonize() -- function
populate_topology_layer() -- function
postgis_topology_scripts_installed() -- function
relationtrigger() -- function
st_addedgemodface() -- function
st_addedgenewfaces() -- function
st_addisoedge() -- function
st_addisonode() -- function
st_changeedgegeom() -- function
st_createtopogeo() -- function
st_geometrytype() -- function
st_getfaceedges() -- function
st_getfacegeometry() -- function
st_inittopogeo() -- function
st_modedgeheal() -- function
st_modedgesplit() -- function
st_moveisonode() -- function
st_newedgeheal() -- function
st_newedgessplit() -- function
st_remedgemodface() -- function
st_remedgenewface() -- function
st_remisonode() -- function
st_removeisoedge() -- function
st_removeisonode() -- function
st_simplify() -- function
topoelementarray_agg() -- function
topoelementarray_append() -- function
topogeo_addgeometry() -- function
topogeo_addlinestring() -- function
topogeo_addpoint() -- function
topogeo_addpolygon() -- function
topogeom_addelement() -- function
topogeom_remelement() -- function
topologysummary() -- function
totopogeom() -- function
validatetopology() -- function
layer -- table
topology -- table
getfaceedges_returntype -- type
topoelement -- type
topoelementarray -- type
topogeometry -- type
validatetopology_returntype -- type
-- Extension: pg_freespacemap
pg_freespace() -- function
-- Extension: file_fdw
file_fdw_handler() -- function
file_fdw_validator() -- function
-- Extension: pg_buffercache
pg_buffercache_pages() -- function
pg_buffercache -- view
-- Extension: dblink
dblink() -- function
dblink_build_sql_delete() -- function
dblink_build_sql_insert() -- function
dblink_build_sql_update() -- function
dblink_cancel_query() -- function
dblink_close() -- function
dblink_connect() -- function
dblink_connect_u() -- function
dblink_current_query() -- function
dblink_disconnect() -- function
dblink_error_message() -- function
dblink_exec() -- function
dblink_fdw_validator() -- function
dblink_fetch() -- function
dblink_get_connections() -- function
dblink_get_notify() -- function
dblink_get_pkey() -- function
dblink_get_result() -- function
dblink_is_busy() -- function
dblink_open() -- function
dblink_send_query() -- function
dblink_pkey_results -- type
-- Extension: pg_stat_statements
pg_stat_statements() -- function
pg_stat_statements_reset() -- function
pg_stat_statements -- view
-- Extension: insert_username
insert_username() -- function
-- Extension: pg_prewarm
autoprewarm_dump_now() -- function
autoprewarm_start_worker() -- function
pg_prewarm() -- function
-- Extension: pgtap
is() -- function
add_result() -- function
alike() -- function
any_column_privs_are() -- function
bag_eq() -- function
bag_has() -- function
bag_hasnt() -- function
bag_ne() -- function
can() -- function
cast_context_is() -- function
casts_are() -- function
check_test() -- function
cmp_ok() -- function
col_default_is() -- function
col_has_check() -- function
col_has_default() -- function
col_hasnt_default() -- function
col_is_fk() -- function
col_is_null() -- function
col_is_pk() -- function
col_is_unique() -- function
col_isnt_fk() -- function
col_isnt_pk() -- function
col_not_null() -- function
col_type_is() -- function
collect_tap() -- function
column_privs_are() -- function
columns_are() -- function
composite_owner_is() -- function
database_privs_are() -- function
db_owner_is() -- function
diag() -- function
diag_test_name() -- function
display_oper() -- function
do_tap() -- function
doesnt_imatch() -- function
doesnt_match() -- function
domain_type_is() -- function
domain_type_isnt() -- function
domains_are() -- function
enum_has_labels() -- function
enums_are() -- function
extensions_are() -- function
fail() -- function
fdw_privs_are() -- function
findfuncs() -- function
finish() -- function
fk_ok() -- function
foreign_table_owner_is() -- function
foreign_tables_are() -- function
function_lang_is() -- function
function_owner_is() -- function
function_privs_are() -- function
function_returns() -- function
functions_are() -- function
groups_are() -- function
has_cast() -- function
has_check() -- function
has_column() -- function
has_composite() -- function
has_domain() -- function
has_enum() -- function
has_extension() -- function
has_fk() -- function
has_foreign_table() -- function
has_function() -- function
has_group() -- function
has_index() -- function
has_inherited_tables() -- function
has_language() -- function
has_leftop() -- function
has_materialized_view() -- function
has_opclass() -- function
has_operator() -- function
has_pk() -- function
has_relation() -- function
has_rightop() -- function
has_role() -- function
has_rule() -- function
has_schema() -- function
has_sequence() -- function
has_table() -- function
has_tablespace() -- function
has_trigger() -- function
has_type() -- function
has_unique() -- function
has_user() -- function
has_view() -- function
hasnt_cast() -- function
hasnt_column() -- function
hasnt_composite() -- function
hasnt_domain() -- function
hasnt_enum() -- function
hasnt_extension() -- function
hasnt_fk() -- function
hasnt_foreign_table() -- function
hasnt_function() -- function
hasnt_group() -- function
hasnt_index() -- function
hasnt_inherited_tables() -- function
hasnt_language() -- function
hasnt_materialized_view() -- function
hasnt_opclass() -- function
hasnt_pk() -- function
hasnt_relation() -- function
hasnt_role() -- function
hasnt_rule() -- function
hasnt_schema() -- function
hasnt_sequence() -- function
hasnt_table() -- function
hasnt_tablespace() -- function
hasnt_trigger() -- function
hasnt_type() -- function
hasnt_user() -- function
hasnt_view() -- function
ialike() -- function
imatches() -- function
in_todo() -- function
index_is_primary() -- function
index_is_type() -- function
index_is_unique() -- function
index_owner_is() -- function
indexes_are() -- function
is_aggregate() -- function
is_ancestor_of() -- function
is_clustered() -- function
is_definer() -- function
is_descendent_of() -- function
is_empty() -- function
is_indexed() -- function
is_member_of() -- function
is_partition_of() -- function
is_partitioned() -- function
is_strict() -- function
is_superuser() -- function
isa_ok() -- function
isnt() -- function
isnt_aggregate() -- function
isnt_ancestor_of() -- function
isnt_definer() -- function
isnt_descendent_of() -- function
isnt_empty() -- function
isnt_partitioned() -- function
isnt_strict() -- function
isnt_superuser() -- function
language_is_trusted() -- function
language_owner_is() -- function
language_privs_are() -- function
languages_are() -- function
lives_ok() -- function
matches() -- function
materialized_view_owner_is() -- function
materialized_views_are() -- function
no_plan() -- function
num_failed() -- function
ok() -- function
opclass_owner_is() -- function
opclasses_are() -- function
operators_are() -- function
os_name() -- function
partitions_are() -- function
pass() -- function
performs_ok() -- function
performs_within() -- function
pg_version() -- function
pg_version_num() -- function
pgtap_version() -- function
plan() -- function
policies_are() -- function
policy_cmd_is() -- function
policy_roles_are() -- function
relation_owner_is() -- function
results_eq() -- function
results_ne() -- function
roles_are() -- function
row_eq() -- function
rule_is_instead() -- function
rule_is_on() -- function
rules_are() -- function
runtests() -- function
schema_owner_is() -- function
schema_privs_are() -- function
schemas_are() -- function
sequence_owner_is() -- function
sequence_privs_are() -- function
sequences_are() -- function
server_privs_are() -- function
set_eq() -- function
set_has() -- function
set_hasnt() -- function
set_ne() -- function
skip() -- function
table_owner_is() -- function
table_privs_are() -- function
tables_are() -- function
tablespace_owner_is() -- function
tablespace_privs_are() -- function
tablespaces_are() -- function
throws_ilike() -- function
throws_imatching() -- function
throws_like() -- function
throws_matching() -- function
throws_ok() -- function
todo() -- function
todo_end() -- function
todo_start() -- function
trigger_is() -- function
triggers_are() -- function
type_owner_is() -- function
types_are() -- function
unalike() -- function
unialike() -- function
users_are() -- function
view_owner_is() -- function
views_are() -- function
volatility_is() -- function
pg_all_foreign_keys -- view
tap_funky -- view
-- Extension: earthdistance
earth() -- function
earth_box() -- function
earth_distance() -- function
gc_to_sec() -- function
geo_distance() -- function
latitude() -- function
ll_to_earth() -- function
longitude() -- function
sec_to_gc() -- function
earth -- type
-- Extension: uuid-ossp
uuid_generate_v1() -- function
uuid_generate_v1mc() -- function
uuid_generate_v3() -- function
uuid_generate_v4() -- function
uuid_generate_v5() -- function
uuid_nil() -- function
uuid_ns_dns() -- function
uuid_ns_oid() -- function
uuid_ns_url() -- function
uuid_ns_x500() -- function
-- Extension: plperlu
plperlu_call_handler() -- function
plperlu_inline_handler() -- function
plperlu_validator() -- function
-- Extension: intarray
boolop() -- function
bqarr_in() -- function
bqarr_out() -- function
g_int_compress() -- function
g_int_consistent() -- function
g_int_decompress() -- function
g_int_penalty() -- function
g_int_picksplit() -- function
g_int_same() -- function
g_int_union() -- function
g_intbig_compress() -- function
g_intbig_consistent() -- function
g_intbig_decompress() -- function
g_intbig_penalty() -- function
g_intbig_picksplit() -- function
g_intbig_same() -- function
g_intbig_union() -- function
ginint4_consistent() -- function
ginint4_queryextract() -- function
icount() -- function
idx() -- function
intarray_del_elem() -- function
intarray_push_array() -- function
intarray_push_elem() -- function
intset() -- function
intset_subtract() -- function
intset_union_elem() -- function
querytree() -- function
rboolop() -- function
sort() -- function
sort_asc() -- function
sort_desc() -- function
subarray() -- function
uniq() -- function
intbig_gkey -- type
query_int -- type
-- Extension: pg_trgm
gin_extract_query_trgm() -- function
gin_extract_value_trgm() -- function
gin_trgm_consistent() -- function
gin_trgm_triconsistent() -- function
gtrgm_compress() -- function
gtrgm_consistent() -- function
gtrgm_decompress() -- function
gtrgm_distance() -- function
gtrgm_in() -- function
gtrgm_out() -- function
gtrgm_penalty() -- function
gtrgm_picksplit() -- function
gtrgm_same() -- function
gtrgm_union() -- function
set_limit() -- function
show_limit() -- function
show_trgm() -- function
similarity() -- function
similarity_dist() -- function
similarity_op() -- function
strict_word_similarity() -- function
strict_word_similarity_commutator_op() -- function
strict_word_similarity_dist_commutator_op() -- function
strict_word_similarity_dist_op() -- function
strict_word_similarity_op() -- function
word_similarity() -- function
word_similarity_commutator_op() -- function
word_similarity_dist_commutator_op() -- function
word_similarity_dist_op() -- function
word_similarity_op() -- function
gtrgm -- type
-- Extension: dict_int
dintdict_init() -- function
dintdict_lexize() -- function
-- Extension: amcheck
bt_index_check() -- function
bt_index_parent_check() -- function
-- Extension: btree_gist
cash_dist() -- function
date_dist() -- function
float4_dist() -- function
float8_dist() -- function
gbt_bit_compress() -- function
gbt_bit_consistent() -- function
gbt_bit_penalty() -- function
gbt_bit_picksplit() -- function
gbt_bit_same() -- function
gbt_bit_union() -- function
gbt_bpchar_compress() -- function
gbt_bpchar_consistent() -- function
gbt_bytea_compress() -- function
gbt_bytea_consistent() -- function
gbt_bytea_penalty() -- function
gbt_bytea_picksplit() -- function
gbt_bytea_same() -- function
gbt_bytea_union() -- function
gbt_cash_compress() -- function
gbt_cash_consistent() -- function
gbt_cash_distance() -- function
gbt_cash_fetch() -- function
gbt_cash_penalty() -- function
gbt_cash_picksplit() -- function
gbt_cash_same() -- function
gbt_cash_union() -- function
gbt_date_compress() -- function
gbt_date_consistent() -- function
gbt_date_distance() -- function
gbt_date_fetch() -- function
gbt_date_penalty() -- function
gbt_date_picksplit() -- function
gbt_date_same() -- function
gbt_date_union() -- function
gbt_decompress() -- function
gbt_enum_compress() -- function
gbt_enum_consistent() -- function
gbt_enum_fetch() -- function
gbt_enum_penalty() -- function
gbt_enum_picksplit() -- function
gbt_enum_same() -- function
gbt_enum_union() -- function
gbt_float4_compress() -- function
gbt_float4_consistent() -- function
gbt_float4_distance() -- function
gbt_float4_fetch() -- function
gbt_float4_penalty() -- function
gbt_float4_picksplit() -- function
gbt_float4_same() -- function
gbt_float4_union() -- function
gbt_float8_compress() -- function
gbt_float8_consistent() -- function
gbt_float8_distance() -- function
gbt_float8_fetch() -- function
gbt_float8_penalty() -- function
gbt_float8_picksplit() -- function
gbt_float8_same() -- function
gbt_float8_union() -- function
gbt_inet_compress() -- function
gbt_inet_consistent() -- function
gbt_inet_penalty() -- function
gbt_inet_picksplit() -- function
gbt_inet_same() -- function
gbt_inet_union() -- function
gbt_int2_compress() -- function
gbt_int2_consistent() -- function
gbt_int2_distance() -- function
gbt_int2_fetch() -- function
gbt_int2_penalty() -- function
gbt_int2_picksplit() -- function
gbt_int2_same() -- function
gbt_int2_union() -- function
gbt_int4_compress() -- function
gbt_int4_consistent() -- function
gbt_int4_distance() -- function
gbt_int4_fetch() -- function
gbt_int4_penalty() -- function
gbt_int4_picksplit() -- function
gbt_int4_same() -- function
gbt_int4_union() -- function
gbt_int8_compress() -- function
gbt_int8_consistent() -- function
gbt_int8_distance() -- function
gbt_int8_fetch() -- function
gbt_int8_penalty() -- function
gbt_int8_picksplit() -- function
gbt_int8_same() -- function
gbt_int8_union() -- function
gbt_intv_compress() -- function
gbt_intv_consistent() -- function
gbt_intv_decompress() -- function
gbt_intv_distance() -- function
gbt_intv_fetch() -- function
gbt_intv_penalty() -- function
gbt_intv_picksplit() -- function
gbt_intv_same() -- function
gbt_intv_union() -- function
gbt_macad8_compress() -- function
gbt_macad8_consistent() -- function
gbt_macad8_fetch() -- function
gbt_macad8_penalty() -- function
gbt_macad8_picksplit() -- function
gbt_macad8_same() -- function
gbt_macad8_union() -- function
gbt_macad_compress() -- function
gbt_macad_consistent() -- function
gbt_macad_fetch() -- function
gbt_macad_penalty() -- function
gbt_macad_picksplit() -- function
gbt_macad_same() -- function
gbt_macad_union() -- function
gbt_numeric_compress() -- function
gbt_numeric_consistent() -- function
gbt_numeric_penalty() -- function
gbt_numeric_picksplit() -- function
gbt_numeric_same() -- function
gbt_numeric_union() -- function
gbt_oid_compress() -- function
gbt_oid_consistent() -- function
gbt_oid_distance() -- function
gbt_oid_fetch() -- function
gbt_oid_penalty() -- function
gbt_oid_picksplit() -- function
gbt_oid_same() -- function
gbt_oid_union() -- function
gbt_text_compress() -- function
gbt_text_consistent() -- function
gbt_text_penalty() -- function
gbt_text_picksplit() -- function
gbt_text_same() -- function
gbt_text_union() -- function
gbt_time_compress() -- function
gbt_time_consistent() -- function
gbt_time_distance() -- function
gbt_time_fetch() -- function
gbt_time_penalty() -- function
gbt_time_picksplit() -- function
gbt_time_same() -- function
gbt_time_union() -- function
gbt_timetz_compress() -- function
gbt_timetz_consistent() -- function
gbt_ts_compress() -- function
gbt_ts_consistent() -- function
gbt_ts_distance() -- function
gbt_ts_fetch() -- function
gbt_ts_penalty() -- function
gbt_ts_picksplit() -- function
gbt_ts_same() -- function
gbt_ts_union() -- function
gbt_tstz_compress() -- function
gbt_tstz_consistent() -- function
gbt_tstz_distance() -- function
gbt_uuid_compress() -- function
gbt_uuid_consistent() -- function
gbt_uuid_fetch() -- function
gbt_uuid_penalty() -- function
gbt_uuid_picksplit() -- function
gbt_uuid_same() -- function
gbt_uuid_union() -- function
gbt_var_decompress() -- function
gbt_var_fetch() -- function
gbtreekey16_in() -- function
gbtreekey16_out() -- function
gbtreekey32_in() -- function
gbtreekey32_out() -- function
gbtreekey4_in() -- function
gbtreekey4_out() -- function
gbtreekey8_in() -- function
gbtreekey8_out() -- function
gbtreekey_var_in() -- function
gbtreekey_var_out() -- function
int2_dist() -- function
int4_dist() -- function
int8_dist() -- function
interval_dist() -- function
oid_dist() -- function
time_dist() -- function
ts_dist() -- function
tstz_dist() -- function
gbtreekey16 -- type
gbtreekey32 -- type
gbtreekey4 -- type
gbtreekey8 -- type
gbtreekey_var -- type
-- Extension: pageinspect
brin_metapage_info() -- function
brin_page_items() -- function
brin_page_type() -- function
brin_revmap_data() -- function
bt_metap() -- function
bt_page_items() -- function
bt_page_stats() -- function
fsm_page_contents() -- function
get_raw_page() -- function
gin_leafpage_items() -- function
gin_metapage_info() -- function
gin_page_opaque_info() -- function
hash_bitmap_info() -- function
hash_metapage_info() -- function
hash_page_items() -- function
hash_page_stats() -- function
hash_page_type() -- function
heap_page_item_attrs() -- function
heap_page_items() -- function
page_checksum() -- function
page_header() -- function
tuple_data_split() -- function
-- Extension: pltclu
pltclu_call_handler() -- function
-- Extension: hstore_plperl
hstore_to_plperl() -- function
plperl_to_hstore() -- function
-- Extension: moddatetime
moddatetime() -- function
-- Extension: fuzzystrmatch
difference() -- function
dmetaphone() -- function
dmetaphone_alt() -- function
levenshtein() -- function
levenshtein_less_equal() -- function
metaphone() -- function
soundex() -- function
text_soundex() -- function
-- Extension: ltree_plpython3u
ltree_to_plpython3() -- function
-- Extension: pgrouting
pgr_alphashape() -- function
pgr_analyzegraph() -- function
pgr_analyzeoneway() -- function
pgr_apspjohnson() -- function
pgr_apspwarshall() -- function
pgr_articulationpoints() -- function
pgr_astar() -- function
pgr_astarcost() -- function
pgr_astarcostmatrix() -- function
pgr_bdastar() -- function
pgr_bdastarcost() -- function
pgr_bdastarcostmatrix() -- function
pgr_bddijkstra() -- function
pgr_bddijkstracost() -- function
pgr_bddijkstracostmatrix() -- function
pgr_biconnectedcomponents() -- function
pgr_boykovkolmogorov() -- function
pgr_bridges() -- function
pgr_connectedcomponents() -- function
pgr_contractgraph() -- function
pgr_createtopology() -- function
pgr_createverticestable() -- function
pgr_dijkstra() -- function
pgr_dijkstracost() -- function
pgr_dijkstracostmatrix() -- function
pgr_dijkstravia() -- function
pgr_drivingdistance() -- function
pgr_edgedisjointpaths() -- function
pgr_edmondskarp() -- function
pgr_endpoint() -- function
pgr_euclediantsp() -- function
pgr_flipedges() -- function
pgr_floydwarshall() -- function
pgr_getcolumnname() -- function
pgr_gettablename() -- function
pgr_gsoc_vrppdtw() -- function
pgr_iscolumnindexed() -- function
pgr_iscolumnintable() -- function
pgr_johnson() -- function
pgr_kdijkstracost() -- function
pgr_kdijkstrapath() -- function
pgr_ksp() -- function
pgr_labelgraph() -- function
pgr_linegraph() -- function
pgr_linegraphfull() -- function
pgr_maxcardinalitymatch() -- function
pgr_maxflow() -- function
pgr_maxflowboykovkolmogorov() -- function
pgr_maxflowedmondskarp() -- function
pgr_maxflowpushrelabel() -- function
pgr_maximumcardinalitymatching() -- function
pgr_nodenetwork() -- function
pgr_pointsaspolygon() -- function
pgr_pointstodmatrix() -- function
pgr_pointstovids() -- function
pgr_pointtoedgenode() -- function
pgr_pushrelabel() -- function
pgr_quote_ident() -- function
pgr_startpoint() -- function
pgr_strongcomponents() -- function
pgr_texttopoints() -- function
pgr_trsp() -- function
pgr_trspviaedges() -- function
pgr_trspviavertices() -- function
pgr_tsp() -- function
pgr_version() -- function
pgr_versionless() -- function
pgr_vidstodmatrix() -- function
pgr_vrponedepot() -- function
pgr_withpoints() -- function
pgr_withpointscost() -- function
pgr_withpointscostmatrix() -- function
pgr_withpointsdd() -- function
pgr_withpointsksp() -- function
pgr_costresult -- type
pgr_costresult3 -- type
pgr_geomresult -- type
-- Extension: pgcrypto
armor() -- function
crypt() -- function
dearmor() -- function
decrypt() -- function
decrypt_iv() -- function
digest() -- function
encrypt() -- function
encrypt_iv() -- function
gen_random_bytes() -- function
gen_random_uuid() -- function
gen_salt() -- function
hmac() -- function
pgp_armor_headers() -- function
pgp_key_id() -- function
pgp_pub_decrypt() -- function
pgp_pub_decrypt_bytea() -- function
pgp_pub_encrypt() -- function
pgp_pub_encrypt_bytea() -- function
pgp_sym_decrypt() -- function
pgp_sym_decrypt_bytea() -- function
pgp_sym_encrypt() -- function
pgp_sym_encrypt_bytea() -- function
-- Extension: postgis_sfcgal
postgis_sfcgal_scripts_installed() -- function
postgis_sfcgal_version() -- function
st_3darea() -- function
st_3ddifference() -- function
st_3dintersection() -- function
st_3dunion() -- function
st_approximatemedialaxis() -- function
st_extrude() -- function
st_forcelhr() -- function
st_isplanar() -- function
st_issolid() -- function
st_makesolid() -- function
st_minkowskisum() -- function
st_orientation() -- function
st_straightskeleton() -- function
st_tesselate() -- function
st_volume() -- function
-- Extension: timetravel
get_timetravel() -- function
set_timetravel() -- function
timetravel() -- function
-- Extension: jsonb_plperlu
jsonb_to_plperlu() -- function
plperlu_to_jsonb() -- function
-- Extension: plperl
plperl_call_handler() -- function
plperl_inline_handler() -- function
plperl_validator() -- function
-- Extension: tablefunc
connectby() -- function
crosstab() -- function
crosstab2() -- function
crosstab3() -- function
crosstab4() -- function
normal_rand() -- function
tablefunc_crosstab_2 -- type
tablefunc_crosstab_3 -- type
tablefunc_crosstab_4 -- type
-- Extension: postgres_fdw
postgres_fdw_handler() -- function
postgres_fdw_validator() -- function
-- Extension: bloom
blhandler() -- function
-- Extension: pltcl
pltcl_call_handler() -- function
-- Extension: citext
citext() -- function
citext_cmp() -- function
citext_eq() -- function
citext_ge() -- function
citext_gt() -- function
citext_hash() -- function
citext_larger() -- function
citext_le() -- function
citext_lt() -- function
citext_ne() -- function
citext_pattern_cmp() -- function
citext_pattern_ge() -- function
citext_pattern_gt() -- function
citext_pattern_le() -- function
citext_pattern_lt() -- function
citext_smaller() -- function
citextin() -- function
citextout() -- function
citextrecv() -- function
citextsend() -- function
max() -- function
min() -- function
regexp_match() -- function
regexp_matches() -- function
regexp_replace() -- function
regexp_split_to_array() -- function
regexp_split_to_table() -- function
replace() -- function
split_part() -- function
strpos() -- function
texticlike() -- function
texticnlike() -- function
texticregexeq() -- function
texticregexne() -- function
translate() -- function
citext -- type
-- Extension: plpgsql
plpgsql_call_handler() -- function
plpgsql_inline_handler() -- function
plpgsql_validator() -- function
-- Extension: plpythonu
plpython_call_handler() -- function
plpython_inline_handler() -- function
plpython_validator() -- function
-- Extension: plpython2u
plpython2_call_handler() -- function
plpython2_inline_handler() -- function
plpython2_validator() -- function
-- Extension: hstore_plpythonu
hstore_to_plpython() -- function
plpython_to_hstore() -- function
-- Extension: hstore_plpython2u
hstore_to_plpython2() -- function
plpython2_to_hstore() -- function
-- Extension: ltree_plpythonu
ltree_to_plpython() -- function
-- Extension: ltree_plpython2u
ltree_to_plpython2() -- function
-- Extension: pldbgapi
pldbg_abort_target() -- function
pldbg_attach_to_port() -- function
pldbg_continue() -- function
pldbg_create_listener() -- function
pldbg_deposit_value() -- function
pldbg_drop_breakpoint() -- function
pldbg_get_breakpoints() -- function
pldbg_get_proxy_info() -- function
pldbg_get_source() -- function
pldbg_get_stack() -- function
pldbg_get_target_info() -- function
pldbg_get_variables() -- function
pldbg_oid_debug() -- function
pldbg_select_frame() -- function
pldbg_set_breakpoint() -- function
pldbg_set_global_breakpoint() -- function
pldbg_step_into pldbg_step_over() -- function
pldbg_wait_for_breakpoint() -- function
pldbg_wait_for_target() -- function
plpgsql_oid_debug() -- function
breakpoint -- type
frame -- type
proxyinfo -- type
targetinfo -- type
var -- type
-- Extension: chkpass
chkpass_in() -- function
chkpass_out() -- function
eq() -- function
ne() -- function
raw() -- function
-- Extensions names
address_standardizer
address_standardizer_data_us
adminpack
amcheck
autoinc
bloom
btree_gin
btree_gist
citext
cube
dblink
dict_int
dict_xsyn
earthdistance
file_fdw
fuzzystrmatch
hstore
hstore_plperl
hstore_plperlu
hstore_plpython3u
insert_username
intagg
intarray
isn
jsonb_plperl
jsonb_plperlu
jsonb_plpython3u
lo
ltree
ltree_plpython3u
moddatetime
pageinspect
pg_buffercache
pg_freespacemap
pg_prewarm
pg_stat_statements
pg_trgm
pg_visibility
pgcrypto
pgrouting
pgrowlocks
pgstattuple
pgtap
plperl
plperlu
plpgsql
plpython3u
pltcl
pltclu
postgis
postgis_sfcgal
postgis_tiger_geocoder
postgis_topology
postgres_fdw
refint
seg
sslinfo
tablefunc
tcn
temporal_tables
timetravel
tsm_system_rows
tsm_system_time
unaccent
xml2
-- Lgeacy extensions names
chkpass
hstore_plpython2u
hstore_plpythonu
ltree_plpython2u
ltree_plpythonu
pldbgapi
plpython2u
plpythonu
-- Catalog tables
administrable_role_authorizations
applicable_roles
attributes
character_sets
check_constraint_routine_usage
check_constraints
collation_character_set_applicability
collations
column_domain_usage
column_options
column_privileges
column_udt_usage
columns
constraint_column_usage
constraint_table_usage
data_type_privileges
domain_constraints
domain_udt_usage
domains
element_types
enabled_roles
foreign_data_wrapper_options
foreign_data_wrappers
foreign_server_options
foreign_servers
foreign_table_options
foreign_tables
information_schema_catalog_name
key_column_usage
parameters
pg_aggregate
pg_am
pg_amop
pg_amproc
pg_attrdef
pg_attribute
pg_auth_members
pg_authid
pg_available_extension_versions
pg_available_extensions
pg_cast
pg_class
pg_collation
pg_config
pg_constraint
pg_conversion
pg_cursors
pg_database
pg_db_role_setting
pg_default_acl
pg_depend
pg_description
pg_enum
pg_event_trigger
pg_extension
pg_file_settings
pg_foreign_data_wrapper
pg_foreign_server
pg_foreign_table
pg_group
pg_hba_file_rules
pg_index
pg_indexes
pg_inherits
pg_init_privs
pg_language
pg_largeobject
pg_largeobject_metadata
pg_locks
pg_matviews
pg_namespace
pg_opclass
pg_operator
pg_opfamily
pg_partitioned_table
pg_pltemplate
pg_policies
pg_policy
pg_prepared_statements
pg_prepared_xacts
pg_proc
pg_publication
pg_publication_rel
pg_publication_tables
pg_range
pg_replication_origin
pg_replication_origin_status
pg_replication_slots
pg_rewrite
pg_roles
pg_rules
pg_seclabel
pg_seclabels
pg_sequence
pg_sequences
pg_settings
pg_shadow
pg_shdepend
pg_shdescription
pg_shseclabel
pg_stat_activity
pg_stat_all_indexes
pg_stat_all_tables
pg_stat_archiver
pg_stat_bgwriter
pg_stat_database
pg_stat_database_conflicts
pg_stat_progress_vacuum
pg_stat_replication
pg_stat_ssl
pg_stat_subscription
pg_stat_sys_indexes
pg_stat_sys_tables
pg_stat_user_functions
pg_stat_user_indexes
pg_stat_user_tables
pg_stat_wal_receiver
pg_stat_xact_all_tables
pg_stat_xact_sys_tables
pg_stat_xact_user_functions
pg_stat_xact_user_tables
pg_statio_all_indexes
pg_statio_all_sequences
pg_statio_all_tables
pg_statio_sys_indexes
pg_statio_sys_sequences
pg_statio_sys_tables
pg_statio_user_indexes
pg_statio_user_sequences
pg_statio_user_tables
pg_statistic
pg_statistic_ext
pg_stats
pg_subscription
pg_subscription_rel
pg_tables
pg_tablespace
pg_timezone_abbrevs
pg_timezone_names
pg_transform
pg_trigger
pg_ts_config
pg_ts_config_map
pg_ts_dict
pg_ts_parser
pg_ts_template
pg_type
pg_user
pg_user_mapping
pg_user_mappings
pg_views
referential_constraints
role_column_grants
role_routine_grants
role_table_grants
role_udt_grants
role_usage_grants
routine_privileges
routines
schemata
sequences
sql_features
sql_implementation_info
sql_languages
sql_packages
sql_parts
sql_sizing
sql_sizing_profiles
table_constraints
table_privileges
tables
transforms
triggered_update_columns
triggers
udt_privileges
usage_privileges
user_defined_types
user_mapping_options
user_mappings
view_column_usage
view_routine_usage
view_table_usage
views
-- Built-in keywords
absolute
access
action
add
admin
after
aggregate
all
also
always
analyse
and
any
as
asc
assertion
assignment
asymmetric
at
attach
attribute
authorization
backward
before
between
binary
both
by
cache
call
called
cascade
cascaded
case
cast
catalog
chain
characteristics
check
class
coalesce
collate
collation
column
columns
comments
committed
concurrently
configuration
conflict
connection
constraint
content
continue
conversion
cost
cross
csv
current
current_catalog
current_date
current_role
current_schema
current_time
current_timestamp
current_user
cursor
cycle
data
database
day
dec
default
defaults
deferrable
deferred
definer
delimiter
delimiters
depends
desc
detach
dictionary
disable
distinct
document
domain
each
else
enable
encoding
encrypted
enum
escape
event
except
exclude
excluding
exclusive
exists
extension
external
extract
false
family
filter
first
float
following
for
force
foreign
forward
freeze
from
full
function
functions
generated
global
granted
greatest
group
grouping
groups
handler
having
header
hold
hour
identity
if
ilike
immediate
immutable
implicit
in
include
including
increment
index
indexes
inherit
inherits
initially
inline
inner
inout
input
insensitive
instead
intersect
into
invoker
is
isnull
isolation
join
key
language
large
last
lateral
leading
leakproof
least
left
level
like
limit
local
localtime
localtimestamp
location
locked
logged
mapping
match
materialized
maxvalue
method
minute
minvalue
mode
month
name
names
national
natural
nchar
new
next
no
none
not
nothing
notnull
nowait
null
nullif
nulls
object
of
off
offset
oids
old
on
only
operator
option
options
or
order
ordinality
others
out
outer
over
overlaps
overlay
overriding
owned
owner
parallel
parser
partial
partition
passing
password
placing
plans
policy
position
preceding
preserve
primary
prior
privileges
procedural
procedure
procedures
program
publication
quote
range
read
recheck
recursive
ref
references
referencing
relative
rename
repeatable
replace
replica
restart
restrict
returning
returns
right
role
rollup
routine
routines
row
rows
rule
schema
schemas
scroll
search
second
sequence
sequences
serializable
server
session
session_user
setof
sets
share
similar
simple
skip
snapshot
some
sql
stable
standalone
statement
statistics
stdin
stdout
storage
strict
strip
subscription
substring
symmetric
sysid
system
table
tables
tablesample
tablespace
temp
template
temporary
then
ties
to
trailing
transform
treat
trigger
trim
true
trusted
type
types
unbounded
uncommitted
unencrypted
union
unique
unknown
unlogged
until
user
using
valid
validate
validator
value
variadic
verbose
version
view
views
volatile
when
where
whitespace
window
with
within
without
wrapper
write
xmlattributes
xmlconcat
xmlelement
xmlexists
xmlforest
xmlnamespaces
xmlparse
xmlpi
xmlroot
xmlserialize
xmltable
year
yes
-- Additional constants
information_schema
pg_catalog
-- Operators
!
!!
!~
!~*
!~~
!~~*
#
##
#-
#<
#<#
#<=
#<=#
#<>
#=
#>
#>#
#>=
#>=#
#>>
%
%#
%%
%>
%>>
&
&&
&&&
&/&
&<
&<|
&>
*
*<
*<=
*<>
*=
*>
*>=
+
-
->
->>
-|-
/
<
<#>
<%
<->
<->>
<->>>
<<
<<%
<<->
<<->>
<<<->
<<=
<<@
<<|
<=
<=>
<>
<?>
<@
<@>
<^
=
>
>=
>>
>>=
>^
?
?#
?&
?-
?-|
?<@
?@
?@>
?|
?||
?~
@
@-@
@>
@>>
@@
@@@
^
^<@
^?
^@
^@>
^~
|
|&>
|/
|=|
|>>
||
||/
~
~*
~<=~
~<~
~=
~==
~>
~>=~
~>~
~~
~~*
-- Error codes
active_sql_transaction
admin_shutdown
ambiguous_alias
ambiguous_column
ambiguous_function
ambiguous_parameter
array_subscript_error
assert_failure
bad_copy_file_format
branch_transaction_already_active
cannot_coerce
cannot_connect_now
cant_change_runtime_param
cardinality_violation
case_not_found
character_not_in_repertoire
check_violation
collation_mismatch
config_file_error
configuration_limit_exceeded
connection_does_not_exist
connection_exception
connection_failure
containing_sql_not_permitted
crash_shutdown
data_corrupted
data_exception
database_dropped
datatype_mismatch
datetime_field_overflow
deadlock_detected
dependent_objects_still_exist
dependent_privilege_descriptors_still_exist
deprecated_feature
diagnostics_exception
disk_full
division_by_zero
duplicate_alias
duplicate_column
duplicate_cursor
duplicate_database
duplicate_file
duplicate_function
duplicate_object
duplicate_prepared_statement
duplicate_schema
duplicate_table
dynamic_result_sets_returned
error_in_assignment
escape_character_conflict
event_trigger_protocol_violated
exclusion_violation
external_routine_exception
external_routine_invocation_exception
fdw_column_name_not_found
fdw_dynamic_parameter_value_needed
fdw_error
fdw_function_sequence_error
fdw_inconsistent_descriptor_information
fdw_invalid_attribute_value
fdw_invalid_column_name
fdw_invalid_column_number
fdw_invalid_data_type
fdw_invalid_data_type_descriptors
fdw_invalid_descriptor_field_identifier
fdw_invalid_handle
fdw_invalid_option_index
fdw_invalid_option_name
fdw_invalid_string_format
fdw_invalid_string_length_or_buffer_length
fdw_invalid_use_of_null_pointer
fdw_no_schemas
fdw_option_name_not_found
fdw_out_of_memory
fdw_reply_handle
fdw_schema_not_found
fdw_table_not_found
fdw_too_many_handles
fdw_unable_to_create_execution
fdw_unable_to_create_reply
fdw_unable_to_establish_connection
feature_not_supported
floating_point_exception
foreign_key_violation
function_executed_no_return_statement
generated_always
grouping_error
held_cursor_requires_same_isolation_level
idle_in_transaction_session_timeout
implicit_zero_bit_padding
in_failed_sql_transaction
inappropriate_access_mode_for_branch_transaction
inappropriate_isolation_level_for_branch_transaction
indeterminate_collation
indeterminate_datatype
index_corrupted
indicator_overflow
insufficient_privilege
insufficient_resources
integrity_constraint_violation
internal_error
interval_field_overflow
invalid_argument_for_logarithm
invalid_argument_for_nth_value_function
invalid_argument_for_ntile_function
invalid_argument_for_power_function
invalid_argument_for_width_bucket_function
invalid_authorization_specification
invalid_binary_representation
invalid_catalog_name
invalid_character_value_for_cast
invalid_column_definition
invalid_column_reference
invalid_cursor_definition
invalid_cursor_name
invalid_cursor_state
invalid_database_definition
invalid_datetime_format
invalid_escape_character
invalid_escape_octet
invalid_escape_sequence
invalid_foreign_key
invalid_function_definition
invalid_grant_operation
invalid_grantor
invalid_indicator_parameter_value
invalid_locator_specification
invalid_name
invalid_object_definition
invalid_parameter_value
invalid_password
invalid_preceding_or_following_size
invalid_prepared_statement_definition
invalid_recursion
invalid_regular_expression
invalid_role_specification
invalid_row_count_in_limit_clause
invalid_row_count_in_result_offset_clause
invalid_savepoint_specification
invalid_schema_definition
invalid_schema_name
invalid_sql_statement_name
invalid_sqlstate_returned
invalid_table_definition
invalid_tablesample_argument
invalid_tablesample_repeat
invalid_text_representation
invalid_time_zone_displacement_value
invalid_transaction_initiation
invalid_transaction_state
invalid_transaction_termination
invalid_use_of_escape_character
invalid_xml_comment
invalid_xml_content
invalid_xml_document
invalid_xml_processing_instruction
io_error
locator_exception
lock_file_exists
lock_not_available
modifying_sql_data_not_permitted
most_specific_type_mismatch
name_too_long
no_active_sql_transaction
no_active_sql_transaction_for_branch_transaction
no_additional_dynamic_result_sets_returned
no_data
no_data_found
nonstandard_use_of_escape_character
not_an_xml_document
not_null_violation
null_value_eliminated_in_set_function
null_value_no_indicator_parameter
null_value_not_allowed
numeric_value_out_of_range
object_in_use
object_not_in_prerequisite_state
operator_intervention
out_of_memory
plpgsql_error
privilege_not_granted
privilege_not_revoked
program_limit_exceeded
prohibited_sql_statement_attempted
protocol_violation
query_canceled
raise_exception
read_only_sql_transaction
reading_sql_data_not_permitted
reserved_name
restrict_violation
savepoint_exception
schema_and_data_statement_mixing_not_supported
sequence_generator_limit_exceeded
serialization_failure
snapshot_too_old
sql_routine_exception
sql_statement_not_yet_complete
sqlclient_unable_to_establish_sqlconnection
sqlserver_rejected_establishment_of_sqlconnection
srf_protocol_violated
stacked_diagnostics_accessed_without_active_handler
statement_completion_unknown
statement_too_complex
string_data_length_mismatch
string_data_right_truncation
substring_error
successful_completion
syntax_error
syntax_error_or_access_rule_violation
system_error
too_many_arguments
too_many_columns
too_many_connections
too_many_rows
transaction_integrity_constraint_violation
transaction_resolution_unknown
transaction_rollback
trigger_protocol_violated
triggered_action_exception
triggered_data_change_violation
trim_error
undefined_column
undefined_file
undefined_function
undefined_object
undefined_parameter
undefined_table
unique_violation
unsafe_new_enum_value_usage
unterminated_c_string
untranslatable_character
warning
windowing_error
with_check_option_violation
wrong_object_type
zero_length_character_string

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

