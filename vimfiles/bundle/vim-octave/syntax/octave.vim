" Vim syntax file
" Language:             Octave
" Maintainer:           Rik <rik@octave.org>
" Original Maintainers: Jaroslav Hajek <highegg@gmail.com>
"                       Francisco Castro <fcr@adinet.com.uy>
"                       Preben 'Peppe' Guldberg <peppe-vim@wielders.org>
" Original Author: Mario Eusebio
" Last Change: 10 Jan 2014
" Syntax matched to Octave Release: 3.8.0
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Use case sensitive matching of keywords
syn case match

" Stop keywords embedded in structures from lighting up
" For example, mystruct.length = 1 should not highlight length.
" WARNING: beginning of word pattern \< will no longer match '.'
setlocal iskeyword +=.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntax group definitions for Octave
syn keyword octaveBeginKeyword  for parfor function if switch
syn keyword octaveBeginKeyword  try unwind_protect while do
syn keyword octaveEndKeyword    end endfor endparfor endfunction endif endswitch
syn keyword octaveEndKeyword    end_try_catch end_unwind_protect endwhile until
syn keyword octaveElseKeyword   case catch else elseif otherwise
syn keyword octaveElseKeyword   unwind_protect_cleanup

syn keyword octaveStatement  break continue global persistent return

syn keyword octaveVarKeyword varargin varargout

syn keyword octaveReserved  __FILE__ __LINE__ static
syn keyword octaveReserved  classdef enumeration events methods properties
syn keyword octaveReserved  endclassdef endenumeration endevents endmethods
syn keyword octaveReserved  endproperties

" List of commands (these don't require a parenthesis to invoke)
syn keyword octaveCommand contained  cd chdir clear close dbcont dbquit dbstep
syn keyword octaveCommand contained  demo diary doc echo edit edit_history
syn keyword octaveCommand contained  example format help history hold ishold
syn keyword octaveCommand contained  load lookfor ls mkoctfile more pkg run
syn keyword octaveCommand contained  run_history save shg test type what which
syn keyword octaveCommand contained  who whos

" List of functions which set internal variables
syn keyword octaveSetVarFun contained  EDITOR EXEC_PATH F_SETFD F_SETFL I
syn keyword octaveSetVarFun contained  IMAGE_PATH Inf J NA NaN O_APPEND O_ASYNC 
syn keyword octaveSetVarFun contained  PAGER PAGER_FLAGS PS1 PS2 PS4
syn keyword octaveSetVarFun contained  __error_text__
syn keyword octaveSetVarFun contained  allow_noninteger_range_as_index ans argv
syn keyword octaveSetVarFun contained  beep_on_error built_in_docstrings_file
syn keyword octaveSetVarFun contained  completion_append_char
syn keyword octaveSetVarFun contained  confirm_recursive_rmdir
syn keyword octaveSetVarFun contained  crash_dumps_octave_core debug_java
syn keyword octaveSetVarFun contained  debug_jit debug_on_error
syn keyword octaveSetVarFun contained  debug_on_interrupt debug_on_warning
syn keyword octaveSetVarFun contained  default_save_options
syn keyword octaveSetVarFun contained  do_braindead_shortcircuit_evaluation
syn keyword octaveSetVarFun contained  doc_cache_file e echo_executing_commands
syn keyword octaveSetVarFun contained  eps error_text false filemarker
syn keyword octaveSetVarFun contained  fixed_point_format gnuplot_binary
syn keyword octaveSetVarFun contained  graphics_toolkit
syn keyword octaveSetVarFun contained  gui_mode history history_control
syn keyword octaveSetVarFun contained  history_file history_save history_size
syn keyword octaveSetVarFun contained  history_timestamp_format_string i
syn keyword octaveSetVarFun contained  ignore_function_time_stamp inf info_file
syn keyword octaveSetVarFun contained  info_program j java_convert_matrix
syn keyword octaveSetVarFun contained  java_debug java_matrix_autoconversion
syn keyword octaveSetVarFun contained  java_unsigned_autoconversion
syn keyword octaveSetVarFun contained  java_unsigned_conversion jit_enable
syn keyword octaveSetVarFun contained  jit_startcnt ls_command makeinfo_program
syn keyword octaveSetVarFun contained  max_recursion_depth
syn keyword octaveSetVarFun contained  missing_component_hook
syn keyword octaveSetVarFun contained  missing_function_hook mouse_wheel_zoom
syn keyword octaveSetVarFun contained  nan nargin nargout
syn keyword octaveSetVarFun contained  octave_core_file_limit
syn keyword octaveSetVarFun contained  octave_core_file_name
syn keyword octaveSetVarFun contained  octave_core_file_options
syn keyword octaveSetVarFun contained  optimize_subsasgn_calls
syn keyword octaveSetVarFun contained  output_max_field_width output_precision
syn keyword octaveSetVarFun contained  page_output_immediately
syn keyword octaveSetVarFun contained  page_screen_output path pathsep pi
syn keyword octaveSetVarFun contained  prefdir print_empty_dimensions
syn keyword octaveSetVarFun contained  print_struct_array_contents
syn keyword octaveSetVarFun contained  program_invocation_name program_name
syn keyword octaveSetVarFun contained  realmax realmin save_default_options
syn keyword octaveSetVarFun contained  save_header_format_string save_precision
syn keyword octaveSetVarFun contained  saving_history sighup_dumps_octave_core
syn keyword octaveSetVarFun contained  sigterm_dumps_octave_core
syn keyword octaveSetVarFun contained  silent_functions sparse_auto_mutate
syn keyword octaveSetVarFun contained  split_long_rows string_fill_char
syn keyword octaveSetVarFun contained  struct_levels_to_print
syn keyword octaveSetVarFun contained  suppress_verbose_help_message svd_driver
syn keyword octaveSetVarFun contained  texi_macros_file true whos_line_format

" List of functions which query internal variables
" Excluded i,j from list above because they are often used as loop variables
" They will be highlighted appropriately by the rule which matches numbers
syn keyword octaveVariable contained  EDITOR EXEC_PATH F_SETFD F_SETFL I
syn keyword octaveVariable contained  EDITOR IMAGE_PATH Inf J NA NaN O_APPEND 
syn keyword octaveVariable contained  O_ASYNC PAGER PAGER_FLAGS PS1 PS2 PS4
syn keyword octaveVariable contained  __error_text__
syn keyword octaveVariable contained  allow_noninteger_range_as_index ans argv
syn keyword octaveVariable contained  beep_on_error built_in_docstrings_file
syn keyword octaveVariable contained  completion_append_char
syn keyword octaveVariable contained  confirm_recursive_rmdir
syn keyword octaveVariable contained  crash_dumps_octave_core debug_java
syn keyword octaveVariable contained  debug_jit debug_on_error
syn keyword octaveVariable contained  debug_on_interrupt debug_on_warning
syn keyword octaveVariable contained  default_save_options
syn keyword octaveVariable contained  do_braindead_shortcircuit_evaluation
syn keyword octaveVariable contained  doc_cache_file e echo_executing_commands
syn keyword octaveVariable contained  eps error_text false filemarker
syn keyword octaveVariable contained  fixed_point_format gnuplot_binary
syn keyword octaveVariable contained  graphics_toolkit
syn keyword octaveVariable contained  gui_mode history history_control
syn keyword octaveVariable contained  history_file history_save history_size
syn keyword octaveVariable contained  history_timestamp_format_string i
syn keyword octaveVariable contained  ignore_function_time_stamp inf info_file
syn keyword octaveVariable contained  info_program j java_convert_matrix
syn keyword octaveVariable contained  java_debug java_matrix_autoconversion
syn keyword octaveVariable contained  java_unsigned_autoconversion
syn keyword octaveVariable contained  java_unsigned_conversion jit_enable
syn keyword octaveVariable contained  jit_startcnt ls_command makeinfo_program
syn keyword octaveVariable contained  max_recursion_depth
syn keyword octaveVariable contained  missing_component_hook
syn keyword octaveVariable contained  missing_function_hook mouse_wheel_zoom
syn keyword octaveVariable contained  nan nargin nargout
syn keyword octaveVariable contained  octave_core_file_limit
syn keyword octaveVariable contained  octave_core_file_name
syn keyword octaveVariable contained  octave_core_file_options
syn keyword octaveVariable contained  optimize_subsasgn_calls
syn keyword octaveVariable contained  output_max_field_width output_precision
syn keyword octaveVariable contained  page_output_immediately
syn keyword octaveVariable contained  page_screen_output path pathsep pi
syn keyword octaveVariable contained  prefdir print_empty_dimensions
syn keyword octaveVariable contained  print_struct_array_contents
syn keyword octaveVariable contained  program_invocation_name program_name
syn keyword octaveVariable contained  realmax realmin save_default_options
syn keyword octaveVariable contained  save_header_format_string save_precision
syn keyword octaveVariable contained  saving_history sighup_dumps_octave_core
syn keyword octaveVariable contained  sigterm_dumps_octave_core
syn keyword octaveVariable contained  silent_functions sparse_auto_mutate
syn keyword octaveVariable contained  split_long_rows string_fill_char
syn keyword octaveVariable contained  struct_levels_to_print
syn keyword octaveVariable contained  suppress_verbose_help_message svd_driver
syn keyword octaveVariable contained  texi_macros_file true whos_line_format

" Read-only variables
syn keyword octaveVariable contained  F_DUPFD F_GETFD F_GETFL OCTAVE_HOME
syn keyword octaveVariable contained  OCTAVE_VERSION O_CREAT O_EXCL O_NONBLOCK
syn keyword octaveVariable contained  O_RDONLY O_RDWR O_SYNC O_TRUNC O_WRONLY
syn keyword octaveVariable contained  P_tmpdir SEEK_CUR SEEK_END SEEK_SET SIG
syn keyword octaveVariable contained  WCONTINUE WCOREDUMP WEXITSTATUS
syn keyword octaveVariable contained  WIFCONTINUED WIFEXITED WIFSIGNALED
syn keyword octaveVariable contained  WIFSTOPPED WNOHANG WSTOPSIG WTERMSIG
syn keyword octaveVariable contained  WUNTRACED available_graphics_toolkits
syn keyword octaveVariable contained  command_line_path have_window_system
syn keyword octaveVariable contained  loaded_graphics_toolkits matlabroot
syn keyword octaveVariable contained  namelengthmax native_float_format pwd
syn keyword octaveVariable contained  stderr stdin stdout

" List of functions
syn keyword octaveFunction contained  S_ISBLK S_ISCHR S_ISDIR S_ISFIFO S_ISLNK
syn keyword octaveFunction contained  S_ISREG S_ISSOCK __accumarray_max__
syn keyword octaveFunction contained  __accumarray_min__ __accumarray_sum__
syn keyword octaveFunction contained  __accumdim_sum__
syn keyword octaveFunction contained  __actual_axis_position__ __all_opts__
syn keyword octaveFunction contained  __builtins__ __calc_dimensions__
syn keyword octaveFunction contained  __clabel__ __contourc__
syn keyword octaveFunction contained  __current_scope__
syn keyword octaveFunction contained  __default_plot_options__ __delaunayn__
syn keyword octaveFunction contained  __dispatch__ __display_tokens__
syn keyword octaveFunction contained  __dsearchn__ __dump_symtab_info__
syn keyword octaveFunction contained  __eigs__ __fieldnames__ __finish__
syn keyword octaveFunction contained  __fltk_maxtime__ __fltk_redraw__
syn keyword octaveFunction contained  __fltk_uigetfile__ __ftp__ __ftp_ascii__
syn keyword octaveFunction contained  __ftp_binary__ __ftp_close__ __ftp_cwd__
syn keyword octaveFunction contained  __ftp_delete__ __ftp_dir__ __ftp_mget__
syn keyword octaveFunction contained  __ftp_mkdir__ __ftp_mode__ __ftp_mput__
syn keyword octaveFunction contained  __ftp_pwd__ __ftp_rename__ __ftp_rmdir__
syn keyword octaveFunction contained  __get__ __getlegenddata__ __glpk__
syn keyword octaveFunction contained  __gnuplot_drawnow__ __go_axes__
syn keyword octaveFunction contained  __go_axes_init__ __go_delete__
syn keyword octaveFunction contained  __go_execute_callback__ __go_figure__
syn keyword octaveFunction contained  __go_figure_handles__ __go_handles__
syn keyword octaveFunction contained  __go_hggroup__ __go_image__ __go_line__
syn keyword octaveFunction contained  __go_patch__ __go_surface__ __go_text__
syn keyword octaveFunction contained  __go_uicontextmenu__ __go_uicontrol__
syn keyword octaveFunction contained  __go_uimenu__ __go_uipanel__
syn keyword octaveFunction contained  __go_uipushtool__ __go_uitoggletool__
syn keyword octaveFunction contained  __go_uitoolbar__
syn keyword octaveFunction contained  __gripe_missing_component__ __gud_mode__
syn keyword octaveFunction contained  __have_feature__ __have_fltk__
syn keyword octaveFunction contained  __image_pixel_size__ __init_fltk__
syn keyword octaveFunction contained  __init_gnuplot__ __is_handle_visible__
syn keyword octaveFunction contained  __isa_parent__ __java_exit__
syn keyword octaveFunction contained  __java_get__ __java_init__ __java_set__
syn keyword octaveFunction contained  __keywords__ __lexer_debug_flag__
syn keyword octaveFunction contained  __lin_interpn__ __list_functions__
syn keyword octaveFunction contained  __magick_finfo__ __magick_formats__
syn keyword octaveFunction contained  __magick_ping__ __magick_read__
syn keyword octaveFunction contained  __magick_write__ __makeinfo__
syn keyword octaveFunction contained  __methods__ __next_line_color__
syn keyword octaveFunction contained  __next_line_style__
syn keyword octaveFunction contained  __octave_link_edit_file__
syn keyword octaveFunction contained  __octave_link_enabled__
syn keyword octaveFunction contained  __octave_link_file_dialog__
syn keyword octaveFunction contained  __octave_link_input_dialog__
syn keyword octaveFunction contained  __octave_link_list_dialog__
syn keyword octaveFunction contained  __octave_link_message_dialog__
syn keyword octaveFunction contained  __octave_link_question_dialog__
syn keyword octaveFunction contained  __octave_link_show_doc__
syn keyword octaveFunction contained  __octave_link_show_preferences__
syn keyword octaveFunction contained  __operators__ __parent_classes__
syn keyword octaveFunction contained  __parse_file__ __parser_debug_flag__
syn keyword octaveFunction contained  __pathorig__ __pchip_deriv__
syn keyword octaveFunction contained  __plt_get_axis_arg__ __pltopt__
syn keyword octaveFunction contained  __printf_assert__ __profiler_data__
syn keyword octaveFunction contained  __profiler_enable__ __profiler_reset__
syn keyword octaveFunction contained  __prog_output_assert__ __qp__
syn keyword octaveFunction contained  __request_drawnow__ __run_test_suite__
syn keyword octaveFunction contained  __sort_rows_idx__ __token_count__
syn keyword octaveFunction contained  __unimplemented__ __varval__
syn keyword octaveFunction contained  __version_info__ __voronoi__ __which__
syn keyword octaveFunction contained  abs accumarray accumdim acos acosd acosh
syn keyword octaveFunction contained  acot acotd acoth acsc acscd acsch
syn keyword octaveFunction contained  add_input_event_hook addlistener addpath
syn keyword octaveFunction contained  addpref addproperty addtodate airy all
syn keyword octaveFunction contained  allchild amd ancestor and angle anova
syn keyword octaveFunction contained  any arch_fit arch_rnd arch_test area arg
syn keyword octaveFunction contained  argnames arma_rnd arrayfun asctime asec
syn keyword octaveFunction contained  asecd asech asin asind asinh assert
syn keyword octaveFunction contained  assignin atan atan2 atan2d atand atanh
syn keyword octaveFunction contained  atexit autoload autoreg_matrix autumn
syn keyword octaveFunction contained  axes axis balance bar barh bartlett
syn keyword octaveFunction contained  bartlett_test base2dec base64_decode
syn keyword octaveFunction contained  base64_encode beep bessel besselh
syn keyword octaveFunction contained  besseli besselj besselk bessely beta
syn keyword octaveFunction contained  betacdf betainc betaincinv betainv
syn keyword octaveFunction contained  betaln betapdf betarnd bicg bicgstab
syn keyword octaveFunction contained  bicubic bin2dec bincoeff binocdf binoinv
syn keyword octaveFunction contained  binopdf binornd bitand bitcmp bitget
syn keyword octaveFunction contained  bitmax bitor bitpack bitset bitshift
syn keyword octaveFunction contained  bitunpack bitxor blackman blanks blkdiag
syn keyword octaveFunction contained  blkmm bone box brighten bsxfun
syn keyword octaveFunction contained  bug_report builtin bunzip2 bzip2
syn keyword octaveFunction contained  calendar canonicalize_file_name cart2pol
syn keyword octaveFunction contained  cart2sph cast cat cauchy_cdf cauchy_inv
syn keyword octaveFunction contained  cauchy_pdf cauchy_rnd caxis cbrt ccolamd
syn keyword octaveFunction contained  ceil cell cell2mat cell2struct celldisp
syn keyword octaveFunction contained  cellfun cellindexmat cellslices cellstr
syn keyword octaveFunction contained  center cgs char chi2cdf chi2inv chi2pdf
syn keyword octaveFunction contained  chi2rnd chisquare_test_homogeneity
syn keyword octaveFunction contained  chisquare_test_independence chol
syn keyword octaveFunction contained  chol2inv choldelete cholinsert cholinv
syn keyword octaveFunction contained  cholshift cholupdate chop circshift
syn keyword octaveFunction contained  citation cla clabel class clc clf clock
syn keyword octaveFunction contained  cloglog closereq cmpermute cmunique
syn keyword octaveFunction contained  colamd colloc colon colorbar colorcube
syn keyword octaveFunction contained  colormap colperm colstyle columns comet
syn keyword octaveFunction contained  comet3 comma common_size
syn keyword octaveFunction contained  commutation_matrix compan
syn keyword octaveFunction contained  compare_versions compass
syn keyword octaveFunction contained  completion_matches complex computer cond
syn keyword octaveFunction contained  condest conj contour contour3 contourc
syn keyword octaveFunction contained  contourf contrast conv conv2 convhull
syn keyword octaveFunction contained  convhulln convn cool copper copyfile
syn keyword octaveFunction contained  copyobj cor cor_test corr corrcoef cos
syn keyword octaveFunction contained  cosd cosh cot cotd coth cov cplxpair
syn keyword octaveFunction contained  cputime cross csc cscd csch cstrcat
syn keyword octaveFunction contained  csvread csvwrite csymamd ctime
syn keyword octaveFunction contained  ctranspose cummax cummin cumprod cumsum
syn keyword octaveFunction contained  cumtrapz curl cut cylinder daspect daspk
syn keyword octaveFunction contained  daspk_options dasrt dasrt_options dassl
syn keyword octaveFunction contained  dassl_options date datenum datestr
syn keyword octaveFunction contained  datetick datevec dawson dbclear dbdown
syn keyword octaveFunction contained  dblist dblquad dbnext dbstack dbstatus
syn keyword octaveFunction contained  dbstop dbtype dbup dbwhere deal deblank
syn keyword octaveFunction contained  debug dec2base dec2bin dec2hex deconv
syn keyword octaveFunction contained  del2 delaunay delaunay3 delaunayn delete
syn keyword octaveFunction contained  dellistener desktop det detrend diag
syn keyword octaveFunction contained  diff diffpara diffuse dir discrete_cdf
syn keyword octaveFunction contained  discrete_inv discrete_pdf discrete_rnd
syn keyword octaveFunction contained  disp display divergence dlmread dlmwrite
syn keyword octaveFunction contained  dmperm do_string_escapes
syn keyword octaveFunction contained  doc_cache_create dos dot double drawnow
syn keyword octaveFunction contained  dsearch dsearchn dump_prefs dup2
syn keyword octaveFunction contained  duplication_matrix durbinlevinson eig
syn keyword octaveFunction contained  eigs ellipj ellipke ellipsoid
syn keyword octaveFunction contained  empirical_cdf empirical_inv
syn keyword octaveFunction contained  empirical_pdf empirical_rnd endgrent
syn keyword octaveFunction contained  endpwent eomday eq erf erfc erfcinv
syn keyword octaveFunction contained  erfcx erfi erfinv errno errno_list error
syn keyword octaveFunction contained  error_ids errorbar errordlg etime etree
syn keyword octaveFunction contained  etreeplot eval evalin exec exist exit
syn keyword octaveFunction contained  exp expcdf expint expinv expm expm1
syn keyword octaveFunction contained  exppdf exprnd eye ezcontour ezcontourf
syn keyword octaveFunction contained  ezmesh ezmeshc ezplot ezplot3 ezpolar
syn keyword octaveFunction contained  ezsurf ezsurfc f_test_regression fact
syn keyword octaveFunction contained  factor factorial fail fcdf fclear fclose
syn keyword octaveFunction contained  fcntl fdisp feather feof ferror feval
syn keyword octaveFunction contained  fflush fft fft2 fftconv fftfilt fftn
syn keyword octaveFunction contained  fftshift fftw fgetl fgets fieldnames
syn keyword octaveFunction contained  figure file_in_loadpath file_in_path
syn keyword octaveFunction contained  fileattrib fileparts fileread filesep
syn keyword octaveFunction contained  fill filter filter2 find
syn keyword octaveFunction contained  find_dir_in_path findall findfigs
syn keyword octaveFunction contained  findobj findstr finite finv fix flag
syn keyword octaveFunction contained  flintmax flipdim fliplr flipud floor
syn keyword octaveFunction contained  fminbnd fminsearch fminunc fmod fnmatch
syn keyword octaveFunction contained  fopen fork formula fpdf fplot fprintf
syn keyword octaveFunction contained  fputs fractdiff fread freport freqz
syn keyword octaveFunction contained  freqz_plot frewind frnd fscanf fseek
syn keyword octaveFunction contained  fskipl fsolve ftell full fullfile
syn keyword octaveFunction contained  func2str functions fwrite fzero gallery
syn keyword octaveFunction contained  gamcdf gaminv gamma gammainc gammaln
syn keyword octaveFunction contained  gampdf gamrnd gca gcbf gcbo gcd gcf gco
syn keyword octaveFunction contained  ge gen_doc_cache genpath genvarname
syn keyword octaveFunction contained  geocdf geoinv geopdf geornd get
syn keyword octaveFunction contained  get_first_help_sentence get_help_text
syn keyword octaveFunction contained  get_help_text_from_file getappdata
syn keyword octaveFunction contained  getegid getenv geteuid getfield getgid
syn keyword octaveFunction contained  getgrent getgrgid getgrnam gethostname
syn keyword octaveFunction contained  getpgrp getpid getppid getpref getpwent
syn keyword octaveFunction contained  getpwnam getpwuid getrusage getuid
syn keyword octaveFunction contained  ginput givens glob glpk gls gmap40 gmres
syn keyword octaveFunction contained  gmtime gplot gradient gray gray2ind grid
syn keyword octaveFunction contained  griddata griddata3 griddatan gt gtext
syn keyword octaveFunction contained  guidata guihandles gunzip gzip hadamard
syn keyword octaveFunction contained  hamming hankel hanning hdl2struct
syn keyword octaveFunction contained  helpdlg hess hex2dec hex2num hggroup
syn keyword octaveFunction contained  hidden hilb hist histc home horzcat hot
syn keyword octaveFunction contained  hotelling_test hotelling_test_2 housh
syn keyword octaveFunction contained  hsv hsv2rgb hurst hygecdf hygeinv
syn keyword octaveFunction contained  hygepdf hygernd hypot idivide ifelse
syn keyword octaveFunction contained  ifft ifft2 ifftn ifftshift imag image
syn keyword octaveFunction contained  imagesc imfinfo imformats importdata
syn keyword octaveFunction contained  imread imshow imwrite ind2gray ind2rgb
syn keyword octaveFunction contained  ind2sub index inferiorto info inline
syn keyword octaveFunction contained  inpolygon input inputdlg inputname int16
syn keyword octaveFunction contained  int2str int32 int64 int8 interp1
syn keyword octaveFunction contained  interp1q interp2 interp3 interpft
syn keyword octaveFunction contained  interpn intersect intmax intmin inv
syn keyword octaveFunction contained  inverse invhilb ipermute iqr
syn keyword octaveFunction contained  is_absolute_filename is_dq_string
syn keyword octaveFunction contained  is_function_handle is_leap_year
syn keyword octaveFunction contained  is_rooted_relative_filename is_sq_string
syn keyword octaveFunction contained  is_valid_file_id isa isalnum isalpha
syn keyword octaveFunction contained  isappdata isargout isascii isaxes isbool
syn keyword octaveFunction contained  iscell iscellstr ischar iscntrl
syn keyword octaveFunction contained  iscolormap iscolumn iscomplex
syn keyword octaveFunction contained  isdebugmode isdefinite isdeployed
syn keyword octaveFunction contained  isdigit isdir isempty isequal isequaln
syn keyword octaveFunction contained  isequalwithequalnans isfield isfigure
syn keyword octaveFunction contained  isfinite isfloat isglobal isgraph
syn keyword octaveFunction contained  isguirunning ishandle ishermitian
syn keyword octaveFunction contained  ishghandle isieee isindex isinf
syn keyword octaveFunction contained  isinteger isjava iskeyword isletter
syn keyword octaveFunction contained  islogical islower ismac ismatrix
syn keyword octaveFunction contained  ismember ismethod isna isnan isnull
syn keyword octaveFunction contained  isnumeric isobject isocolors isonormals
syn keyword octaveFunction contained  isosurface ispc ispref isprime isprint
syn keyword octaveFunction contained  isprop ispunct isreal isrow isscalar
syn keyword octaveFunction contained  issorted isspace issparse issquare isstr
syn keyword octaveFunction contained  isstrprop isstruct issymmetric isunix
syn keyword octaveFunction contained  isupper isvarname isvector isxdigit
syn keyword octaveFunction contained  java2mat javaArray javaMethod javaObject
syn keyword octaveFunction contained  java_get java_invoke java_new java_set
syn keyword octaveFunction contained  javaaddpath javaclasspath javafields
syn keyword octaveFunction contained  javamem javamethods javarmpath jet kbhit
syn keyword octaveFunction contained  kendall keyboard kill
syn keyword octaveFunction contained  kolmogorov_smirnov_cdf
syn keyword octaveFunction contained  kolmogorov_smirnov_test
syn keyword octaveFunction contained  kolmogorov_smirnov_test_2 kron
syn keyword octaveFunction contained  kruskal_wallis_test krylov kurtosis
syn keyword octaveFunction contained  laplace_cdf laplace_inv laplace_pdf
syn keyword octaveFunction contained  laplace_rnd lasterr lasterror lastwarn
syn keyword octaveFunction contained  lcm ldivide le legend legendre length
syn keyword octaveFunction contained  lgamma license lin2mu line lines link
syn keyword octaveFunction contained  linkprop linsolve linspace
syn keyword octaveFunction contained  list_in_columns list_primes listdlg
syn keyword octaveFunction contained  loadaudio loadobj localtime log log10
syn keyword octaveFunction contained  log1p log2 logical logistic_cdf
syn keyword octaveFunction contained  logistic_inv logistic_pdf
syn keyword octaveFunction contained  logistic_regression logistic_rnd logit
syn keyword octaveFunction contained  loglog loglogerr logm logncdf logninv
syn keyword octaveFunction contained  lognpdf lognrnd logspace lookup lower
syn keyword octaveFunction contained  lsode lsode_options lsqnonneg lstat lt
syn keyword octaveFunction contained  lu luinc luupdate magic mahalanobis
syn keyword octaveFunction contained  make_absolute_filename manova mat2cell
syn keyword octaveFunction contained  mat2str matrix_type max mcnemar_test
syn keyword octaveFunction contained  md5sum mean meansq median menu merge
syn keyword octaveFunction contained  mesh meshc meshgrid meshz methods mex
syn keyword octaveFunction contained  mexext mfilename mgorth min minus
syn keyword octaveFunction contained  mislocked mkdir mkfifo mkpp mkstemp
syn keyword octaveFunction contained  mktime mldivide mlock mod mode moment
syn keyword octaveFunction contained  movefile mpoles mpower mrdivide msgbox
syn keyword octaveFunction contained  mtimes mu2lin munlock nargchk narginchk
syn keyword octaveFunction contained  nargoutchk nbincdf nbininv nbinpdf
syn keyword octaveFunction contained  nbinrnd nchoosek ndgrid ndims ne newplot
syn keyword octaveFunction contained  news nextpow2 nfields nnz nonzeros norm
syn keyword octaveFunction contained  normcdf normest norminv normpdf normrnd
syn keyword octaveFunction contained  not now nproc nth_element nthargout
syn keyword octaveFunction contained  nthroot ntsc2rgb null num2cell num2hex
syn keyword octaveFunction contained  num2str numel nzmax ocean
syn keyword octaveFunction contained  octave_config_info octave_tmp_file_name
syn keyword octaveFunction contained  ols onCleanup onenormest ones optimget
syn keyword octaveFunction contained  optimset or orderfields orient orth
syn keyword octaveFunction contained  ostrsplit pack paren pareto parseparams
syn keyword octaveFunction contained  pascal patch pathdef pause pbaspect pcg
syn keyword octaveFunction contained  pchip pclose pcolor pcr peaks
syn keyword octaveFunction contained  periodogram perl perms permute pie pie3
syn keyword octaveFunction contained  pink pinv pipe planerot playaudio plot
syn keyword octaveFunction contained  plot3 plotmatrix plotyy plus poisscdf
syn keyword octaveFunction contained  poissinv poisspdf poissrnd pol2cart
syn keyword octaveFunction contained  polar poly polyaffine polyarea polyder
syn keyword octaveFunction contained  polyderiv polyeig polyfit polygcd
syn keyword octaveFunction contained  polyint polyout polyreduce polyval
syn keyword octaveFunction contained  polyvalm popen popen2 postpad pow2 power
syn keyword octaveFunction contained  powerset ppder ppint ppjumps ppplot
syn keyword octaveFunction contained  ppval pqpnonneg prctile preferences
syn keyword octaveFunction contained  prepad primes print print_usage printd
syn keyword octaveFunction contained  printf prism probit prod profexplore
syn keyword octaveFunction contained  profile profshow prop_test_2 putenv puts
syn keyword octaveFunction contained  python qp qqplot qr qrdelete qrinsert
syn keyword octaveFunction contained  qrshift qrupdate quad quad_options
syn keyword octaveFunction contained  quadcc quadgk quadl quadv quantile
syn keyword octaveFunction contained  questdlg quit quiver quiver3 qz qzhess
syn keyword octaveFunction contained  rainbow rand rande randg randi randn
syn keyword octaveFunction contained  randp randperm range rank ranks rat rats
syn keyword octaveFunction contained  rcond rdivide re_read_readline_init_file
syn keyword octaveFunction contained  read_readline_init_file readdir
syn keyword octaveFunction contained  readline_re_read_init_file
syn keyword octaveFunction contained  readline_read_init_file readlink real
syn keyword octaveFunction contained  reallog realpow realsqrt record
syn keyword octaveFunction contained  rectangle rectint recycle refresh
syn keyword octaveFunction contained  refreshdata regexp regexpi regexprep
syn keyword octaveFunction contained  regexptranslate
syn keyword octaveFunction contained  register_graphics_toolkit rehash rem
syn keyword octaveFunction contained  remove_input_event_hook rename repelems
syn keyword octaveFunction contained  repmat reset reshape residue resize
syn keyword octaveFunction contained  restoredefaultpath rethrow rgb2hsv
syn keyword octaveFunction contained  rgb2ind rgb2ntsc rgbplot ribbon rindex
syn keyword octaveFunction contained  rmappdata rmdir rmfield rmpath rmpref
syn keyword octaveFunction contained  roots rose rosser rot90 rotdim round
syn keyword octaveFunction contained  roundb rows rref rsf2csf run_count
syn keyword octaveFunction contained  run_test rundemos runlength runtests
syn keyword octaveFunction contained  saveas saveaudio saveobj savepath scanf
syn keyword octaveFunction contained  scatter scatter3 schur sec secd sech
syn keyword octaveFunction contained  semicolon semilogx semilogxerr semilogy
syn keyword octaveFunction contained  semilogyerr set setappdata setaudio
syn keyword octaveFunction contained  setdiff setenv setfield setgrent setpref
syn keyword octaveFunction contained  setpwent setxor shading shell_cmd shift
syn keyword octaveFunction contained  shiftdim shrinkfaces sign sign_test
syn keyword octaveFunction contained  signbit sin sinc sind sinetone sinewave
syn keyword octaveFunction contained  single sinh size size_equal sizemax
syn keyword octaveFunction contained  sizeof skewness sleep slice sombrero
syn keyword octaveFunction contained  sort sortrows source spalloc sparse
syn keyword octaveFunction contained  spaugment spconvert spdiags spearman
syn keyword octaveFunction contained  spectral_adf spectral_xdf specular speed
syn keyword octaveFunction contained  spencer speye spfun sph2cart sphere
syn keyword octaveFunction contained  spinmap spline splinefit spones spparms
syn keyword octaveFunction contained  sprand sprandn sprandsym sprank spring
syn keyword octaveFunction contained  sprintf spstats spy sqp sqrt sqrtm
syn keyword octaveFunction contained  squeeze sscanf stairs stat statistics
syn keyword octaveFunction contained  std stdnormal_cdf stdnormal_inv
syn keyword octaveFunction contained  stdnormal_pdf stdnormal_rnd stem stem3
syn keyword octaveFunction contained  stemleaf stft str2double str2func
syn keyword octaveFunction contained  str2num strcat strchr strcmp strcmpi
syn keyword octaveFunction contained  strfind strftime strjoin strjust
syn keyword octaveFunction contained  strmatch strncmp strncmpi strptime
syn keyword octaveFunction contained  strread strrep strsplit strtok strtrim
syn keyword octaveFunction contained  strtrunc struct struct2cell struct2hdl
syn keyword octaveFunction contained  structfun strvcat studentize sub2ind
syn keyword octaveFunction contained  subplot subsasgn subsindex subspace
syn keyword octaveFunction contained  subsref substr substruct sum summer
syn keyword octaveFunction contained  sumsq superiorto surf surface surfc
syn keyword octaveFunction contained  surfl surfnorm svd svds swapbytes syl
syn keyword octaveFunction contained  sylvester_matrix symamd symbfact symlink
syn keyword octaveFunction contained  symrcm symvar synthesis system t_test
syn keyword octaveFunction contained  t_test_2 t_test_regression table tan
syn keyword octaveFunction contained  tand tanh tar tcdf tempdir tempname
syn keyword octaveFunction contained  terminal_size tetramesh text textread
syn keyword octaveFunction contained  textscan tic tilde_expand time times
syn keyword octaveFunction contained  tinv title tmpfile tmpnam toascii toc
syn keyword octaveFunction contained  toeplitz tolower toupper tpdf trace
syn keyword octaveFunction contained  transpose trapz treelayout treeplot tril
syn keyword octaveFunction contained  trimesh triplequad triplot trisurf triu
syn keyword octaveFunction contained  trnd tsearch tsearchn typecast typeinfo
syn keyword octaveFunction contained  u_test uicontextmenu uicontrol uigetdir
syn keyword octaveFunction contained  uigetfile uimenu uint16 uint32 uint64
syn keyword octaveFunction contained  uint8 uipanel uipushtool uiputfile
syn keyword octaveFunction contained  uiresume uitoggletool uitoolbar uiwait
syn keyword octaveFunction contained  umask uminus uname undo_string_escapes
syn keyword octaveFunction contained  unidcdf unidinv unidpdf unidrnd unifcdf
syn keyword octaveFunction contained  unifinv unifpdf unifrnd union unique
syn keyword octaveFunction contained  unix unlink unmkpp unpack untabify untar
syn keyword octaveFunction contained  unwrap unzip uplus upper urlread
syn keyword octaveFunction contained  urlwrite usage usejava usleep
syn keyword octaveFunction contained  validatestring vander var var_test vec
syn keyword octaveFunction contained  vech vectorize ver version vertcat view
syn keyword octaveFunction contained  voronoi voronoin waitbar waitfor
syn keyword octaveFunction contained  waitforbuttonpress waitpid warndlg
syn keyword octaveFunction contained  warning warning_ids warranty waterfall
syn keyword octaveFunction contained  wavread wavwrite wblcdf wblinv wblpdf
syn keyword octaveFunction contained  wblrnd weekday welch_test white whitebg
syn keyword octaveFunction contained  wienrnd wilcoxon_test wilkinson winter
syn keyword octaveFunction contained  xlabel xlim xor yes_or_no ylabel ylim
syn keyword octaveFunction contained  yulewalker z_test z_test_2 zeros zip
syn keyword octaveFunction contained  zlabel zlim zscore

" Add functions defined in .m file being read to list of highlighted functions
function! s:CheckForFunctions()
  let i = 1
  while i <= line('$')
    let line = getline(i)
    " Only look for functions at start of line.
    " Commented function, '# function', will not trigger as match returns 3
    if match(line, '\Cfunction') == 0
      let line = substitute(line, '\vfunction *([^(]*\= *)?', '', '')
      let nfun = matchstr(line, '\v^\h\w*')
      if !empty(nfun)
        execute "syn keyword octaveFunction" nfun
      endif
    " Include anonymous functions 'func = @(...)'.
    " Use contained keyword to prevent highlighting on LHS of '='
    elseif match(line, '\<\(\h\w*\)\s*=\s*@\s*(') != -1
      let list = matchlist(line, '\<\(\h\w*\)\s*=\s*@\s*(')
      let nfun = list[1]
      if !empty(nfun)
        execute "syn keyword octaveFunction contained" nfun
      endif
    endif
    let i = i + 1
  endwhile
endfunction

call s:CheckForFunctions()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Define clusters for ease of writing subsequent rules
syn cluster AllFuncVarCmd contains=octaveVariable,octaveFunction,octaveCommand
syn cluster AllFuncSetCmd contains=octaveSetVarFun,octaveFunction,octaveCommand

" Switch highlighting of variables based on coding use.
" Query -> Constant, Set -> Function
" order of items is is important here
syn match octaveQueryVar "\<\h\w*[^(]"me=e-1  contains=@AllFuncVarCmd
syn match octaveSetVar   "\<\h\w*\s*("me=e-1  contains=@AllFuncSetCmd
syn match octaveQueryVar "\<\h\w*\s*\((\s*)\)\@="  contains=@AllFuncVarCmd

" Don't highlight Octave keywords on LHS of '=', these are user vars
syn match octaveUserVar  "\<\h\w*\ze[^<>!~="']\{-}==\@!"
syn match octaveUserVar  "\<\h\w*\s*[<>!~=]=" contains=octaveVariable

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Errors (placed early so they may be overriden by more specific rules
" Struct with nonvalid identifier starting with number (Example: 1.a or a.1b)
syn region octaveError  start="\<\d\+\(\w*\.\)\@="  end="[^0-9]"he=s-1 oneline
syn region octaveError  start="\.\d\+\(\w*\)\@="hs=s+1  end="[^0-9]"he=s-1 oneline
" Numbers with double decimal points (Example: 1.2.3)
syn region octaveError  start="\<-\?\d\+\.\d\+\.[^*/\\^]"hs=e-1 end="\>"  oneline
syn region octaveError  start="\<-\?\d\+\.\d\+[eEdD][-+]\?\d\+\.[^*/\\^]"hs=e-1 end="\>"  oneline

" Operators
" Uncommment "Hilink octaveOperator" below to highlight these
syn match octaveLogicalOperator     "[&|~!]"
syn match octaveArithmeticOperator  "\.\?[-+*/\\^]"
syn match octaveRelationalOperator  "[=!~]="
syn match octaveRelationalOperator  "[<>]=\?"

" User Variables
" Uncomment this syntax group and "Hilink octaveIdentifier" below to highlight
"syn match octaveIdentifier  "\<\h\w*\>"

" Strings
syn region octaveString  start=/'/  end=/'/  skip=/''/ contains=octaveLineContinuation,@Spell
syn region octaveString  start=/"/  end=/"/  skip=/\\./re=e+1 contains=octaveLineContinuation,@Spell

" Standard numbers
syn match octaveNumber  "\<\d\+[ij]\?\>"
" Floating point number, with dot, optional exponent
syn match octaveFloat   "\<\d\+\(\.\d*\)\?\([edED][-+]\?\d\+\)\?[ij]\?\>"
" Floating point number, starting with a dot, optional exponent
syn match octaveFloat   "\.\d\+\([edED][-+]\?\d\+\)\?[ij]\?\>"

" Delimiters and transpose character
syn match octaveDelimiter          "[][(){}@]"
syn match octaveTransposeOperator  "[])}[:alnum:]._]\@<='"

" Tabs, for possibly highlighting as errors
syn match octaveTab  "\t"
" Other special constructs
syn match octaveSemicolon  ";"
syn match octaveTilde "\~\s*[[:punct:]]"me=e-1

" Line continuations, order of matches is important here
syn match octaveLineContinuation  "\.\{3}$"
syn match octaveLineContinuation  "\\$"
syn match octaveError  "\.\{3}.\+$"hs=s+3
syn match octaveError  "\\\s\+$"hs=s+1
" Line continuations w/comments
syn match octaveLineContinuation  "\.\{3}\s*[#%]"me=e-1
syn match octaveLineContinuation  "\\\s*[#%]"me=e-1

" Comments, order of matches is important here
syn keyword octaveFIXME contained  FIXME TODO
syn match  octaveComment  "[%#].*$"  contains=octaveFIXME,octaveTab,@Spell
syn match  octaveError    "[#%][{}]"
syn region octaveBlockComment  start="^\s*[#%]{\s*$"  end="^\s*[#%]}\s*$" contains=octaveFIXME,octaveTab,@Spell

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Apply highlight groups to syntax groups defined above

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_octave_syntax_inits")
  if version < 508
    let did_octave_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink octaveBeginKeyword             Conditional
  HiLink octaveElseKeyword              Conditional
  HiLink octaveEndKeyword               Conditional
  HiLink octaveVarKeyword               Conditional
  HiLink octaveReserved                 Conditional

  HiLink octaveStatement                Statement
  HiLink octaveVariable                 Constant
  HiLink octaveSetVarFun                Function
  HiLink octaveCommand                  Statement
  HiLink octaveFunction                 Function

  HiLink octaveConditional              Conditional
  HiLink octaveLabel                    Label
  HiLink octaveRepeat                   Repeat
  HiLink octaveFIXME                    Todo
  HiLink octaveString                   String
  HiLink octaveDelimiter                Identifier
  HiLink octaveNumber                   Number
  HiLink octaveFloat                    Float
  HiLink octaveError                    Error
  HiLink octaveComment                  Comment
  HiLink octaveBlockComment             Comment
  HiLink octaveSemicolon                SpecialChar
  HiLink octaveTilde                    SpecialChar
  HiLink octaveLineContinuation         Special

  HiLink octaveTransposeOperator        octaveOperator
  HiLink octaveArithmeticOperator       octaveOperator
  HiLink octaveRelationalOperator       octaveOperator
  HiLink octaveLogicalOperator          octaveOperator

" Optional highlighting
"  HiLink octaveOperator                Operator
"  HiLink octaveIdentifier              Identifier
"  HiLink octaveTab                     Error

  delcommand HiLink
endif

let b:current_syntax = "octave"

"EOF	vim: ts=2 et tw=80 sw=2 sts=0
