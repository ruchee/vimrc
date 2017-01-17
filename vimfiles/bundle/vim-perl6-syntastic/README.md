# Perl 6 support for vim-syntastic

![Alt text](/../master/screenshot-perl6.png?raw=true "Screenshot")


While the Vim syntax check plugin [syntastic]
(https://github.com/scrooloose/syntastic "Syntastic Vim plugin") offers Perl 5
support, it does not at the moment support Perl 6.  This plugin implements
Perl 6 support (until these changes are incorporated upstream).

[ Since the 2016.09 Rakudo release, rakudo supports JSON error support. This
plugin will be adapted accordingly. Rakudo 2016.09 will be the minimum
required version. This regex based functionality will be archived in a
branch. Work in progress. ]

## Installation & configuration
You need to install syntastic to use this plugin. Instructions for
[pathogen plugin manager] (https://github.com/tpope/vim-pathogen "vim-pathogen"):
```
$ cd ~/.vim && git clone https://github.com/scrooloose/syntastic.git ~/.vim/bundle/synastic
$ cd ~/.vim && git clone https://github.com/nxadm/syntastic-perl6.git ~/.vim/bundle/synastic-perl6
```
Type ":Helptags" in Vim to generate Help Tags.

Syntastic and syntastic-perl6 vimrc configuration (comments start with "):
```
"airline statusbar integration if installed
"set laststatus=2
"set ttimeoutlen=50
"let g:airline#extensions#tabline#enabled = 1
"let g:airline_theme='luna'
"In order to see the powerline fonts, adapt the font of your terminal
"In Gnome Terminal: "use custom font" in the profile. I use Monospace regular.
"let g:airline_powerline_fonts = 1

"syntastic syntax checking
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
"Perl 6 support
"Optional comma separated list of quoted paths to be included to -I
"let g:syntastic_perl6_lib_path = [ '/home/user/Code/some_project/lib', 'lib' ]
"Optional perl6 binary (defaults to perl6)
"let g:syntastic_perl6_interpreter = '/home/claudio/tmp/perl6'
"Register the checker provided by this plugin
let g:syntastic_perl6_checkers = [ 'perl6latest']
"Enable the perl6latest checker
let g:syntastic_enable_perl6latest_checker = 1
```
## Module path of your code
There are two ways of dealing with unknown lib path perl6 errors,
you can populate the g:syntastic_perl6_lib_path with default lib dirs,
and/or more practically set the PERL6LIB shell variable in the shell that will
run vim:
```
$ export PERL6LIB=~/Code/Perl6Module/lib:~/Code/SomeOtherModule/lib
$ vim my_perl6_program
```

## Make this plugin better
- Send a PR to make the code (vimscript) better where needed.
- Send a PR to add tests for error cases not yet tested (see the t directory)
or -more importantely- caught.
- Post an issue if you find a bug or find errors not caught. In that case
copy-paste the error (e.g. within vim: :!perl6 -c %) and post a sample
of the erroneous Perl 6 code in question.

## Implemented compile time errors
- Show regular errors of the type:
```
===SORRY!=== Error while compiling <file>
<reason>
at <file:line>
------> <place of program eject>
```
- Show error for undeclared rountines/names/... :
```
===SORRY!=== Error while compiling <file>
Undeclared <type>:
    <name> used at line <line>
```
- Show error for not found libraries (use):
```
===SORRY!===
Could not find <name> at line <line> in:
   <paths>
```
- Hightlight error in text for errors reported by:
    - ------> and ⏏ (eject)
    - "Can only use..."
    - "Undeclared..."
    - "Could not find..."

## Author
nxadm (El_Che @ #perl6 (freenode))

A big thanks to lcd047 (syntasic contributor) for the many pointers and corrections.
