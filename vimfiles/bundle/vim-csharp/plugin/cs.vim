" Vim plugin file
" Language:     Microsoft C#
" Maintainer:   Kian Ryan (kian@orangetentacle.co.uk)
" Last Change:  2012 Sep 23

function! MsProjFile(file)
    let g:net_build_file = a:file
    compiler msbuild
endfunction

function! MsVersion(version)
    let g:net_framework_version = a:version
    compiler msbuild
endfunction

function! IISExpress()
    e ~\Documents\IISExpress\config\applicationhost.config
endfunction

" Set msproj file extensions
au BufNewFile,BufRead *.proj compiler msbuild | set filetype=xml
au BufNewFile,BufRead *.csproj compiler msbuild | set filetype=xml
au BufNewFile,BufRead *.sln compiler msbuild | set filetype=xml
au BufNewFile,BufRead *.cshtml compiler msbuild
au BufNewFile,BufRead *.aspx compiler msbuild | set filetype=html syntax=aspx
au BufNewFile,BufRead *.ascx compiler msbuild | set filetype=html syntax=aspx

com! -complete=file -nargs=1 MsProjFile :call MsProjFile(<f-args>)
com! -nargs=1 MsVersion :call MsVersion(<f-args>)
com! IISExpress :call IISExpress()
