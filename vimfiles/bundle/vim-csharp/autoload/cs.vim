" Vim plugin functions
" Language:     Microsoft C#
" Maintainer:   Kian Ryan (kian@orangetentacle.co.uk)
" Last Change:  2012 Sep 23

function! s:get_net_framework_dir(version)

    if exists("g:net_framework_top")
        net_framework_top = g:net_framework_top
    else
        let net_framework_top = "c:\\windows\\Microsoft.NET\\Framework\\"
    endif

    if a:version == "1"
        return net_framework_top . "v1.1.4322\\"
    elseif a:version == "2"
        return net_framework_top . "v2.0.50727\\"
    elseif a:version == "3.5"
        return net_framework_top . "v3.5\\"
    elseif a:version == "4"
        return net_framework_top . "v4.0.30319\\"
    endif

endfunction

function! cs#get_net_compiler(compiler)

    if exists("g:net_framework_version")
        let msbuild = s:get_net_framework_dir(g:net_framework_version) . a:compiler
        return msbuild
    else
        for i in ["4","3.5","2","1"]
            let msbuild = s:get_net_framework_dir(i) . a:compiler
            if findfile(msbuild) != ""
                return msbuild
            endif
        endfor
    endif

    echom "Unable to find " . a:compiler
endfunction

function! cs#find_net_solution_file()
    " Just consider first match
    return get(split(globpath(".", "*.sln"), "\n"), 0, "")
endfunction

