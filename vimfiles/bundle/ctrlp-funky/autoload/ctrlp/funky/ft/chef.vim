" Language: Ruby (chef)
" Author: Takahiro Yoshihara
" License: The MIT License

function! s:resources()
  " Chef Resources "{{{
  " http://docs.opscode.com/resource.html
  let resources = [
        \ 'apt_package',
        \ 'chef_gem',
        \ 'cookbook_file',
        \ 'cron',
        \ 'deploy',
        \ 'directory',
        \ 'dpkg_package',
        \ 'easy_install_package',
        \ 'env',
        \ 'erl_call',
        \ 'execute',
        \ 'file',
        \ 'freebsd_package',
        \ 'gem_package',
        \ 'git',
        \ 'group',
        \ 'http_request',
        \ 'ifconfig',
        \ 'ips_package',
        \ 'link',
        \ 'log',
        \ 'macports_package',
        \ 'mdadm',
        \ 'mount',
        \ 'ohai',
        \ 'package',
        \ 'pacman_package',
        \ 'portage_package',
        \ 'powershell_script',
        \ 'remote_directory',
        \ 'remote_file',
        \ 'rpm_package',
        \ 'route',
        \ 'ruby_block',
        \ 'scm',
        \ 'script',
        \ 'service',
        \ 'smartos_package',
        \ 'solaris_package',
        \ 'subversion',
        \ 'template',
        \ 'user',
        \ 'yum_package'
  \ ] "}}}
  return resources
endfunction

function! s:lwrp()
  " LWRP "{{{
  " http://docs.opscode.com/lwrp.html
  let lwrp = [
        \ 'apt',
        \ 'aws',
        \ 'bluepill',
        \ 'chef_handler',
        \ 'daemontools',
        \ 'djbdns',
        \ 'dmg',
        \ 'dynect',
        \ 'firewall',
        \ 'freebsd',
        \ 'gunicorn',
        \ 'homebrew',
        \ 'iis',
        \ 'maven',
        \ 'mysql',
        \ 'nagios',
        \ 'pacman',
        \ 'php',
        \ 'powershell',
        \ 'python',
        \ 'rabbitmq',
        \ 'riak',
        \ 'samba',
        \ 'sudo',
        \ 'supervisor',
        \ 'transmission',
        \ 'users',
        \ 'webpi',
        \ 'windows',
        \ 'yum',
        \ 'zenoss'
  \ ] "}}}
  return lwrp
endfunction

function! ctrlp#funky#ft#chef#filters()
  let res = s:resources()

  if get(g:, 'ctrlp_funky_chef_lwrp', 1)
    call extend(res, map(s:lwrp(), 'v:val."_\\S\\+"'))
  endif

  " user defined resources
  call extend(res, get(g:, 'ctrlp_funky_chef_custom_resources', []))

  let filters = [
        \ { 'pattern': '\m\C^[\t ]*\('.join(res, '\|').'\)[\t ]\+\S\+',
        \   'formatter': ['\m\C^[\t ]*', '', 'g'] }
  \ ]

  return filters
endfunction
" vim: fdm=marker
