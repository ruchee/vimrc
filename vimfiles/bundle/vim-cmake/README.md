# vim-cmake
[![Travis (Linux)](https://travis-ci.org/vhdirk/vim-cmake.svg?branch=master)](https://travis-ci.org/vhdirk/vim-cmake)
[![AppVeyor (Windows)](https://ci.appveyor.com/api/projects/status/ns1o9252o2rrmv6g?svg=true)](https://ci.appveyor.com/project/vhdirk/vim-cmake)

vim-cmake is a Vim plugin to make working with CMake a little nicer.

I got tired of navigating to the build directory each time, and I also
disliked setting makeprg manually each time. This plugin does just that.

## Usage

### Commands

 * `:CMake` searches for the closest directory named build in an upwards search,
and whenever one is found, it runs the cmake command there, assuming the CMakeLists.txt
file is just one directory above. Any arguments given to :CMake will be directly passed
on to the cmake command. It also sets the working directory of the make command, so
you can just use quickfix as with a normal Makefile project.
If you have the [AsyncRun plugin](https://github.com/skywind3000/asyncrun.vim)
installed, it will be used automatically and you will be able to check the
result of the cmake command in the quickfix as well.

 * `:CMakeClean` deletes all files in the build directory. You can think of this as a CMake version of make clean.

 * `:CMakeFindBuildDir` resets the build directory path set for the current buffer and then tries to find a new one. Useful if it previously found a wrong path to then reset it after a new build folder has been created for example.

### Variables

 * `g:cmake_install_prefix` same as `-DCMAKE_INSTALL_PREFIX`

 * `g:cmake_build_type` same as `-DCMAKE_BUILD_TYPE`

 * `g:cmake_cxx_compiler` same as `-DCMAKE_CXX_COMPILER`. Changes will have no effect until you run :CMakeClean and then :CMake.

 * `g:cmake_c_compiler` same as `-DCMAKE_C_COMPILER`. Changes will have no effect until you run :CMakeClean and then :CMake.

 * `g:cmake_build_shared_libs` same as `-DBUILD_SHARED_LIBS`

 * `g:cmake_project_generator` same as `-G`. Changes will have no effect until you run :CMakeClean and then :CMake.

 * `g:cmake_export_compile_commands` same as `-DCMAKE_EXPORT_COMPILE_COMMANDS`.

 * `g:cmake_ycm_symlinks` create symlinks to the generated compilation database for use with [YouCompleteMe](https://github.com/Valloric/YouCompleteMe/).

 * `b:build_dir` is the path to the cmake build directory for the current buffer. This variable is set with the first :CMake or :CMakeFindBuildDir call. Once found, it will not be searched for again unless you call :CMakeFindBuildDir. If automatic finding is not sufficient you can set this variable manually to the build dir of your choice.


## Installation


### Vim-pathogen

With [pathogen.vim](https://github.com/tpope/vim-pathogen) simply copy and paste:

    cd ~/.vim/bundle
    git clone git://github.com/vhdirk/vim-cmake.git

Once help tags have been generated, you can view the manual with
`:help cmake`.

### Vundle

With [Vundle.vim](https://github.com/VundleVim/Vundle.vim) simply add this repository to your plugins list:

    Plugin 'vhdirk/vim-cmake'

## Acknowledgements

 * Thanks to [Tim Pope](http://tpo.pe/), his plugins are really awesome.
 * Thanks to [Junegunn Choi](https://junegunn.kr/), for [vader.vim](https://github.com/junegunn/vader.vim), which is the testing framework used for this plugin.
 * Also thanks to
    * @SteveDeFacto for extending this with more fine grained control.
    * @snikulov for enhancing makeprg.
    * @dapicester for allowing specifying targets.
    * @thomasgubler for the build dir option.
    * @antmd for fixing a bug with handing of spaces in directory names.
    * @jmirabel for fixing concatenation of cmake arguments.
    * @T4ng10r for the project generator option.
    * @Squareys for a small overhaul of the project, the initial test and travis setup.

## License

Copyright (c) Dirk Van Haerenborgh, @SteveDeFacto. Distributed under the same terms as Vim itself.
See `:help license`.
