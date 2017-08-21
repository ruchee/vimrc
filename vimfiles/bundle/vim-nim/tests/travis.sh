#!/bin/bash

current=$PWD


install_nim() {
    wget "http://nim-lang.org/download/nim-0.13.0.tar.xz" -O nim.tar.xz
    mkdir nim
    tar -xvf nim.tar.xz -C nim --strip-components=1
    cd nim
    ./build.sh
    export PATH=$PATH:$PWD/bin
    cd ..
}

install_nimble() {
    git clone https://github.com/nim-lang/nimble.git
    cd nimble
    git clone -b v0.13.0 --depth 1 https://github.com/nim-lang/nim vendor/nim
    nim c -r src/nimble
    export PATH=$PATH:$PWD/src
    cd ..
}

install_nimsuggest() {
    git clone https://github.com/nim-lang/nimsuggest
    cd nimsuggest
    echo "y" | nimble build
    export PATH=$PATH:$PWD
    cd ..
}


if [[ ! -d "tmp" ]]; then
    mkdir tmp
    cd tmp

    echo "Installing nim"
    install_nim

    echo "Installing nimble"
    install_nimble

    echo "Installing nimsuggest"
    install_nimsuggest

    cd ..
fi

export PATH=$PATH:$current/tmp/nim/bin
export PATH=$PATH:$current/nimble/src
export PATH=$PATH:$current/nimble/nimsuggest

echo "================================================================================"

echo -e "\nNeovim:"
nvim --version

echo -e "\nNim:"
nim --version

echo -e "\nNimble:"
nimble --version

echo -e "\nNimsuggest:"
nimsuggest --version

echo "================================================================================"

cd $current/tests
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
cp nvimcfg/init.vim ~/.config/nvim/
mkdir -p "$HOME/.config/nvim/undodir"
mkdir -p "$HOME/.config/nvim/autoload"
mkdir -p "$HOME/.config/nvim/view"

cd ~/.config/nvim
mkdir plugged
cd plugged
git clone https://github.com/baabelfish/nvim-nim
tree ~/.config/nvim


echo "================================================================================"
echo "Run vim tests"
cd $current/tests
./run_tests.sh
