# Directories

bin_d  = $(abspath ftplugin/bin)

# Installation paths.
#dest_root = $(HOME)/.vim/bundle/fsharpbinding-vim/
#dest_bin  = $(dest_root)/ftplugin/bin/

ac_exe     = $(bin_d)/fsautocomplete.exe
ac_archive = fsautocomplete.zip
ac_version = 0.23.0
ac_url     = https://github.com/fsharp/FSharp.AutoComplete/releases/download/$(ac_version)/$(ac_archive)

# ----------------------------------------------------------------------------

# Building

fsautocomplete : $(ac_exe)
$(ac_exe) : $(bin_d)
	curl -L "$(ac_url)" -o "$(bin_d)/$(ac_archive)"
	unzip "$(bin_d)/$(ac_archive)" -d "$(bin_d)"
	touch "$(ac_exe)"

~/.config/.mono/certs:
	mozroots --import --sync --quiet

$(dest_root) :; mkdir -p $(dest_root)
$(dest_bin)  :; mkdir -p $(dest_bin)
$(bin_d)     :; mkdir -p $(bin_d)

# Cleaning

clean :
	rm -rf $(bin_d)

.PHONY: fsautocomplete
