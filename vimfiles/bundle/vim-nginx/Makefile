MODULES := $(shell find syntax/modules -name '*.vim') 

default: syntax/nginx.vim

syntax/nginx.vim: syntax/layout/nginx.vim $(MODULES)
	cat syntax/layout/nginx.vim | perl -pe 's/^" \@3PARTY\n/`cat syntax\/modules\/*`/ge' > syntax/nginx.vim
