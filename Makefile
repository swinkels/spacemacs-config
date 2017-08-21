ifeq (,$(wildcard ~/.emacs.d))
	$(error There already exists an ~/.emacs.d directory.)
endif

ifeq (,$(wildcard ~/.spacemacs))
	$(error There already exists a ~/.spacemacs configuration file.)
endif

all:
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
	cd ~/.emacs.d/private ; git clone https://github.com/swinkels/spacemacs-config
	ln -s ~/.emacs.d/private/spacemacs-config/.spacemacs
