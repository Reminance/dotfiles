# dotfiles
# inspired by:
# https://github.com/masasam/dotfiles

BASE = $(shell pwd)
SCRIPTS = $(BASE)/scripts
LN = ln -vsf
LNDIR = ln -vs
MKDIR = mkdir -p
PKGINSTALL = sudo pacman --noconfirm -S
SYSTEMD_ENABLE := sudo systemctl --now enable

.DEFAULT_GOAL := help
.PHONY: all allinstall nextinstall allupdate allbackup

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

sudo: ## stop asking for pasword when useing sudo
	sudo echo "$wheel ALL=(All) NOPASSWD: ALL" >> /etc/sudoers

${HOME}/.local:
	$(MKDIR) $<

ssh: ## Init ssh
	$(PACMAN) open$@
	$(MKDIR) ${HOME}/.$@
	$(LN) {${BASE},${HOME}}/.ssh/{config,known_hosts}
	chmod 600 ${HOME}/.ssh/id_rsa

init: ## Initial deploy dotfiles
	test -L ${HOME}/.config/emacs || rm -rf ${HOME}/.config/emacs
	$(LN) ${BASE}/.config/emacs ${HOME}/.config/emacs
	$(LN) ${BASE}/.lesskey ${HOME}/.lesskey
	lesskey
	for item in zshrc vimrc bashrc npmrc myclirc tmux.conf screenrc aspell.conf gitconfig netrc authinfo; do
		$(LN) {${BASE},${HOME}}/.$$item
	done
	$(MKDIR) ${HOME}/.config/mpv
	$(LN) {${BASE},${HOME}}/.config/mpv/mpv.conf
	$(LN) {${BASE},${HOME}}/.config/hub
	sudo $(LN) {${BASE},}/etc/hosts

base: ## Install base and base-devel package
	$(PKGINSTALL) $(BASE_PKGS)

install: ## Install some extra packages
	$(PKGINSTALL) --needed - < $(BASE)/archlinux/pacmanList
	$(PKGINSTALL) --needed $(PACKAGES)

aur: ## Install arch linux AUR packages using yay
	yay -S --needed - < $(base)/archlinux/aurlist

backup: ## Backup arch linux packages
	mkdir -p ${PWD}/archlinux
	pacman -Qnq > ${BASE}/archlinux/pacmanlist
	pacman -Qqem > ${BASE}/archlinux/aurlist

alacritty: ## Init alacritty
	$(PKGINSTALL) $@
	test -L ${HOME}/.config/$@/$@.yml || rm -rf ${HOME}/.config/$@/$@.yml
	$(LN) {${BASE},${HOME}}/.config/$@/$@.yml

intel: ## Setup Intel Graphics
	sudo $(LN) {${BASE},}/etc/X11/xorg.conf.d/20-intel.conf

maria-db: mariadb
mariadb: ## Mariadb initial setup
	sudo $(LN) {${BASE},}/etc/sysctl.d/40-max-user-watches.conf
	$(PKGINSTALL) $@ $@-clients
	sudo $(LN) {${BASE},}/etc/my.cnf
	sudo mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
	$(SYSTEMD_ENABLE) $@.service
	sudo mysql -u root < ${BASE}/$@/init.sql
	mysql_secure_installation
	mysql_tzinfo_to_sql /usr/share/zoneinfo | mysql -u root mysql

.ONESHELL:
postgresql: ## PostgreSQL initial setup
	$(PKGINSTALL) $@
	cd /home
	sudo -u postgres initdb -E UTF8 --no-locale -D '/var/lib/postgres/data'
	$(SYSTEMD_ENABLE) postgresql.service
	sudo -u postgres createuser --interactive

remotedesktop: ## Install remotedesktop
	$(PKGINSTALL) remmina freerdp libvncserver

mycli: ## Init mycli
	$(MKDIR) ${HOME}/backup/$@
	pip install --user $@
	$(LN) ${HOME}{/backup/$@,}/.$@-history

pgcli: ## Init pgcli
	$(MKDIR) ${HOME}/backup
	pip install --user $@
	test -L ${HOME}/.config/$@ || rm -rf ${HOME}/.config/$@
	$(LN) ${HOME}/{backup,.config}/$@

redis: ## Redis inital setup
	$(PKGINSTALL) $@
	$(SYSTEMD_ENABLE) $@.service

mongodb: ## Mongodb initial setup
	$(PKGINSTALL) $@ $@-tools
	$(SYSTEMD_ENABLE) $@.service

pipinstall: ${HOME}/.local ## Install python packages
	curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
	python ${BASE}/get-pip.py --user
	sudo $(LN) ${BASE}/usr/share/zsh/site-functions/_pipenv /usr/share/zsh/site-functions/_pipenv
	pip install --user --upgrade pip
	pip install --user $(PIP_PKGS)
	rm -fr get-pip.py

pipbackup: ## Backup python packages
	mkdir -p ${PWD}/archlinux
	pip freeze > ${PWD}/archlinux/requirements.txt

piprecover: ## Recover python packages
	mkdir -p ${PWD}/archlinux
	pip install --user -r ${PWD}/archlinux/requirements.txt

pipupdate: ## Update python packages
	pip list --user | cut -d" " -f 1 | tail -n +3 | xargs pip install -U --user

goinstall: ${HOME}/.local ## Install go packages
	GO111MODULE="on" go get golang.org/x/tools/gopls@latest
	GO111MODULE="on" go get -u -v golang.org/x/tools/cmd/goimports
	GO111MODULE="on" go get -u -v github.com/x-motemen/ghq
	go get -u -v github.com/kyoshidajp/ghkw
	go get -u -v github.com/simeji/jid/cmd/jid
	go get -u -v github.com/jmhodges/jsonpp
	GO111MODULE="on" go get -u -v github.com/mithrandie/csvq

nodeinstall: ## Install node packages
	sudo pacman -S yarn
	$(MKDIR) $${HOME}/.node_modules
	for pkg in $(NODE_PKGS); do yarn global add $$pkg; done

yarnupdate: ## Update yarn packages
	yarn global upgrade

rustinstall: ## Install rust and rust language server
	sudo pacman -S rustup
	rustup default stable
	rustup component add rls rust-analysis rust-src

rustupdate: ## Update rust packages
	cargo install-update -a

docker: ## Docker initial setup
	$(PKGINSTALL) $@ $@-compose
	sudo usermod -aG $@ ${USER}
	$(SYSTEMD_ENABLE) $@.service

docker_image: docker
	docker build -t dotfiles ${BASE}

podman: ## Podman initial setup
	$(PKGINSTALL) $@
	$(SYSTEMD_ENABLE) io.$@.service

testbackup: docker_image ## Test this Makefile with mount backup directory
	docker run -it --name make$@ -v /home/${USER}/backup:${HOME}/backup:cached --name makefiletest -d dotfiles:latest /bin/bash
	for target in base install init neomutt aur pipinstall goinstall nodeinstall; do
		docker exec -it make$@ sh -c "cd ${BASE}; make $${target}"
	done

test: docker_image ## Test this Makefile with docker without backup directory
	docker run -it --name make$@ -d dotfiles:latest /bin/bash
	for target in init install aur pipinstall goinstall nodeinstall; do
		docker exec -it make$@ sh -c "cd ${BASE}; make $${target}"
	done

all: allinstall nextinstall allupdate allbackup

allinstall: rclone gnupg ssh install init keyring urxvt xterm termite yay tlp thinkpad ttf-cica dnsmasq pipinstall goinstall ibusmozc neomutt docker nodeinstall zeal lvfs gcloud awsv2 toggle aur beekeeper kind eralchemy mpsyt gh

nextinstall: chrome rubygem rbenv rustinstall postgresql maria-db mycli pgcli

allupdate: update pipupdate rustupdate goinstall yarnupdate

allbackup: backup pipbackup

BASE_PKGS := filesystem gcc gcc-libs glibc libtool bash coreutils file findutils gawk grep sed
BASE_PKGS += base base-devel pciutils psmisc shadow util-linux tar unzip unrar bzip2 gzip xz
BASE_PKGS += iputils iproute2 autoconf sudo automake binutils bison fakeroot flex groff licenses
BASE_PKGS += systemd systemd-sysvcompat make patch pkgconf texinfo which gettext m4 procps-ng

PACKAGES := go gopls java nodejs rust rustup clang pyright rust-analyzer python ruby rubygems gdb
PACKAGES += nodejs npm yarn lua nasm maven gradle ctags gtags typescript llvm llvm-libs lldb
PACKAGES += zsh zsh-completions zsh-syntax-highlighting git lazygit vim neovim emacs tmux
PACKAGES += dmenu clash fcitx fcitx-configtool fcitx-im fcitx-googlepinyin google-chrome bat
PACKAGES += i3 i3-gaps i3status i3blocks alacritty picom screenkey flameshot w3m wget ncdu
PACKAGES += fzf the_silver_searcher ripgrep fd tree shellcheck bash-completion cmatrix imagemagick
PACKAGES += pandoc htop curl jq mupdf zathura figlet mpc mpd mpv feh nitrogen ffmpeg man-db
PACKAGES += netease-cloud-music ntfs-3g pcmanfm sddm dunst bc sysstat yad archiso qemu
PACKAGES += ethtool xsel xclip openfortivpn openssl openvpn pacman-contrib peek aria2 nmap tcpdump
PACKAGES += ttc-iosevka noto-fonts-emoji wqy-zenhei nerd-fonts-source-code-pro
PACKAGES += pkgstats alsa-utils keychain gpaste xdotool opencv rsync inotify-tools festival
PACKAGES += asciidoc asciidoctor linux-docs arch-install-scripts virtualbox-host-modules-arch

NODE_PKGS := babel-eslint bash-language-server cloc create-nuxt-app create-react-app webpack
NODE_PKGS += dockerfile-language-server-nodejs eslint eslint-cli eslint-config-vue netlify-cli
NODE_PKGS += eslint-plugin-react eslint-plugin-vue@next expo-cli firebase-tools fx heroku 
NODE_PKGS += indium intelephense javascript-typescript-langserver logo.svg @marp-team/marp-cli
NODE_PKGS += mermaid mermaid.cli ngrok now prettier parcel-bundler typescript-language-server
NODE_PKGS += @vue/cli vue-language-server vue-native-cli jshint fy neovim

PIP_PKGS := ansible ansible-lint beautifulsoup4 black cheat diagrams django djangorestframework time
PIP_PKGS += django-nested-admin django-ses faker gif-for-cli graph-cli httpie importmagic ipywidgets
PIP_PKGS += jupyter jupyterlab jupyterthemes litecli matplotlib neovim nose pandas pipenv poetry
PIP_PKGS += progressbar2 psycopg2-binary py-spy pydantic pydoc_utils r7insight_python redis zappa
PIP_PKGS += rtv scipy scrapy seaborn selenium speedtest-cli streamlink tldr trash-cli truffleHog
PIP_PKGS += virtualenv virtualenvwrapper requests_mock pre-commit chromedriver-binary pynvim neovim
