sudo vim /etc/pacman.d/mirrorlist
sudo vim /etc/pacman.conf 
sudo pacman -Sy archlinuxcn-keyring neovim
sudo ln -sf /usr/bin/nvim /usr/bin/vim
sudo ln -sf /usr/bin/nvim /usr/bin/vi
sudo pacman -Syyu
mkdir old-arch
sudo mount /dev/sda2 old-arch/
cd old-arch/home/xc/
cp -r .dotfiles/ doc/ .m2/ .oh-my-zsh/ .ssh/ workspace/ get-pip.py fortivpn-config.sh github.token ~/
./.dotfiles/script/bootstrap 
sudo pacman -S zsh google-chrome git dmenu clash fcitx fcitx-configtool fcitx-im
fcitx-googlepinyin base-devel ethtool alacritty screenkey tmux neovim python
rust rustup go emacs i3 i3-gaps i3status i3blocks fzf the_silver_searcher
ripgrep mupdf zathura mpc mpd mpv feh nitrogen ttc-iosevka wqy-zenhei xclip wget
picom openfortivpn ruby rubygems nodejs npm yarn lua nasm maven gradle figlet
openssl openvpn fd bat jq htop tree expect curl jq lazygit virtualbox-host-modules-arch
archiso qemu asciidoc asciidoctor autoconf clang clash ctags flameshot gcc gdb
imagemagick inotify-tools libtool netease-cloud-music nerd-fonts-source-code-pro
ttc-iosevka ntfs-3g pcmanfm sddm dunst xdotool bc sysstat yad clang pyright rust-analyzer gopls

sudo npm i -g fanyi && sps festival

vim .xinitrc 
sudo chsh -s /usr/bin/zsh
chsh -s /usr/bin/zsh
cp -r old-arch/home/xc/picture/ .

startx
vim .dotfiles/i3status
i3status
sudo python ./get-pip.py
pip install pynvim
pip install neovim
sudo npm install -g neovim
pip3 install pynvim
gem install neovim
vim /etc/locale.conf
vim /etc/locale.gen
sudo vim /etc/locale.gen
sudo locale-gen
sudo vim /etc/locale.conf
sudo ln -sf /home/xc/workspace/java/zulu8-jdk/zulu8.54.0.21-ca-fx-jdk8.0.292-linux_x64/ /usr/lib/jvm/java-8-zulu-fx
java -version
echo $JAVA_HOME
mvn -v
