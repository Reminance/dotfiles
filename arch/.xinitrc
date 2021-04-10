# screen setup
xrandr --output DP-0 --left-of HDMI-0 --auto
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
fcitx&
exec dwm
