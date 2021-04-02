xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
setxkbmap -option caps:swapescape
setxkbmap -layout latam

xcompmgr &

~/.fehbg &

xmodmap ~/.Xmodmap &  #This is commented for working with
                        # sxhkd which is part of bspwm
                       # should be uncomment if bspwm is not used

xrdb -merge /home/munra/.Xresources

##xrandr --dpi 144

exec volnoti -t 2 &
##exec xfce4-session
exec i3
#exec spectrwm
#exec bspwm

