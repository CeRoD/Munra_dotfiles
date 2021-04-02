#set fish_function_path $fish_function_path "/home/munra/.config/powerline/fish"
#source /home/munra/.config/powerline/fish/powerline-setup.fish
#powerline-setup

set fish_greeting

fish_vi_key_bindings

#list of aliases
alias less='less -r -M'
alias SearchPacman "pacman -Slq | fzf --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -Syu"
#alias InstallFZFPacman "sudo pacman -Slq | fzf --multi --preview 'pacman -Si {1}' | xargs -ro  pacman -S"
alias fd "fd -H"
alias cdf "fzf-cd-widget"
alias rg "rg -i"

#alias ls='ls --color=auto --group-directories-first'
#alias lsa='ls -a --color=auto --group-directories-first'

### A function was created with the
### alias -s ls 'command'

    ##alias ls ='lsd --group-dirs=first -F '
    ##alias lsa = 'lsd --group-dirs=first -F -A '
    ##alias lsl = 'lsd --group-dirs=first -F -A -l '


bind \cp history-search-backward
bind \cn history-search-forward

## Load the dircolors file
#eval (dircolors -c .dir_colors)
eval (dircolors -c /home/munra/.dir_colors/Dracula.dircolors)

#FZF Configurations
export FZF_DEFAULT_OPTS='
  --color fg:#ebdbb2,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
  --color info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54
'

#export FZF_DEFAULT_COMMAND="fd --type f --color=always --hidden --follow --exclude .git . $HOME"
#export FZF_DEFAULT_COMMAND="fd --type f --color=always --hidden --follow --exclude .git . / "
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_DEFAULT_OPTS="--ansi"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#export FZF_ALT_C_COMMAND="fd -t d --color=always --hidden --exclude .git . $HOME "
export FZF_ALT_C_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'


#BAT configurations
export BAT_THEME="base16"

## color scheme for man colors.
# Solarized Dark & Green highlight
set -g man_blink -o red
set -g man_bold -o green
set -g man_standout -b black 93a1a1
set -g man_underline -u 93a1a1

## Prompt charge...
starship init fish | source
