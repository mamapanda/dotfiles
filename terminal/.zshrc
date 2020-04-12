PROMPT="%B%F{blue}%n@%M %F{yellow}%~ %(?.%F{green}.%F{red})(%?)
%F{yellow}> %f%b"

setopt auto_cd
setopt cd_silent
setopt pushd_silent

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks

zstyle ':completion:*' menu select

zmodload -i zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete

# https://stackoverflow.com/questions/8657648
alias debug="gdb -ex='set confirm on' -ex=run -ex=quit --args"
alias grep="grep --color=auto -i"
alias la="ls -a"
alias ll="ls -l"
alias ls="ls --color=auto"
alias sxiv="sxiv -a -o"

alias -g D="&!"
alias -g G="| grep"
alias -g L="| less"

alias -s bash=bash
alias -s go="go run"
alias -s html="$BROWSER"
alias -s lisp="sbcl --script"
alias -s pdf=zathura
alias -s py=python
alias -s R=Rscript
alias -s sh=sh

take() {
    mkdir -p "$1" && cd "$1"
}

if [[ ! -f ~/.zgen/zgen.zsh ]]; then
    git clone https://github.com/tarjoilija/zgen.git ~/.zgen
fi

source ~/.zgen/zgen.zsh

if ! zgen saved; then
    zgen load zdharma/fast-syntax-highlighting
    zgen load junegunn/fzf shell
    zgen load zsh-users/zsh-autosuggestions
    zgen load zsh-users/zsh-completions src
    zgen load MichaelAquilina/zsh-you-should-use
    zgen load agkozak/zsh-z

    zgen save
fi

FAST_HIGHLIGHT[chroma-awk]=
FAST_HIGHLIGHT[chroma-bash]=
FAST_HIGHLIGHT[chroma-perl]=
FAST_HIGHLIGHT[chroma-sh]=

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=blue"
