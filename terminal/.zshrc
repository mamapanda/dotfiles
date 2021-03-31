PROMPT="%B%F{blue}%n@%M %F{yellow}%~ %(?.%F{green}.%F{red})(%?)
%F{yellow}> %f%b"

setopt auto_cd
setopt auto_pushd
setopt cd_silent
setopt pushd_ignore_dups
setopt pushd_silent

setopt interactive_comments

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt share_history

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
alias xclip="xclip -selection clipboard"

alias -g D="&!"
alias -g G="| grep"
alias -g L="| less"
alias -g N="> /dev/null"
alias -g N2="2> /dev/null"
alias -g NN=">& /dev/null"

compile-and-run() {
    local compiler=$1
    local source=$2
    local executable=$(mktemp)

    "$compiler" "$source" -o "$executable" && "$executable"

    if [[ -f $executable ]]; then
        rm "$executable"
    fi
}

alias -s bash=bash
alias -s c="compile-and-run clang"
alias -s cc="compile-and-run clang++"
alias -s cpp="compile-and-run clang++"
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

# "eval $(thefuck --alias)" causes a noticeable increase in startup time.  This
# is basically a hack to defer evaluation to when "fuck" is actually used.
fuck() {
    eval $(thefuck --alias)
    fuck
}

# bear doesn't work that well with some projects
gen-ccls() {
    local header_dirs="$(fd -e h -e hh -e hpp | xargs -n1 dirname | sort -u)"

    cat <<EOF
%clang
-Wall
-Wextra
-Werror
$(echo "$header_dirs" | awk '{ print "-I" $0 }')
EOF
}

diff-sorted() {
    local file_1="$1"
    local file_2="$2"

    diff <(sort "$file_1") <(sort "$file_2")
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

# YSU_HARDCORE=1

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=blue"

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi
