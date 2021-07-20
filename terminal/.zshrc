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
alias icat="kitty +kitten icat"
alias la="ls -a"
alias ll="ls -l"
alias lock="lockscreen"
alias ls="ls --color=auto"
alias patch="patch --backup"
alias sxiv="sxiv -a -o"
alias x="xclip"
alias xclip="xclip -selection clipboard"

alias -g D="&!"
alias -g E="| entr"
alias -g G="| grep"
alias -g H="| head"
alias -g L="| less"
alias -g N=">/dev/null"
alias -g N2="2>/dev/null"
alias -g NN=">&/dev/null"
alias -g T="| tail"
alias -g X="| xclip"

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
# alias -s sh=sh  # quite a few .sh files have a bash shebang...

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

uri() {
    if [[ -n "$1" ]]; then
        echo "file://$(realpath "$1")"
    else
        echo "file://$PWD"
    fi
}

glog-last() {
    local app="$1"
    local level=$(echo "${2:-info}" | awk '{print toupper($0)}')

    local log_dir="${GLOG_log_dir:-/tmp}"
    local log_prefix="${app}.$(hostname).$(whoami).log.${level}."
    local last_log="$(find ${log_dir} -name "${log_prefix}*" 2>/dev/null | sort | tail -n1)"

    if [[ ! -f "$last_log" ]]; then
        echo >&2 "${0}: No ${level} log files found for ${app}"
        return 1
    fi

    "${EDITOR:-vi}" "$last_log"
}

send-linux-patch() {
    local patch="$1"
    local args="${@:2}"
    local get_maintainer_script="./scripts/get_maintainer.pl"

    if [[ ! -f "$patch" ]]; then
        echo >&2 "${0}: '$patch' is not a file"
        return 1
    elif ! git config user.email >& /dev/null; then
        echo >&2 "${0}: user.email not configured for git"
        return 1
    elif [[ ! -f "$get_maintainer_script" ]]; then
        echo >&2 "${0}: '$get_maintainer_script' not found"
        return 1
    fi

    git send-email \
        --cc-cmd="$get_maintainer_script --norolestats $patch" \
        --cc "$(git config user.email)" \
        "$args" \
        "$patch"
}

rxclip() {
    local remote_file="$1"
    local local_copy="$(mktemp)"

    scp "$remote_file" "$local_copy" >/dev/null
    xclip -selection clipboard "$local_copy"
    rm "$local_copy"
}

rcat() {
    local remote_file="$1"
    local local_copy="$(mktemp)"

    scp "$remote_file" "$local_copy" >/dev/null
    cat "$local_copy"
    rm "$local_copy"
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
