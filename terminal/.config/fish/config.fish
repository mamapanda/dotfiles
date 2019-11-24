# https://github.com/fish-shell/fish-shell/issues/4542
for v in (set --show | string replace -rf '^\$([^:[]+).*: set in universal.*' '$1')
    set -e $v
end

# disable annoying startup text
set -g __fish_init_2_3_0
set -g fish_greeting

# syntax colors
set -g fish_color_command magenta
set -g fish_color_param white
set -g fish_color_valid_path --underline
set -g fish_color_quote green  # strings
set -g fish_color_escape cyan
set -g fish_color_operator yellow
set -g fish_color_end yellow  # process separators (e.g. | and &)
set -g fish_color_redirection yellow
set -g fish_color_cancel red  # ^C
set -g fish_color_comment white
set -g fish_color_error red

# completion colors
set -g fish_color_autosuggestion blue  # untyped part of suggestion
set -g fish_color_search_match bryellow --background=brblack  # pager selection
set -g fish_pager_color_prefix yellow  # typed parts of completions
set -g fish_pager_color_completion normal  # untyped parts of completions
set -g fish_pager_color_description yellow
set -g fish_pager_color_progress yellow

# other colors
set -g fish_color_history_current yellow  # dirh current directory
set -g fish_color_selection white --bold --background=black  # vi selection

# plugin variables
set -g FZF_LEGACY_KEYBINDINGS 0

# load plugins
fundle plugin "edc/bass"
fundle plugin "jethrokuan/fzf"
fundle plugin "jethrokuan/z"

fundle init
