function fish_prompt
    set -l last_status $status

    set_color --bold blue
    echo -n $USER"@"(prompt_hostname)" "

    set_color --bold yellow
    echo -n (pwd | sed -e "s|^$HOME|~|")" "

    if test $last_status -eq 0
        set_color --bold green
    else
        set_color --bold red
    end
    echo "("$last_status")"

    set_color --bold yellow
    echo -n "> "

    set_color normal
end
