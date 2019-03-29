function fish_prompt
    set_color --bold green
    echo -n (printf "[%s@%s " $USER (prompt_hostname))

    set_color --bold $fish_color_cwd
    echo -n (basename (prompt_pwd))

    set_color --bold green
    echo -n "]\$ "

    set_color normal
end
