function fish_mode_prompt
    if [ $fish_key_bindings = "fish_vi_key_bindings" ]
        switch $fish_bind_mode
            case default
                set_color --bold green
                echo '[N]'
            case insert
                set_color --bold red
                echo '[I]'
            case visual
                set_color --bold white
                echo '[V]'
        end
        set_color normal
        echo -n ' '
    end
end
