function git
    if command -sq hub
        hub $argv
    else
        command git $argv
    end
end
