function cat
    if command -sq bat
        bat $argv
    else
        command cat $argv
    end
end
