function fish_default_mode_prompt --description "Display the default mode for the prompt"
    # Do nothing if not in vi mode
    if test "$fish_key_bindings" = "fish_vi_key_bindings"
        or test "$fish_key_bindings" = "fish_hybrid_key_bindings"
        switch $fish_bind_mode
            case default
                set_color --bold ffffff
                echo '𝒏'
            case insert
                set_color --bold ffffff
                echo '𝒊'
            case replace_one
                set_color --bold ffffff
                echo '𝒓'
            case replace
                set_color --bold ffffff
                echo 'ʀ'
            case visual
                set_color --bold ffffff
                echo '𝒗'
        end
        set_color normal
        echo -n ' '
    end
end

