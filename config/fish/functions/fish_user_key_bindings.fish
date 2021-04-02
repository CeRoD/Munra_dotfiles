function fish_user_key_bindings
    for mode in insert default visual
        bind -M $mode \cf forward-char
        bind -M $mode \cp history-search-backward
        bind -M $mode \cn history-search-forward
    end
	fzf_key_bindings
end

