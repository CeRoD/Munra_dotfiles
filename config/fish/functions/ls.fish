# Defined in - @ line 1
function ls --wraps='=lsd --group-dirs=first -F ' --wraps='lsd --group-dirs=first -F' --description 'alias ls lsd --group-dirs=first -F'
  lsd --group-dirs=first -F $argv;
end
