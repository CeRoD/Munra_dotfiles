# Defined in - @ line 1
function lsa --wraps='lsd --group-dirs=first -F -A' --description 'alias lsa lsd --group-dirs=first -F -A'
  lsd --group-dirs=first -F -A $argv;
end
