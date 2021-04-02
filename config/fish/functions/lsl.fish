# Defined in - @ line 1
function lsl --wraps='lsd --group-dirs=first -F -A -l' --description 'alias lsl lsd --group-dirs=first -F -A -l'
  lsd --group-dirs=first -F -A -l $argv;
end
