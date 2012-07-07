# TODO Decide on how topical dotfiles really need to be
function manpdf() { man -t $@ | open -f -a Preview; }
