echo "Updating all submodules..."
git submodule -q foreach 'git checkout -q master && echo "Updating `pwd`" &&  git pull -q --rebase origin master' > /dev/null
