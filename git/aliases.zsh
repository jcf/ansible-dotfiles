#
# Defines Git aliases.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   James Conroy-Finn <james@logi.cl>
#

alias gl='git pull'
compdef _git gl=git-pull

alias glr='git pull --rebase'
compdef _git glr=git-pull

alias gp='git push'
compdef _git gp=git-push

alias gpa='git push-all' # see [alias] in ~/.gitconfig
compdef _git gpa=git-push

alias ga='git add'
compdef _git ga=git-add

alias d='git diff'
compdef _git d=git-diff

alias gc='git commit -v -S'
compdef _git gc=git-commit

alias gca='git commit -av -S'
compdef _git gca=git-commit

alias gco='git checkout'
compdef _git gco=git-checkout

alias gb='git branch'
compdef _git gb=git-branch

alias gbv='git branch -v'
compdef _git gbv=git-branch

alias s='git status -sb'
compdef _git s=git-status

alias gg='git grep'
compdef _git gg=git-grep

alias glog='git log --stat --date=relative'
compdef _git glog=git-log

alias glg="git l"
compdef _git glg=git-log

alias gmt='git mergetool'

alias grc="git rebase --continue"
compdef _git grc=git-rebase

alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"

alias ignore_empty='find . \( -type d -empty \) -and \( -not -regex ./\.git.* \) -exec touch {}/.gitignore \;'
