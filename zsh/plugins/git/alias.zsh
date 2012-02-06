#
# Defines Git aliases.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
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

alias gd='git diff'
compdef _git gd=git-diff

alias gc='git commit -v'
compdef _git gc=git-commit

alias gca='git commit -av'
compdef _git gca=git-commit

alias gco='git checkout'
compdef _git gco=git-checkout

alias gb='git branch'
compdef _git gb=git-branch

alias gbv='git branch -v'
compdef _git gbv=git-branch

alias gs='git status -sb'
compdef _git gs=git-status

alias glog='git log --stat'
compdef _git glog=git-log

alias glg="git log --decorate --oneline"
compdef _git glg=git-log

alias gll="git log --decorate --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
compdef _git gll=git-log

alias gmt='git mergetool'

alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"

alias ignore_empty='find . \( -type d -empty \) -and \( -not -regex ./\.git.* \) -exec touch {}/.gitignore \;'
