# git
alias gl='git pull'
alias glr='git pull --rebase'
alias gp='git push'
alias gpa='git push-all' # see [alias] in ~/.gitconfig
alias ga='git add'
alias gd='git diff --word-diff'
alias gc='git commit -v'
alias gca='git commit -av'
alias gco='git checkout'
alias gb='git branch -v'
alias gs='git status -sb'

alias glog='git log --stat'
alias glg="git log --decorate --oneline"
alias gll="git log --decorate --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

alias gmt='git mergetool'
alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"
alias ignore_empty='find . \( -type d -empty \) -and \( -not -regex ./\.git.* \) -exec touch {}/.gitignore \;'

if [[ -d "/usr/local/git" ]]; then
  PATH="/usr/local/git/bin:$PATH"
  MANPATH="/usr/local/git/man:$MANPATH"
fi
