#
# Defines Git aliases.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Settings
#
#

# Log
zstyle -s ':prezto:module:git:log:medium' format '_git_log_medium_format' \
  || _git_log_medium_format='%C(bold)Commit:%C(reset) %C(green)%H%C(red)%d%n%C(bold)Author:%C(reset) %C(cyan)%an <%ae>%n%C(bold)Date:%C(reset)   %C(blue)%ai (%ar)%C(reset)%n%+B'
zstyle -s ':prezto:module:git:log:oneline' format '_git_log_oneline_format' \
  || _git_log_oneline_format='%C(green)%h%C(reset) %s%C(red)%d%C(reset)%n'
zstyle -s ':prezto:module:git:log:brief' format '_git_log_brief_format' \
  || _git_log_brief_format='%C(green)%h%C(reset) %s%n%C(blue)(%ar by %an)%C(red)%d%C(reset)%n'

# Status
zstyle -s ':prezto:module:git:status:ignore' submodules '_git_status_ignore_submodules' \
  || _git_status_ignore_submodules='none'

#
# Aliases
#

# Git
alias g='git'

alias gl='git pull'
alias glr='git pull --rebase'

alias gp='git push'
alias gpa='git push-all' # see [alias] in ~/.gitconfig

alias ga='git add'
alias gap='git add -p'

alias d='git diff'

alias gc='git commit -vS'
alias gca='git commit -avS'

alias gco='git checkout'

alias gb='git branch'
alias gbv='git branch -v'
alias gbm='git branch --merged'
alias gbr='git branch --remote'
alias gbrm='git branch --remote --merged'
alias gbs='git branches'

alias s='git status -sb'

alias gg='git grep'

alias glg="git l"
alias glog='git log --stat --date=relative'

alias gmt='git mergetool'

alias gr="git rebase"
alias grc="git rebase --continue"
