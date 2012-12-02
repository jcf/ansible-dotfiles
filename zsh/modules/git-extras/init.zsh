if (( ! $+commands['git-extras'] )); then
  return 1
fi

# Add git-extras to git completion
zstyle ':completion:*:*:git:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}
