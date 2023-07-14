### Alias

## ls
if [[ $(command -v exa) ]]; then
  alias l='clear && ls'
  alias ls='exa --icons --git'
  alias ll='exa -aahl --icons --git'
  alias lt='exa -T -L 3 -a -I ".git" --git-ignore --icons'
  alias lta='exa -T -a -I ".git" --git-ignore --color=always --icons | less -r'
fi

## grep
if [[ $(command -v rg) ]]; then
  alias grep='rg'
fi

## cat
if [[ $(command -v bat) ]]; then
  alias cat='bat'
fi

## vim
if [[ $(command -v nvim) ]]; then
  alias vim='nvim'
fi
