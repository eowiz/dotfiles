### Alias

## ls
if [[ $(command -v exa) ]]; then
  alias l='clear && ls'
  alias ls='exa --icons --git'
  alias ll='exa -aahl --icons --git'
  alias lt='exa -T -L 3 -a -I ".git" --git-ignore --icons'
  alias lta='exa -T -a -I ".git" --git-ignore --color=always --icons | less -r'
fi

alias grep='rg'
alias cat='bat'
alias find='fd'
