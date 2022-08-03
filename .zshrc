typeset -U path PATH
path=(
  # homebrew
  /opt/homebrew/bin(N-/)
  /opt/homebrew/sbin(N-/)
  # unix
  /usr/bin
  /usr/sbin
  /bin
  /sbin
  /usr/local/bin(N-/)
  /usr/local/sbin(N-/)
  # macOS
  /Library/Apple/usr/bin(N-/)
  # rbenv
  $HOME/.rbenv/bin(N-/)
  $HOME/.rbenv/shims(N-/)
)

source $HOME/.cargo/env

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

### starship
eval "$(starship init zsh)"

### Load basic config
source "$HOME/.zsh/basic.zsh"

### Load key bindings
source "$HOME/.zsh/keybindings.zsh"

### Load plugins
source "$HOME/.zsh/plugins.zsh"

### Load alias
source "$HOME/.zsh/alias.zsh"

export FZF_TMUX=1
export FZF_TMUX_OPTS="-p 80%"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
