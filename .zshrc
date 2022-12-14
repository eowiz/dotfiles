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

### Load basic config
source "$HOME/.zsh/basic.zsh"

### Load key bindings
source "$HOME/.zsh/keybindings.zsh"

### Load plugins
source "$HOME/.zsh/plugins.zsh"

### Load alias
source "$HOME/.zsh/alias.zsh"

bin=(
    "starship"
    "fzf"
    "cargo"
    "opam"
    "fnm"
    "ghcup"
    "sdkman"
    "volta"
    "rbenv"
)

for filename in $bin; do
    source "$HOME/.zsh/config/$filename.zsh"
done

