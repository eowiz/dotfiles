## emacs
if [[ $(command -v emacs) ]]; then
  alias emacs='emacsclient -nw -c -a ""'
  alias killemacs='emacsclient -e "(kill-emacs)"'
  alias e='emacs'
  export EDITOR='emacsclient -nw -c -a ""'
fi

