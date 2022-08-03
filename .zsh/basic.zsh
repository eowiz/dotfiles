## 履歴保存管理
HISTSIZE=100000
SAVEHIST=1000000

## 他のzshと履歴を共有
setopt inc_append_history
setopt share_history

## 重複排除
setopt hist_ignore_all_dups
setopt hist_ignore_dups

setopt hist_no_store
setopt hist_reduce_blanks

## パスを直接入力してもcdする
setopt AUTO_CD

## 環境変数を補完
setopt AUTO_PARAM_KEYS

autoload -Uz compinit
compinit
zstyle ':completion:*:default' menu select=2

autoload colors
colors

export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

setopt globdots

