
# Sample .bashrc for SUSE Linux
# Copyright (c) SUSE Software Solutions Germany GmbH

# There are 3 different types of shells in bash: the login shell, normal shell
# and interactive shell. Login shells read ~/.profile and interactive shells
# read ~/.bashrc; in our setup, /etc/profile sources ~/.bashrc - thus all
# settings made here will also take effect in a login shell.
#
# NOTE: It is recommended to make language settings in ~/.profile rather than
# here, since multilingual X sessions would not work properly if LANG is over-
# ridden in every subshell.

test -s ~/.alias && . ~/.alias || true

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/chaos/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/chaos/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/chaos/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/chaos/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


[ -f ~/.fzf.bash ] && source ~/.fzf.bash



export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
export NVM_NODEJS_ORG_MIRROR=http://npm.taobao.org/mirrors/node

# golang 1.15
export GOROOT=/opt/go
export GOPATH=/home/chaos/me/goProjects
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

# 启用 Go Modules 功能
# export GO111MODULE=on
export GO111MODULE=auto
# 配置 GOPROXY 环境变量
export GOPROXY=https://goproxy.io
# export GOPROXY=https://mirrors.aliyun.com/goproxy/

# Rust
export RUSTUP_DIST_SERVER=https://mirrors.sjtug.sjtu.edu.cn/rust-static
export RUSTUP_UPDATE_ROOT=https://mirrors.sjtug.sjtu.edu.cn/rust-static/rustup

export PATH=$PATH:$HOME/.cargo/bin

# logo-ls
alias ls='logo-ls'
alias la='logo-ls -A'
alias ll='logo-ls -al'
# equivalents with Git Status on by Default
alias lsg='logo-ls -D'
alias lag='logo-ls -AD'
alias llg='logo-ls -alD'

export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
# Setting fd as the default source for fzf
export FZF_DEFAULT_COMMAND='fd --type f'
# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

