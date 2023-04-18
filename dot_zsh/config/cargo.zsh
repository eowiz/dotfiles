# Rust
[ -f $HOME/.cargo/env ] && source $HOME/.cargo/env

# rust-analyzer
which rustup >/dev/null 2>&1 &&
    [ -f $(rustup which --toolchain stable rust-analyzer) ] &&
    export PATH=$PATH:$(dirname $(rustup which --toolchain stable rust-analyzer))
