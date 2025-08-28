# create pre/post snapshot on install
safe-install() {
    if [[ -z "$1" ]]; then
        echo "Usage: safe-install <package>"
        return 1
    fi

    desc="install: $*"
    pre_id=$(sudo snapper create \
        --type pre \
        --cleanup-algorithm number \
        --print-number \
        --description "$desc-pre")

    sudo dnf install "$@"

    sudo snapper create \
        --type post \
        --pre-number "$pre_id" \
        --cleanup-algorithm number \
        --description "$desc-post"
}
