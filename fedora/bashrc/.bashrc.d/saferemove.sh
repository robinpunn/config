# create pre/post snapshot on remove
safe-remove() {
    if [[ -z "$1" ]]; then
        echo "Usage: safe-remove <package>"
        return 1
    fi

    desc="remove: $*"
    pre_id=$(sudo snapper create \
        --type pre \
        --cleanup-algorithm number \
        --print-number \
        --description "$desc-pre")

    sudo dnf remove "$@"

    sudo snapper create \
        --type post \
        --pre-number "$pre_id" \
        --cleanup-algorithm number \
        --description "$desc-post"
}
