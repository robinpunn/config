# create pre/post snapshot on update
safe-update() {
    desc="update"
    echo "Checking for updates..."
    sudo dnf check-update

    read -rp "Proceed with upgrade and snapshot? (y/N): " confirm
    if [[ "$confirm" =~ ^[Yy]$ ]]; then
        pre_id=$(sudo snapper create \
		--type pre \
		--cleanup-algorithm number \
		--print-number \
		--description "$desc-pre")

        sudo dnf upgrade 

        sudo snapper create \
		--type post \
		--pre-number "$pre_id" \
		--cleanup-algorithm number \
		--description "$desc-post"
    else
        echo "Update cancelled."
    fi
}
