#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

sudo cp "$SCRIPT_DIR/systemd/system/battery-charge-threshold.service" /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now battery-charge-threshold.service

echo "Battery threshold rule applied"
