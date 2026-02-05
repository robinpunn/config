#!/bin/bash
# Install udev rules for battery threshold
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
sudo ln -sf "$SCRIPT_DIR/udev/rules.d/99-battery-threshold.rules" /etc/udev/rules.d/
sudo udevadm control --reload
sudo udevadm trigger
echo "Battery threshold udev rule installed"
