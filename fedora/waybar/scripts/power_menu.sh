#!/bin/bash

options="Poweroff\nShutdown\nReboot\nSuspend\nHibernate"

choice=$(echo -e "$options" | rofi -dmenu -p "Power Menu:")

case "$choice" in
    "Poweroff") 
        systemctl poweroff
        ;;	
    "Shutdown") 
        shutdown 
        ;;
    "Reboot")   
	systemctl reboot
        ;;	
    "Suspend")   
	systemctl suspend
        ;;
    "Hibernate")   
	systemctl hibernate
        ;;
    *)
	# do nothing on cancel or invalid
	;;
esac

