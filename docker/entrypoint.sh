#!/bin/bash
# Start Xvfb for headless display (VS Code Electron needs it)
Xvfb :99 -screen 0 1920x1080x24 -nolisten tcp &
sleep 1

# Start dbus (VS Code needs it)
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
  eval $(dbus-launch --sh-syntax)
fi

# Execute the provided command
exec "$@"
