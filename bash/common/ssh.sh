# Start the SSH keychain. The first new terminal will request the passphrase.
$USER_LIBRARY/bin/keychain --quiet ~/.ssh/id_rsa
. ~/.ssh-agent-${HOSTNAME}
