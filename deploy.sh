#!/bin/bash

# Simple deployment script for Shiny app
#
# IMPORTANT:
# 1. This script uses sshpass and relies on environment variables for credentials.
#    For enhanced security, using SSH key-based authentication is strongly recommended over passwords.
# 2. Ensure 'sshpass' is installed on the machine running this script (e.g., sudo apt-get install sshpass).
# 3. Ensure 'devtools' is installed in the R environment on the server for the user executing the R command.
# 4. The SSH user (defined by DEPLOY_USER or defaulting to 'root') must have permissions for:
#    - git pull in the APP_DIR
#    - Installing R packages (consider user-specific R libraries if not running as root)
#    - Restarting the shiny-server service via supervisorctl (may require sudo)
# 5. Adjust APP_DIR, SHINY_SERVICE_NAME, or the 'sudo supervisorctl restart...' command if your setup differs.
# 6. Using '-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null' with SSH bypasses host
#    key verification, which is a security risk (potential man-in-the-middle attacks).
#    Remove these options if you have manually accepted the host key or prefer higher security.

set -euo pipefail # Exit on error, treat unset vars as errors, propagate pipe errors

# --- Configuration ---
# Read sensitive data from environment variables
HOST_IP="${DEPLOY_IP:?Error: DEPLOY_IP environment variable is not set.}"
SSH_USER="${DEPLOY_USER:-root}" # Default to 'root' if DEPLOY_USER is not set
HOST_PASS="${DEPLOY_PASSWORD:?Error: DEPLOY_PASSWORD environment variable is not set.}"

# Application-specific paths and names (adjust if necessary)
APP_DIR="/srv/shiny-server/OPPP"
SHINY_SERVICE_NAME="shiny-server" # Supervisor service name for Shiny

echo "Starting deployment to ${SSH_USER}@${HOST_IP}..."
echo "Target application directory on server: ${APP_DIR}"

# --- Remote Commands ---
# Using a heredoc for better readability of the multi-line remote script.
# 'bash -s' on the remote end will execute these commands.
REMOTE_COMMANDS=$(cat <<EOF
set -e # Exit immediately if a command exits with a non-zero status on the remote server

echo
echo "===================================================="
echo "Connected to ${SSH_USER}@${HOST_IP}"
echo "===================================================="
echo

echo "--- Navigating to application directory: ${APP_DIR} ---"
cd "${APP_DIR}"
echo "Current directory: \$(pwd)"
echo

echo "--- Performing git pull ---"
git pull
echo

echo "--- Installing R package using devtools ---"
# Using options to potentially speed up install and make it quieter.
# Ensure R and devtools are correctly set up for \$SSH_USER on the server.
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install libgdal-dev libgeos-dev libproj-dev libtbb-dev libnetcdf-dev
R -e "options(Ncpus = \$(nproc)); remotes::install_github('rspatial/terra'); pak::pak()"
echo

echo "--- Restarting Shiny server (${SHINY_SERVICE_NAME}) via systemctl ---"
# This command typically requires sudo privileges.
sudo systemctl restart ${SHINY_SERVICE_NAME}
echo

echo "===================================================="
echo "Deployment to ${HOST_IP} completed successfully."
echo "===================================================="
EOF
)

# --- Execute Remote Commands ---
echo "Attempting to execute remote commands. This may take a few minutes..."
# The -o SSH options are for convenience in automated environments but reduce security.
# Consider removing them if the host key is already known and stable.
if sshpass -p "${HOST_PASS}" ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null "${SSH_USER}@${HOST_IP}" "bash -s" <<< "${REMOTE_COMMANDS}"; then
  echo
  echo "-----------------------------------------------------"
  echo "Local script: Deployment process finished successfully."
  echo "-----------------------------------------------------"
else
  echo
  echo "-----------------------------------------------------"
  echo "Local script: Error during deployment. Review output from server."
  echo "-----------------------------------------------------"
  exit 1
fi

exit 0 
