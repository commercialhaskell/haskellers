#!/usr/bin/env bash

set -euo pipefail

BASE_DIR="./config/db"

amber write-file --key  HASKELLERS_AWS --dest "$BASE_DIR/aws"
amber write-file --key  HASKELLERS_FACEBOOK --dest "$BASE_DIR/facebook.yaml"
amber write-file --key  HASKELLERS_GOOGLE --dest "$BASE_DIR/google-email.yaml"
amber write-file --key  HASKELLERS_POSTGRES --dest "$BASE_DIR/postgresql.yml"
amber write-file --key  HASKELLERS_CLIENT_SESSION_BASE64 --dest "$BASE_DIR/client-session-key.base64"

base64 -d "$BASE_DIR/client-session-key.base64" > "$BASE_DIR/client-session-key.aes"

/usr/local/bin/haskellers production
