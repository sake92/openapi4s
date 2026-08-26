#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "$0")/.." && pwd)"
curl -sSL -o "$DIR/openapi4s/src/test/resources/github.json" \
  https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json
curl -sSL -o "$DIR/openapi4s/src/test/resources/jira-cloud-v3.json" \
  https://developer.atlassian.com/cloud/jira/platform/swagger-v3.v3.json
