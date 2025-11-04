#!/bin/bash

# Test script for WSSE authentication

SECRET="your-secret-token"
HOST="http://localhost:8080"

# Generate WSSE credentials
USERNAME="verba-user"
NONCE=$(openssl rand -base64 16)
CREATED=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Calculate PasswordDigest: Base64(SHA-256(Nonce + Created + Secret))
PASSWORD_DIGEST=$(echo -n "${NONCE}${CREATED}${SECRET}" | openssl dgst -sha256 -binary | base64)

# Build WSSE token
WSSE_TOKEN="UsernameToken Username=\"${USERNAME}\", PasswordDigest=\"${PASSWORD_DIGEST}\", Nonce=\"${NONCE}\", Created=\"${CREATED}\""

echo "Testing WSSE Authentication..."
echo "WSSE Token: $WSSE_TOKEN"
echo ""

# Test translation endpoint
echo "Testing POST /translation endpoint..."
curl -v -X POST \
  -H "Authorization: ${WSSE_TOKEN}" \
  -H "Content-Type: application/json" \
  -d '{
    "from": "english",
    "to": "russian",
    "mode": "explain",
    "provider": "gemini",
    "phrase": "London is the capital"
  }' \
  "$HOST/translation"
echo -e "\n"

# Test GET endpoints
echo "Testing GET /modes endpoint..."
curl -H "Authorization: ${WSSE_TOKEN}" "$HOST/modes"
echo -e "\n"

echo "Testing GET /providers endpoint..."
curl -H "Authorization: ${WSSE_TOKEN}" "$HOST/providers"
echo -e "\n"

echo "Testing GET /qualities endpoint..."
curl -H "Authorization: ${WSSE_TOKEN}" "$HOST/qualities"
echo -e "\n"

