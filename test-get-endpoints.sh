#!/bin/bash

# Test script for all GET endpoints

SECRET="your-secret-token"
HOST="http://localhost:8080"

echo "Testing GET /modes endpoint..."
curl -H "Authorization: Bearer $SECRET" "$HOST/modes"
echo -e "\n"

echo "Testing GET /providers endpoint..."
curl -H "Authorization: Bearer $SECRET" "$HOST/providers"
echo -e "\n"

echo "Testing GET /qualities endpoint..."
curl -H "Authorization: Bearer $SECRET" "$HOST/qualities"
echo -e "\n"

