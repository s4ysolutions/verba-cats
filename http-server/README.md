# HTTP Server Authentication

The HTTP server now includes Bearer token authentication to secure the translation endpoint.

## Configuration

The server requires a secret token to be provided via command-line arguments:

```bash
# Start the server with authentication
sbt "httpServerJVM/run --secret your-secret-token"

# Optional: specify custom host and port
sbt "httpServerJVM/run --host 127.0.0.1 --port 9090 --secret your-secret-token"
```

### Command-Line Arguments

- `--secret` or `-s`: **Required**. The secret token for authentication
- `--host` or `-h`: Server host address (default: `0.0.0.0`)
- `--port` or `-p`: Server port (default: `8080`)

## Making Authenticated Requests

All requests to the `/translation` endpoint must include an `Authorization` header with a Bearer token matching the server's secret:

```bash
curl -X POST http://localhost:8080/translation \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer your-secret-token" \
  -d '{
    "text": "London is the capital",
    "from": "english",
    "to": "russian",
    "mode": "translate", # explain | auto
    "provider": "openai", # google
    "quality": "optimal", # fast | deep
  }'
```

## Response Codes

- **200 OK**: Request authenticated successfully and translation completed
- **403 Forbidden**: Authentication failed (missing or invalid token)
  - Missing header: Returns "Missing authentication header"
  - Invalid token: Returns "Invalid authentication token"

## Testing

A test script is provided to verify authentication:

```bash
./test-auth.sh
```

This script tests:
1. Valid authentication (should succeed)
2. Missing authentication (should return 403)
3. Invalid token (should return 403)

## Security Notes

- The secret token is passed as a command-line argument and should be treated securely
- Consider using environment variables or secure configuration management in production
- All authentication failures are logged with WARN level
- Use HTTPS in production to protect the token in transit

## Implementation Details

The authentication is implemented using:
- `AuthMiddleware`: A middleware that wraps the translation routes
- Checks for `Authorization: Bearer <token>` header format
- Uses Cats Effect IO for functional effects

### Example Server Start

```bash
# Development
sbt "httpServerJVM/run -s dev-secret-123"

# Production (example)
sbt "httpServerJVM/run --host 0.0.0.0 --port 8080 --secret $(cat /secure/path/to/secret)"
```

