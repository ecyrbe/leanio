# WhoAmI

Minimal demo of the `RemoteAddr` extractor — shows your public IP address
as seen by the server.

## Quick start

```bash
lake build whoami
lake exe whoami
```

Open <http://127.0.0.1:8080> in your browser.

## API

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/whoami` | Returns the client's IP address as plain text |

## Structure

```
Examples/WhoAmI/
  WhoAmI.lean        — server entry point
  static/
    index.html        — single-page frontend
```
