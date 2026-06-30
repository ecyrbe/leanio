# LeanPlay

A YouTube-like video streaming website built with LeanIO.

## Features

- **Video library** — lists all `.mp4` files from `static/media/`
- **Streaming with seeking** — uses HTTP `Range` headers for efficient scrubbing
- **YouTube-style UI** — dark theme, search, sidebar, video player page
- **Comments** — post and view comments per video via the API

## Quick start

```bash
lake build leanplay
lake exe leanplay
```

Open <http://127.0.0.1:8080> in your browser.

## Adding videos

Drop `.mp4` files into `Examples/LeanPlay/static/media/` and refresh the page.

## API

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/v1/videos` | List all videos with name, size, and URL |
| `GET` | `/api/v1/videos/{name}/comments` | Get comments for a video |
| `POST` | `/api/v1/videos/{name}/comments` | Add a comment (`{"author":"...","text":"..."}`) |

## Structure

```
Examples/LeanPlay/
  Main.lean          — server entry point
  static/
    index.html        — video browser UI
    style.css         — styles
    script.js         — client-side logic
    media/            — video files
```
