let allVideos = [];

async function init() {
  try {
    const res = await fetch('/api/v1/videos');
    allVideos = await res.json();
    renderGrid(allVideos);
  } catch (e) {
    document.getElementById('app').innerHTML = '<div class="empty">Failed to load videos.</div>';
  }
}

function renderGrid(videos) {
  const app = document.getElementById('app');
  if (!videos.length) {
    app.innerHTML = '<div class="empty"><svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5"><rect x="3" y="3" width="18" height="18" rx="2"/><polygon points="9,7 9,17 17,12"/></svg><br>No videos yet.<br>Add .mp4 files to static/media/</div>';
    return;
  }
  let html = '<h2>Library</h2><div class="video-grid">';
  for (const v of videos) {
    html +=
      '<div class="video-card" onclick="openPlayer(\'' + esc(v.url) + '\',\'' + esc(v.name) + '\')">' +
      '<div class="thumbnail"><div class="play-icon"><svg viewBox="0 0 24 24"><polygon points="8,5 19,12 8,19"/></svg></div></div>' +
      '<div class="video-meta">' +
      '<div class="avatar-sm">L</div>' +
      '<div class="video-info">' +
      '<div class="video-title">' + esc(v.name) + '</div>' +
      '<div class="video-chan">LeanPlay</div>' +
      '<div class="video-stats">' + sizeStr(v.size) + '</div>' +
      '</div>' +
      '</div>' +
      '</div>';
  }
  html += '</div>';
  app.innerHTML = html;
}

function openPlayer(url, name) {
  const app = document.getElementById('app');
  app.innerHTML =
    '<div class="player-page active">' +
    '<div class="back-btn" onclick="goHome()"><svg viewBox="0 0 24 24" stroke="currentColor" stroke-width="2" fill="none"><line x1="19" y1="12" x2="5" y2="12"/><polyline points="12 19 5 12 12 5"/></svg> Back</div>' +
    '<div class="player-wrapper">' +
    '<div class="player-main">' +
    '<video src="' + esc(url) + '" controls autoplay></video>' +
    '<div class="video-details">' +
    '<h1>' + esc(name) + '</h1>' +
    '<div class="video-actions">' +
    '<button onclick="this.classList.toggle(\'liked\')">&#x1F44D; Like</button>' +
    '<button>&#x1F44E;</button>' +
    '<button>&#x2197; Share</button>' +
    '<button>&#x1F4BE; Save</button>' +
    '</div>' +
    '<div class="video-desc">' + esc(name) + '<br><br>Streamed with LeanPlay — a Lean 4 web server with HTTP Range support. Built with LeanIO.</div>' +
    '<div class="comments" id="commentsArea">' +
    '<h2>&#x1F4AC; Comments</h2>' +
    '<div style="display:flex;gap:8px;margin-bottom:20px" id="commentForm">' +
    '<input placeholder="Your name" id="cmtAuthor" style="background:var(--bg2);border:1px solid #333;color:var(--text);padding:8px 12px;border-radius:8px;font-size:14px;width:120px">' +
    '<input placeholder="Add a comment..." id="cmtText" style="flex:1;background:var(--bg2);border:1px solid #333;color:var(--text);padding:8px 12px;border-radius:8px;font-size:14px">' +
    '<button onclick="postComment(\'' + esc(name) + '\')" style="background:var(--accent);color:#000;border:none;padding:8px 16px;border-radius:8px;cursor:pointer;font-weight:500;font-size:14px">Post</button>' +
    '</div>' +
    '<div id="commentsList">Loading comments...</div>' +
    '</div>' +
    '</div>' +
    '</div>' +
    '<div class="side-videos" id="sideVideos"></div>' +
    '</div>' +
    '</div>';
  renderSidebar(url);
  loadComments(name);
}

function renderSidebar(currentUrl) {
  const others = allVideos.filter(v => !currentUrl.endsWith(v.url));
  let html = '';
  for (const v of others.slice(0, 10)) {
    html +=
      '<div class="mini-card" onclick="openPlayer(\'' + esc(v.url) + '\',\'' + esc(v.name) + '\')">' +
      '<div class="mini-thumb">&#9654;</div>' +
      '<div class="mini-info">' +
      '<div class="mini-title">' + esc(v.name) + '</div>' +
      '<div class="mini-meta">LeanPlay · ' + sizeStr(v.size) + '</div>' +
      '</div>' +
      '</div>';
  }
  document.getElementById('sideVideos').innerHTML = html || '<div style="color:var(--text2);padding:20px">No more videos</div>';
}

function goHome() { renderGrid(allVideos); }

function filter() {
  const q = document.getElementById('searchInput').value.toLowerCase();
  renderGrid(q ? allVideos.filter(v => v.name.toLowerCase().includes(q)) : allVideos);
}

async function loadComments(videoName) {
  try {
    const res = await fetch('/api/v1/videos/' + encodeURIComponent(videoName) + '/comments');
    const comments = await res.json();
    const list = document.getElementById('commentsList');
    if (!comments.length) { list.innerHTML = '<div style="color:var(--text2);padding:8px 0">No comments yet.</div>'; return; }
    list.innerHTML = comments.map(c =>
      '<div class="comment"><div class="avatar-sm">' + esc(c.author[0] || '?') + '</div><div class="comment-body"><div class="author">@' + esc(c.author) + '</div><div class="text">' + esc(c.text) + '</div></div></div>'
    ).join('');
  } catch (e) { document.getElementById('commentsList').textContent = 'Failed to load comments.'; }
}

async function postComment(videoName) {
  const author = document.getElementById('cmtAuthor').value.trim();
  const text = document.getElementById('cmtText').value.trim();
  if (!author || !text) return;
  try {
    await fetch('/api/v1/videos/' + encodeURIComponent(videoName) + '/comments', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ author, text })
    });
    document.getElementById('cmtAuthor').value = '';
    document.getElementById('cmtText').value = '';
    loadComments(videoName);
  } catch (e) { alert('Failed to post comment.'); }
}

function sizeStr(s) {
  if (!s) return '';
  const b = Number(s);
  if (b < 1000) return b + ' B';
  if (b < 1e6) return (b / 1e3).toFixed(1) + ' KB';
  if (b < 1e9) return (b / 1e6).toFixed(1) + ' MB';
  return (b / 1e9).toFixed(1) + ' GB';
}

function esc(s) { return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#39;'); }

init();
