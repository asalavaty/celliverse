import { useCallback, useEffect, useState } from "react";
import { api, ResultsManifest, ResultArtifact } from "../api/client";

// The Results tab is a durable, downloadable view of everything the agent has
// produced this session. It is driven entirely by the server manifest
// (GET /api/artifacts), NOT by in-memory chat state, so it survives reloads and
// reflects every server-side object — each serialized to its own portable .rds.
//
// Sections (in manifest order): Figures, Objects (RDS), Tables (CSV), Text
// (TXT), and a catch-all Other. A single "Download all (zip)" bundles the lot.

function fmtBytes(n?: number): string {
  if (n == null || !isFinite(n) || n < 0) return "";
  if (n < 1024) return `${n} B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)} KB`;
  return `${(n / (1024 * 1024)).toFixed(1)} MB`;
}

// Round LIV: objects are no longer serialized at the end of every turn — the
// server indexes them and writes the bytes when a download asks for it. A row
// flagged `pending` therefore takes a few seconds on first click, and the whole
// point of this file's changes is that the user is TOLD that rather than left
// looking at a button that appears to do nothing.
//
// Fetching to a blob instead of using a plain <a download> is what makes the
// waiting state possible at all: an anchor gives no event to hang "Preparing…"
// off. Buffering is acceptable here because artifacts stay gzip-compressed
// (deliberately — see Round LIV in CHANGES.md), so even a 193 MB object is a
// ~12 MB file. On any failure we fall back to letting the browser handle the
// URL directly, so a blob problem can never make a file undownloadable.
async function downloadViaFetch(url: string, filename: string): Promise<void> {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Download failed (${res.status})`);
  const blob = await res.blob();
  const href = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = href;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  URL.revokeObjectURL(href);
}

// One download control that knows whether its file exists yet.
function DownloadAction({
  art, label, onPrepared,
}: { art: ResultArtifact; label: string; onPrepared?: () => void }) {
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState("");
  const url = art.url || "";
  const filename = art.filename || "download";

  // Already on disk: a plain link, exactly as before this round. No fetch, no
  // buffering, no behaviour change for the common case.
  if (!art.pending) {
    return <a className="res-dl" href={url} download>{label}</a>;
  }

  const run = async () => {
    setBusy(true);
    setErr("");
    try {
      await downloadViaFetch(url, filename);
      // The file now exists, so re-reading the manifest turns this row into an
      // ordinary one with a real size.
      onPrepared?.();
    } catch (e: any) {
      setErr(e?.message || "Could not prepare this file.");
      window.location.href = url;   // fallback: let the browser do it
    } finally {
      setBusy(false);
    }
  };

  return (
    <span>
      <button className="res-dl" onClick={run} disabled={busy}
              title="This object has not been written to disk yet — preparing it takes a moment for large objects.">
        {busy ? "Preparing…" : "Prepare & download"}
      </button>
      {err && <div className="sub err-text">{err}</div>}
    </span>
  );
}

// One figure (svg/png/pdf grouped): PNG thumbnail + a download link per format.
function FigureCard({ art }: { art: ResultArtifact }) {
  const fmts = art.formats ?? [];
  return (
    <div className="fig-card">
      {art.thumb && <img className="thumb" src={art.thumb} alt={art.name || "figure"} loading="lazy" />}
      <div className="cap">
        <span className="name">{art.name}</span>
        <span>
          {fmts.map((f) => (
            <a key={f.filename} href={f.url} download title={`Download ${f.format.toUpperCase()}`}>
              {f.format.toUpperCase()}
            </a>
          ))}
        </span>
      </div>
    </div>
  );
}

// One server-side object: handle + type + one-line summary + provenance + .rds.
function ObjectRow({ art, onPrepared }: { art: ResultArtifact; onPrepared?: () => void }) {
  return (
    <div className="res-row">
      <div className="grow">
        <div className="fn">
          {art.handle ? <strong>{art.handle}</strong> : art.filename}
          {art.type && <span className="chip" style={{ marginLeft: 6 }}>{art.type}</span>}
        </div>
        {art.summary && <div className="sub">{art.summary}</div>}
        <div className="sub">
          {art.source ? `produced by ${art.source} · ` : ""}{art.filename}
        </div>
      </div>
      {/* A pending row has no size, because knowing it would mean doing the
          work the whole change exists to defer. Show a dash, not a blank. */}
      <span className="size">{art.pending ? "—" : fmtBytes(art.size)}</span>
      <DownloadAction art={art} label="Download .rds" onPrepared={onPrepared} />
    </div>
  );
}

// One plain file (table/text/other) with optional object provenance.
function FileRow({ art, onPrepared }: { art: ResultArtifact; onPrepared?: () => void }) {
  const label =
    art.kind === "table" ? "Download .csv" :
    art.kind === "text"  ? "Download .txt" : "Download";
  const prov = [art.handle, art.type, art.summary].filter(Boolean).join(" · ");
  return (
    <div className="res-row">
      <div className="grow">
        <div className="fn">{art.filename}</div>
        {prov && <div className="sub">{prov}</div>}
      </div>
      <span className="size">{art.pending ? "—" : fmtBytes(art.size)}</span>
      <DownloadAction art={art} label={label} onPrepared={onPrepared} />
    </div>
  );
}

export default function Results({ session }: { session: string }) {
  const [manifest, setManifest] = useState<ResultsManifest | null>(null);
  const [loading, setLoading] = useState(false);
  const [zipping, setZipping] = useState(false);
  const [error, setError] = useState<string>("");

  const load = useCallback(async () => {
    if (!session) return;
    setLoading(true);
    setError("");
    try {
      setManifest(await api.listArtifacts(session));
    } catch (e: any) {
      setError(e?.message || "Could not load results.");
    } finally {
      setLoading(false);
    }
  }, [session]);

  // Refetch whenever the tab is opened (component remounts) or the session changes.
  useEffect(() => { load(); }, [load]);

  const arts = manifest?.artifacts ?? [];
  const figures = arts.filter((a) => a.kind === "figure");
  const objects = arts.filter((a) => a.kind === "rds");
  const tables  = arts.filter((a) => a.kind === "table");
  const texts   = arts.filter((a) => a.kind === "text");
  const others  = arts.filter((a) => a.kind === "other");
  const hasAny  = arts.length > 0;
  const nPending = arts.filter((a) => a.pending).length;

  // Round LIV: "Download all" is now the moment every not-yet-written object
  // gets serialized, so it can take real time on a session holding large
  // objects. It was a bare <a download>, which gives no event to show a waiting
  // state from — hence the fetch, and hence this being a button.
  const downloadAll = async () => {
    if (!session) return;
    setZipping(true);
    setError("");
    try {
      await downloadViaFetch(api.artifactsZipUrl(session), `celliverse_${session}_results.zip`);
      await load();   // pending rows are now real files with real sizes
    } catch (e: any) {
      setError(e?.message || "Could not prepare the download.");
      window.location.href = api.artifactsZipUrl(session);
    } finally {
      setZipping(false);
    }
  };

  return (
    <div>
      <div className="results-head">
        <h3>Results</h3>
        {hasAny && (
          <span className="muted" style={{ fontSize: 12 }}>
            {arts.length} artifact{arts.length === 1 ? "" : "s"}
          </span>
        )}
        <span className="spacer" />
        <button className="btn secondary" onClick={load} disabled={loading || zipping || !session}>
          {loading ? "Refreshing…" : "Refresh"}
        </button>
        <button className="btn" onClick={downloadAll} disabled={!hasAny || zipping}>
          {zipping ? "Preparing…" : "Download all (zip)"}
        </button>
      </div>

      {zipping && (
        <div className="sub" style={{ marginBottom: 12 }}>
          Preparing your download — writing out the session objects and packaging them.
          This can take a moment for large objects, and only happens once.
        </div>
      )}

      {!zipping && nPending > 0 && (
        <div className="sub" style={{ marginBottom: 12 }}>
          {nPending} file{nPending === 1 ? " is" : "s are"} prepared on download, so nothing
          large is written until you ask for it.
        </div>
      )}

      {error && <div className="err-text" style={{ marginBottom: 12 }}>{error}</div>}

      {!hasAny && !loading && (
        <div className="empty">
          No results yet. Ask the agent to cluster, find markers, annotate, or plot — every
          figure, table, and object it produces appears here, each downloadable on its own
          (objects as portable .rds you can read back into R).
        </div>
      )}

      {figures.length > 0 && (
        <section className="res-section">
          <h4>Figures ({figures.length})</h4>
          <div className="fig-grid">
            {figures.map((a, i) => <FigureCard key={i} art={a} />)}
          </div>
        </section>
      )}

      {objects.length > 0 && (
        <section className="res-section">
          <h4>Objects — RDS ({objects.length})</h4>
          <div className="card" style={{ padding: "2px 14px" }}>
            {objects.map((a, i) => <ObjectRow key={i} art={a} onPrepared={load} />)}
          </div>
        </section>
      )}

      {tables.length > 0 && (
        <section className="res-section">
          <h4>Tables — CSV ({tables.length})</h4>
          <div className="card" style={{ padding: "2px 14px" }}>
            {tables.map((a, i) => <FileRow key={i} art={a} onPrepared={load} />)}
          </div>
        </section>
      )}

      {texts.length > 0 && (
        <section className="res-section">
          <h4>Text — TXT ({texts.length})</h4>
          <div className="card" style={{ padding: "2px 14px" }}>
            {texts.map((a, i) => <FileRow key={i} art={a} onPrepared={load} />)}
          </div>
        </section>
      )}

      {others.length > 0 && (
        <section className="res-section">
          <h4>Other ({others.length})</h4>
          <div className="card" style={{ padding: "2px 14px" }}>
            {others.map((a, i) => <FileRow key={i} art={a} onPrepared={load} />)}
          </div>
        </section>
      )}
    </div>
  );
}
