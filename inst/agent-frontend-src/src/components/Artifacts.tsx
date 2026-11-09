import { useEffect, useState } from "react";
import { api } from "../api/client";

// A rendered plot artifact: { kind:"plot", files:[{format,url,filename}], primary:{url,format} }
// or, when nothing could be written, { kind:"plot", error:"..." } and no `primary`.
export function PlotArtifact({ art }: { art: any }) {
  if (!art || art.kind !== "plot") return null;
  // Round LXIV (D9): a total render failure used to return null here, which
  // left the card showing a green tick and the model's prose describing a
  // figure that was never produced. Say the figure is missing; the analysis
  // itself may have been fine, so this reports the RENDERING, nothing more.
  if (!art.primary) {
    if (!art.error) return null;
    return (
      <div className="artifact trun-hint" style={{ padding: 8 }}>
        The figure could not be produced. The analysis itself may have finished —
        only the image failed to render. Ask for the plot again, or request a
        different format.
      </div>
    );
  }
  const png = (art.files || []).find((f: any) => f.format === "png");
  const svg = (art.files || []).find((f: any) => f.format === "svg");
  // Show PNG inline (universally rendered); offer SVG + PNG downloads.
  const display = png?.url || art.primary.url;
  return (
    <div className="artifact">
      <img src={display} alt="plot" loading="lazy" />
      <div className="cap">
        <span>plot</span>
        {svg && <a href={svg.url} target="_blank" rel="noreferrer">SVG</a>}
        {png && <a href={png.url} target="_blank" rel="noreferrer">PNG</a>}
      </div>
    </div>
  );
}

// A rendered table artifact (from cv_render_table): has columns, rows, paging, csv url.
//
// Round LXXV (D5 / audit #46). The previous version was
//   const [rows] = useState<any[]>(art?.rows ?? []);
// destructured WITHOUT a setter, so the component could not change page even in
// principle: it rendered rows 1-50 of 5,000 and truthfully captioned
// "page 1/100" with no control to go anywhere. `.pager` and `.pager button`
// had been sitting in styles.css unused since the beginning.
//
// Pages are fetched from /api/table, which re-slices the CSV that
// cv_render_table() already wrote to the session's artifacts dir -- the same
// bytes the "Download CSV" link serves, so the page on screen and the file on
// disk cannot disagree. That also means paging works for a table of any size
// and does not depend on the whole frame having been shipped to the browser
// (which it no longer is -- see cv_result_for_browser() in agent_render.R).
//
// Defensive on `columns` because plumber's unboxedJSON uses auto_unbox=TRUE: a
// single-column table arrives as a bare string, not a 1-element array, and
// cols.map() would throw on exactly the narrow tables this is meant to fix.
export function TableArtifact({ art, session }: { art: any; session?: string }) {
  const [rows, setRows] = useState<any[]>(art?.rows ?? []);
  const [page, setPage] = useState<number>(art?.page ?? 1);
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState("");

  // A new result must reset the view; without this a second table rendered into
  // the same slot would keep the first one's rows. This is the resync the
  // missing setter also made impossible.
  useEffect(() => {
    setRows(art?.rows ?? []);
    setPage(art?.page ?? 1);
    setErr("");
  }, [art]);

  if (!art || !art.columns) return null;
  const cols: string[] = Array.isArray(art.columns) ? art.columns : [art.columns];
  const nPages: number = Number(art.n_pages ?? 1) || 1;
  const csvName: string | undefined = art.csv?.filename;
  const canPage = nPages > 1 && !!csvName && !!session;

  const go = async (target: number) => {
    const p = Math.max(1, Math.min(target, nPages));
    if (p === page || busy || !csvName || !session) return;
    setBusy(true);
    setErr("");
    try {
      const r = await api.tablePage(session, csvName, p, art.page_size ?? 50);
      setRows(Array.isArray(r.rows) ? r.rows : r.rows ? [r.rows] : []);
      setPage(r.page ?? p);
    } catch (e: any) {
      // Say what happened and what to do next; the rows already on screen stay,
      // so a failed page turn is never a blank table.
      setErr(e?.message || "That page could not be loaded. The full table is in the CSV.");
    } finally {
      setBusy(false);
    }
  };

  return (
    <div className="artifact" style={{ padding: 8 }}>
      <div style={{ maxHeight: 340, overflow: "auto" }}>
        <table className="grid">
          <thead>
            <tr>{cols.map((c) => <th key={c}>{c}</th>)}</tr>
          </thead>
          <tbody>
            {rows.map((r, i) => (
              <tr key={i}>{cols.map((c) => <td key={c}>{fmt(r[c])}</td>)}</tr>
            ))}
          </tbody>
        </table>
      </div>
      {canPage && (
        <div className="pager">
          <button onClick={() => go(page - 1)} disabled={busy || page <= 1}
                  aria-label="Previous page">‹ Prev</button>
          <span>{busy ? "Loading…" : `Page ${page} of ${nPages}`}</span>
          <button onClick={() => go(page + 1)} disabled={busy || page >= nPages}
                  aria-label="Next page">Next ›</button>
        </div>
      )}
      {err && <div className="sub err-text">{err}</div>}
      <div className="cap">
        <span>{art.nrow} rows × {art.ncol} cols {nPages > 1 ? `· page ${page}/${nPages}` : ""}</span>
        {art.csv?.url && <a href={art.csv.url} target="_blank" rel="noreferrer">Download CSV</a>}
      </div>
    </div>
  );
}

function fmt(v: unknown): string {
  if (v == null) return "";
  if (typeof v === "number") return Number.isInteger(v) ? String(v) : v.toFixed(4);
  return String(v);
}
