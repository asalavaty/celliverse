import { useEffect, useMemo, useState } from "react";
import { api, CvApiError } from "../api/client";

// Lists persisted sessions from ~/.celliverse/sessions via /api/sessions.
// The backend returns structured records { id, created, updated, n_messages }.
// We still normalize defensively so a bare-string entry (older server) renders.
//
// Round LXVIII reworked this page. What it was: an unbounded list of clickable
// ids that grew forever, whose only action was to dump a session's raw JSON
// into a <pre>. Every conversation was on disk and reachable through the API,
// and there was no way to get back into one and no way to delete one.
//
// What it is now: a fixed scroll window over the newest 100, a search box and a
// date range that reach the ones outside that window, Restore per row, and
// Clear history (audit #67 -- there was previously no session-delete route at
// all, so a transcript containing embargoed marker genes could not be erased).

type SessionRow = {
  id: string;
  created?: string | null;
  updated?: string | null;
  n_messages?: number | null;
};

// How many rows are RENDERED. Deliberately a display bound and not a retention
// policy: nothing is deleted to satisfy it, and the true total is always stated
// beside it. Round LVI is the governing precedent in this codebase -- a silent
// cap hid clusters C3-C9 on a 30-cluster object -- and the rule that came out
// of it is "bounded with an honest marker, never quietly smaller".
const MAX_ROWS = 100;

// Accept either a rich object or a bare id string and return a uniform row.
function normalizeSession(s: any): SessionRow {
  if (typeof s === "string") return { id: s };
  return {
    id: s?.id ?? s?.session_id ?? "",
    created: s?.created ?? null,
    updated: s?.updated ?? s?.created ?? null,
    n_messages: s?.n_messages ?? s?.messages ?? null,
  };
}

function fmtWhen(iso?: string | null): string {
  if (!iso) return "unknown time";
  const d = new Date(iso);
  return isNaN(d.getTime()) ? String(iso) : d.toLocaleString();
}

function fmtMsgs(n?: number | null): string {
  if (n == null) return "? msgs";
  return `${n} msg${n === 1 ? "" : "s"}`;
}

// ---- Round LXXIX (audit #54): reading a stored session --------------------

/**
 * The turns worth SHOWING from a stored session record.
 *
 * `tool` and `system` messages are internal plumbing and are dropped, exactly
 * as historyToItems() in Chat.tsx drops them when rehydrating a live
 * conversation — the two must not disagree about what a transcript is, or a
 * restored session would show a different history from the one previewed here.
 *
 * Pure and exported so that agreement can be asserted directly.
 */
export function detailTurns(detail: any): Array<{ role: string; text: string }> {
  const hist = Array.isArray(detail?.history) ? detail.history : [];
  const out: Array<{ role: string; text: string }> = [];
  for (const m of hist) {
    const role = m?.role;
    const content = typeof m?.content === "string" ? m.content : "";
    if (role === "user") out.push({ role: "you", text: content });
    else if (role === "assistant" && content) out.push({ role: "agent", text: content });
  }
  return out;
}

/**
 * One line naming what the record contains, including the parts the transcript
 * above deliberately does not render.
 *
 * The counts are stated rather than implied. A session whose history is 40
 * messages but whose transcript shows 12 has had 28 tool/system records
 * filtered out, and a reader who is not told that will read the difference as
 * data loss.
 */
export function detailSummary(detail: any): string {
  const hist = Array.isArray(detail?.history) ? detail.history : [];
  const shown = detailTurns(detail).length;
  const objs = Array.isArray(detail?.objects) ? detail.objects.length : 0;
  const det = Array.isArray(detail?.detached) ? detail.detached.length : 0;
  const bits: string[] = [];
  bits.push(`${shown} message${shown === 1 ? "" : "s"} of ${hist.length} record${hist.length === 1 ? "" : "s"}` +
            (hist.length > shown ? ` (${hist.length - shown} tool/system record${hist.length - shown === 1 ? "" : "s"} not shown)` : ""));
  if (objs) bits.push(`${objs} object${objs === 1 ? "" : "s"} still loaded`);
  if (det) bits.push(`${det} object${det === 1 ? "" : "s"} no longer loaded`);
  return bits.join(" · ");
}

// The calendar day a row belongs to, as YYYY-MM-DD.
//
// Compared as a STRING against the two <input type="date"> values, which are
// themselves always YYYY-MM-DD. Deliberately not Date arithmetic: cv_now()
// formats as "%Y-%m-%dT%H:%M:%S%z", giving an offset like "+1000" with no
// colon, and how a JS engine parses that is implementation-defined. The first
// ten characters are unambiguous in every case, including the degraded rows
// where `updated` is null.
export function sessionDay(row: SessionRow): string {
  const iso = row.updated ?? row.created ?? "";
  return typeof iso === "string" && iso.length >= 10 ? iso.slice(0, 10) : "";
}

/**
 * Filter the FULL session list; the caller then caps what it renders.
 *
 * That order is the whole point of the search box. Filtering AFTER capping
 * would mean a date search could never reach a conversation outside the newest
 * 100 -- which would make the date control decorative on exactly the archive it
 * exists to search.
 *
 * Exported as a pure function (no React state) so it can be transpiled and
 * tested directly in Node -- the same convention as Chat.tsx's
 * mergeToolResultItem() and Settings.tsx's reconcileModelForProvider().
 */
export function filterSessions(
  rows: SessionRow[],
  query: string,
  from: string,
  to: string
): SessionRow[] {
  const q = query.trim().toLowerCase();
  return rows.filter((r) => {
    if (q) {
      // Match the id AND the stored timestamps, so "sess_09" and a fragment of
      // a date both work without needing a second control.
      const hay = `${r.id} ${r.updated ?? ""} ${r.created ?? ""}`.toLowerCase();
      if (!hay.includes(q)) return false;
    }
    const day = sessionDay(r);
    // A row with no usable timestamp is kept when no date bound is set and
    // dropped when one is: it cannot honestly be shown as satisfying a range,
    // but hiding it from an unfiltered list would lose a session over a
    // missing field.
    if (from && (!day || day < from)) return false;
    if (to && (!day || day > to)) return false;
    return true;
  });
}

/**
 * Everything the list needs to render: which rows to draw, and the true totals
 * to state beside them.
 *
 * This exists so the FILTER-THEN-CAP order is a property of one tested
 * function rather than of how the component happens to chain two calls. Get it
 * backwards -- cap first, filter after -- and the page looks identical for the
 * newest 100 while the date picker silently cannot reach anything older, which
 * is the only reason the date picker exists. That is not a difference a source
 * review reliably catches, so it is asserted instead.
 *
 * `total` and `matched` are returned rather than recomputed by the caller
 * because the count line is the honest marker that makes a display cap
 * acceptable at all (Round LVI), and a marker computed from the capped list
 * would report the cap back to itself.
 */
export function visibleSessions(
  rows: SessionRow[],
  query: string,
  from: string,
  to: string,
  max: number = MAX_ROWS
): { shown: SessionRow[]; total: number; matched: number; hidden: number } {
  const matched = filterSessions(rows, query, from, to);
  const shown = matched.slice(0, max);
  return {
    shown,
    total: rows.length,
    matched: matched.length,
    hidden: matched.length - shown.length,
  };
}

type Props = {
  /** The session currently open in Chat. Never a delete candidate. */
  current: string;
  /** Adopt a past session: App repoints the app at it and rehydrates Chat. */
  onRestore: (id: string, full: any) => void;
};

export default function History({ current, onRestore }: Props) {
  const [sessions, setSessions] = useState<SessionRow[]>([]);
  const [err, setErr] = useState("");
  const [detail, setDetail] = useState<any | null>(null);
  const [query, setQuery] = useState("");
  const [from, setFrom] = useState("");
  const [to, setTo] = useState("");
  const [busy, setBusy] = useState("");        // id being restored, or "*" while clearing
  const [confirming, setConfirming] = useState(false);
  const [notice, setNotice] = useState("");

  async function load() {
    try {
      const r = await api.listSessions();
      setSessions((r.sessions ?? []).map(normalizeSession).filter((s) => s.id));
      setErr("");
    } catch (e: any) {
      setErr(e.message);
    }
  }

  useEffect(() => { load(); }, []);

  const { shown, total, matched, hidden } = useMemo(
    () => visibleSessions(sessions, query, from, to),
    [sessions, query, from, to]
  );
  const isFiltered = Boolean(query.trim() || from || to);
  const others = sessions.filter((s) => s.id !== current).length;

  async function open(id: string) {
    if (!id) return;
    try {
      setDetail(await api.getSession(id));
    } catch (e: any) {
      setErr(e.message);
    }
  }

  async function restore(id: string) {
    if (!id || busy) return;
    setBusy(id);
    setNotice("");
    try {
      const full = await api.getSession(id);
      onRestore(id, full);
    } catch (e: any) {
      setErr(e.message);
    } finally {
      setBusy("");
    }
  }

  // Clear every saved conversation except the one currently open.
  //
  // Fans out over the single-id route rather than adding a second, bulk
  // destructive endpoint. Promise.allSettled, not Promise.all: one refusal (a
  // session with a tool still running returns 409) must not abandon the rest,
  // and the count reported afterwards has to be what actually happened rather
  // than what was attempted.
  async function clearAll() {
    const targets = sessions.map((s) => s.id).filter((id) => id && id !== current);
    setConfirming(false);
    if (!targets.length) return;
    setBusy("*");
    setNotice("");
    const results = await Promise.allSettled(targets.map((id) => api.deleteSession(id)));
    const failed = results.filter((r) => r.status === "rejected") as PromiseRejectedResult[];
    const done = results.length - failed.length;
    const why = failed.length
      ? ` ${failed.length} could not be deleted: ${
          failed[0].reason instanceof CvApiError
            ? failed[0].reason.message
            : "the server refused the request."
        }`
      : "";
    setNotice(
      `Deleted ${done} conversation${done === 1 ? "" : "s"}. The one you are in was kept,` +
      ` and every saved figure, table and export stays on disk.${why}`
    );
    setBusy("");
    setDetail(null);
    await load();
  }

  if (err) return <div className="err-text">Could not load history: {err}</div>;

  return (
    <div>
      <div className="card">
        <div className="hist-head">
          <h3>Saved conversations</h3>
          {!confirming && (
            <button
              className="btn secondary"
              onClick={() => setConfirming(true)}
              disabled={Boolean(busy) || others === 0}
              title={
                others === 0
                  ? "There is nothing to clear yet."
                  : "Delete every saved conversation except the one you are in."
              }
            >
              Clear history
            </button>
          )}
        </div>

        {confirming && (
          <div className="hist-confirm">
            <div>
              Delete {others} saved conversation{others === 1 ? "" : "s"}? The one you are in
              is kept. Saved figures, tables and exports are not deleted. This cannot be
              undone.
            </div>
            <div className="hist-confirm-actions">
              <button className="btn" onClick={clearAll} disabled={Boolean(busy)}>
                {busy === "*" ? "Deleting…" : "Delete them"}
              </button>
              <button className="btn secondary" onClick={() => setConfirming(false)}>
                Cancel
              </button>
            </div>
          </div>
        )}

        {notice && <div className="hist-notice">{notice}</div>}

        <div className="hist-filters">
          <input
            type="search"
            className="hist-search"
            placeholder="Search by session id or date…"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            aria-label="Search saved conversations"
          />
          <span className="hist-range">
            <label htmlFor="hist-from">From</label>
            <input id="hist-from" type="date" value={from}
                   onChange={(e) => setFrom(e.target.value)} />
            <label htmlFor="hist-to">to</label>
            <input id="hist-to" type="date" value={to}
                   onChange={(e) => setTo(e.target.value)} />
          </span>
          {isFiltered && (
            <button className="btn secondary"
                    onClick={() => { setQuery(""); setFrom(""); setTo(""); }}>
              Reset
            </button>
          )}
        </div>

        {/* The count line is not decoration: it is the honest marker that makes
            a display cap acceptable at all. It always states the true total. */}
        <div className="muted hist-count">
          {total === 0
            ? "No saved conversations yet."
            : isFiltered
            ? `${matched} of ${total} conversations match` +
              (hidden > 0 ? ` — showing the ${MAX_ROWS} most recent of them.` : ".")
            : hidden > 0
            ? `Showing the ${MAX_ROWS} most recent of ${total} conversations. Search, or pick a date range, to reach the other ${hidden}.`
            : `${total} conversation${total === 1 ? "" : "s"}.`}
        </div>

        {/* Fixed window. The page used to grow without bound, so the filters and
            the Clear button scrolled away from the rows they act on. */}
        <div className="hist-list">
          {shown.map((s) => (
            <div key={s.id} className={`hist-row${s.id === current ? " is-current" : ""}`}>
              <button
                type="button"
                className="hist-open mono"
                onClick={() => open(s.id)}
                title="Show this conversation's stored record"
              >
                {s.id}
              </button>
              <span className="muted hist-meta">
                {fmtWhen(s.updated)} · {fmtMsgs(s.n_messages)}
              </span>
              {s.id === current ? (
                <span className="hist-current-tag">current</span>
              ) : (
                <button
                  className="btn secondary hist-restore"
                  onClick={() => restore(s.id)}
                  disabled={Boolean(busy)}
                  title="Reopen this conversation in the Chat tab"
                >
                  {busy === s.id ? "Restoring…" : "Restore"}
                </button>
              )}
            </div>
          ))}
          {sessions.length > 0 && shown.length === 0 && (
            <div className="muted" style={{ padding: "8px 0" }}>
              No conversation matches that search.
            </div>
          )}
        </div>
      </div>

      {detail && (
        <div className="card">
          <h3>Session detail</h3>
          {/* Round LXXIX (audit #54): this was `JSON.stringify(detail)` in a
              <pre>, shipped as one of nine nav tabs. What a reader opens a past
              conversation to see is WHAT WAS SAID, and an escaped-newline blob
              is the one form in which that is unreadable. It renders as a
              transcript now; the raw record is still one click away for anyone
              debugging the store. */}
          <div className="muted" style={{ fontSize: 12, marginBottom: 8 }}>
            {detailSummary(detail)}
          </div>
          <div className="hist-transcript">
            {detailTurns(detail).length === 0
              ? <div className="muted">Nothing was said in this conversation.</div>
              : detailTurns(detail).map((t, i) => (
                  <div key={i} className={`hist-turn hist-turn-${t.role}`}>
                    <div className="hist-turn-role">{t.role}</div>
                    <div className="hist-turn-text">{t.text}</div>
                  </div>
                ))}
          </div>
          <details className="hist-raw">
            <summary>Raw session record</summary>
            <pre className="mono">{JSON.stringify(detail, null, 2)}</pre>
          </details>
        </div>
      )}
    </div>
  );
}
