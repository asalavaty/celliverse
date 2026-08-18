import { useEffect, useState } from "react";
import { api, JobInfo } from "../api/client";

// Async job status (from /api/jobs), with a live poll on the selected job.
//
// Round LXXIX (audit #49 + #54), the same two defects PackageBrowser had:
//
//   #49 — the job rows were `<div onClick>`: not focusable, not operable from
//   the keyboard. They are <button>s now.
//
//   #54 — the detail panel was `JSON.stringify(detail, null, 2)`, shipped as a
//   feature. The eight fields cv_job_public() returns are named and typed on
//   the server, so there was never a reason to make the reader pick `status`
//   out of a brace-delimited blob; the one field that genuinely benefits from
//   raw text (an error) is now the one field shown as text.

/** ISO/epoch-ish timestamp -> a short local string. Blank if unreadable. */
export function fmtStamp(v: unknown): string {
  if (v == null || v === "") return "";
  const d = typeof v === "number" ? new Date(v * (v > 1e11 ? 1 : 1000)) : new Date(String(v));
  return isNaN(d.getTime()) ? String(v) : d.toLocaleString();
}

/**
 * The rows the detail panel shows, in order, skipping anything absent.
 *
 * Pure and exported so what a job detail is REQUIRED to surface can be asserted
 * without a DOM. `error` is deliberately excluded here: it is rendered on its
 * own below, in a block that preserves line breaks, because a worker traceback
 * folded into a definition list is unreadable.
 */
export function jobDetailRows(detail: any): Array<{ label: string; value: string }> {
  if (!detail || typeof detail !== "object") return [];
  const out: Array<{ label: string; value: string }> = [];
  const add = (label: string, value: unknown) => {
    if (value === null || value === undefined || value === "") return;
    out.push({ label, value: String(value) });
  };
  add("Tool", detail.tool);
  add("Status", detail.status);
  if (typeof detail.progress === "number" && isFinite(detail.progress)) {
    add("Progress", `${Math.round(Math.min(100, Math.max(0, detail.progress)))}%`);
  }
  add("Message", detail.message);
  add("Result object", detail.handle);
  add("Started", fmtStamp(detail.created));
  add("Updated", fmtStamp(detail.updated));
  add("Job id", detail.id);
  return out;
}

export default function Logs({ session, jobs }: { session: string; jobs: JobInfo[] }) {
  const [selected, setSelected] = useState<string>("");
  const [detail, setDetail] = useState<any | null>(null);
  // Round LXXX (audit #71): the local turn summary. Derived from
  // ~/.celliverse/logs/*.jsonl on the server; nothing leaves the machine.
  const [sum, setSum] = useState<{ summary: Record<string, number> | null;
                                   enabled: boolean; log_dir: string; keep_days: number } | null>(null);
  useEffect(() => { api.logSummary(7).then(setSum).catch(() => setSum(null)); }, []);

  useEffect(() => {
    if (!selected) return;
    let live = true;
    const poll = async () => {
      try { const d = await api.jobStatus(session, selected); if (live) setDetail(d); } catch { /* ignore */ }
    };
    poll();
    const t = setInterval(poll, 2000);
    return () => { live = false; clearInterval(t); };
  }, [selected]);

  const rows = jobDetailRows(detail);

  return (
    <div>
      <div className="card">
        <h3>Jobs (this session)</h3>
        {jobs.length === 0 && <div className="muted">No async jobs have run yet. Heavy tools (e.g. clustoCell) appear here while running.</div>}
        <div className="job-list">
          {jobs.map((j) => (
            <button
              key={j.id}
              type="button"
              className={"job-row mono" + (selected === j.id ? " is-sel" : "")}
              aria-pressed={selected === j.id}
              onClick={() => setSelected(j.id)}
            >
              <strong>{j.tool}</strong> · {j.status}
              {j.progress != null ? ` · ${Math.round(Math.min(100, Math.max(0, j.progress || 0)))}%` : ""}
              {j.message ? ` · ${j.message}` : ""}
            </button>
          ))}
        </div>
      </div>
      {detail && (
        <div className="card">
          <h3>Job {selected}</h3>
          <dl className="job-detail">
            {rows.map((r) => (
              <div key={r.label}><dt>{r.label}</dt><dd>{r.value}</dd></div>
            ))}
          </dl>
          {detail.error && (
            <div className="job-error">
              <div className="job-error-head">What went wrong</div>
              <pre className="mono">{String(detail.error)}</pre>
            </div>
          )}
          {/* Kept behind a disclosure rather than deleted: this is a debugging
              page, and a field the renderer above does not know about must
              still be reachable. */}
          <details className="job-raw">
            <summary>Raw job record</summary>
            <pre className="mono">{JSON.stringify(detail, null, 2)}</pre>
          </details>
          {(detail.status === "running") && (
            <button className="btn secondary" onClick={() => api.cancelJob(session, selected)}>Cancel job</button>
          )}
        </div>
      )}
      {/* Round LXXX (audit #71). The three questions this answers -- do turns
          finish, do tools fail, how long does a turn take -- previously had no
          answer at all: light tools left no record anywhere, and every
          performance number in this project's history came from a bespoke
          harness written after a complaint. */}
      {sum && (
        <div className="card">
          <h3>Last 7 days (this machine)</h3>
          {!sum.summary ? (
            <div className="muted">
              No turns recorded yet. {sum.enabled
                ? <>Activity is logged locally to <span className="mono">{sum.log_dir}</span> and kept for {sum.keep_days} days.</>
                : <>Local logging is switched off (<span className="mono">CELLIVERSE_NO_LOG</span>).</>}
            </div>
          ) : (
            <>
              <dl className="job-detail">
                <div><dt>Turns</dt><dd>{sum.summary.turns}</dd></div>
                <div><dt>Completed</dt><dd>
                  {sum.summary.turns_completed} ({Math.round((sum.summary.completion_rate ?? 0) * 100)}%)
                </dd></div>
                <div><dt>Failed / stopped</dt><dd>
                  {sum.summary.turns_failed} / {sum.summary.turns_cancelled}
                </dd></div>
                <div><dt>Tool calls</dt><dd>
                  {sum.summary.tool_calls}, {sum.summary.tool_failures} failed
                  ({Math.round((sum.summary.tool_failure_rate ?? 0) * 100)}%)
                </dd></div>
                <div><dt>Turn time</dt><dd>
                  median {sum.summary.median_turn_sec}s · 90th percentile {sum.summary.p90_turn_sec}s
                </dd></div>
                <div><dt>Tokens</dt><dd>
                  {sum.summary.turns_with_tokens > 0
                    ? <>{sum.summary.total_tokens} across {sum.summary.turns_with_tokens} of {sum.summary.turns} turns</>
                    : <>not reported — a local model does not meter tokens</>}
                </dd></div>
              </dl>
              {/* The honest marker, always shown: a token total over 3 of 40
                  turns must not read as a total over 40. */}
              <p className="muted" style={{ fontSize: 12 }}>
                From <span className="mono">{sum.log_dir}</span>, kept {sum.keep_days} days. This never
                leaves your machine.
              </p>
            </>
          )}
        </div>
      )}
      <p className="muted" style={{ fontSize: 12 }}>
        Session: <span className="mono">{session || "—"}</span>
      </p>
    </div>
  );
}
