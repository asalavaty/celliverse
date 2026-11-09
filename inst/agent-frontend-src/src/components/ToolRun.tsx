// One tool call, rendered as a single card that lives through its whole
// lifecycle (Round XLVII).
//
// WHAT THIS REPLACES. The chat used to emit one throwaway line per event:
// `▶ running clustoCell…` on tool_start, a separate red `✗ clustoCell: …` on
// tool_error, and a third block on tool_result. A run that retried twice left
// five stacked lines with no relationship between them, no indication of
// progress, and no sense that anything was still alive.
//
// Worse, it threw information away. The worker emits `progress` events roughly
// every 400 ms carrying a percent AND the analysis's own status message
// ("Preparing the clusters, cell-subsets, and marker panels."). Those events
// reached the browser and were dropped on the floor by the chat reducer — they
// only ever surfaced in the Jobs sidebar. So a 30-minute clustering run showed
// a motionless italic line while the data needed to animate it was arriving the
// whole time.
//
// The card here is the single owner of a tool call: it shows live percent,
// elapsed time and the tool's own message while running; it becomes the result
// when the tool finishes; and any earlier failed attempts fold INTO it rather
// than sitting above it as red noise. A retry that eventually succeeded should
// read as one step that took a while, not as a series of failures.

import { useEffect, useState } from "react";
import { PlotArtifact, TableArtifact } from "./Artifacts";
import type { CvWarning } from "../api/stream";

/** An earlier attempt at the same tool that did not finish. */
export type ToolAttempt = {
  outcome: "timeout" | "error" | "cancelled";
  detail?: string;
  ms?: number;
};

/**
 * Round LXIX (audit #24/#25): split a tool result's warnings by what they cost
 * the reader.
 *
 * `blocking` are the may_invalidate ones. They are rendered ALWAYS, in full,
 * uncollapsed, above the result — ignoring one can leave you with a wrong
 * conclusion from the numbers beside it, so hiding it behind a toggle would
 * defeat the point of having raised it.
 *
 * `notes` are informational: the agent saying what it decided. They go in a
 * collapsed disclosure, the same treatment as `Settings used`, because a run
 * that auto-resolved a handle and turned log1p off is a normal, correct run and
 * should not look like a problem.
 *
 * Exported and pure so the behaviour is testable without a DOM — the
 * convention mergeToolResultItem() and closeOpenToolCards() already set.
 */
export function splitWarnings(warnings?: CvWarning[] | null): {
  blocking: CvWarning[];
  notes: CvWarning[];
} {
  const all = Array.isArray(warnings) ? warnings.filter((w) => w && typeof w.text === "string" && w.text) : [];
  return {
    blocking: all.filter((w) => w.severity === "may_invalidate"),
    // Anything that is not may_invalidate is treated as a note rather than
    // dropped. A severity this client does not recognise is still something the
    // server wanted the user told; silently discarding it would be the worst of
    // the available options.
    notes: all.filter((w) => w.severity !== "may_invalidate"),
  };
}

/**
 * The card state a finished tool call earns.
 *
 * Amber is reserved for may_invalidate. If any warning turned the card amber,
 * the state would fire on nearly every run — the log1p override alone fires on
 * most normalized datasets — and an indicator that is always on carries no
 * information. That is the specific failure audit item 3b#5 forbids.
 */
export function doneStatusFor(warnings?: CvWarning[] | null): "done" | "done_with_warnings" {
  return splitWarnings(warnings).blocking.length ? "done_with_warnings" : "done";
}

export type ToolItemLike = {
  tool: string;
  status: "start" | "done" | "done_with_warnings" | "error" | "skipped" | "stopped";
  detail?: string;
  artifact?: any;
  table?: any;
  // Round LXV (audit #22): the arguments the call actually ran with.
  args?: Record<string, unknown>;
  // Round LXIX (audit #23/#24/#25): the caveats this call raised, already
  // sorted may_invalidate-first by the server.
  warnings?: CvWarning[];
  progress?: number;
  note?: string;
  startedAt?: number;
  endedAt?: number;
  attempts?: ToolAttempt[];
};

/** m:ss, or h:mm:ss past an hour. Blank for an unknown duration. */
export function formatElapsed(ms?: number): string {
  if (ms == null || !isFinite(ms) || ms < 0) return "";
  const total = Math.floor(ms / 1000);
  const s = total % 60;
  const m = Math.floor(total / 60) % 60;
  const h = Math.floor(total / 3600);
  const pad = (n: number) => String(n).padStart(2, "0");
  return h > 0 ? `${h}:${pad(m)}:${pad(s)}` : `${m}:${pad(s)}`;
}

// After this long, a running tool starts saying that a long wait is normal and
// that it can be stopped. Chosen so ordinary fast tools never show it at all.
const REASSURE_AFTER_MS = 90_000;

// Round LXXXVI, from live use: this hint used to name "a full clustering
// pass" for EVERY heavy tool running past REASSURE_AFTER_MS, including
// umapPlot -- which draws a plot, not a clustering pass, and kept the same
// sentence anyway. Only clustoCell and markoClust actually run a clustering
// pass (major and, optionally, sub-cluster detection); everything else gets a
// plain, tool-agnostic sentence rather than a guess at what it is doing.
const CLUSTERING_TOOLS = new Set(["clustoCell", "markoClust"]);

function longRunHint(tool: string | undefined): string {
  if (tool && CLUSTERING_TOOLS.has(tool)) {
    return "Large datasets can take a while — up to 30 minutes for a full clustering " +
      "pass. You can stop this at any time with the Stop button.";
  }
  return "Large datasets can take a while to process. You can stop this at any time " +
    "with the Stop button.";
}

/** A live 1 Hz clock, but only while something is actually running. */
function useTicker(active: boolean): number {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (!active) return;
    const id = setInterval(() => setNow(Date.now()), 1000);
    return () => clearInterval(id);
  }, [active]);
  return now;
}

function AttemptStrip({ attempts }: { attempts: ToolAttempt[] }) {
  const [open, setOpen] = useState(false);
  const n = attempts.length;
  const label = n === 1 ? "1 earlier attempt didn't finish" : `${n} earlier attempts didn't finish`;
  return (
    <div className="trun-attempts">
      <button type="button" className="trun-attempts-head" onClick={() => setOpen((o) => !o)}
              aria-expanded={open}>
        <span className="trun-attempts-icon" aria-hidden>⟳</span>
        <span>{label}</span>
        <span className="trun-attempts-toggle">{open ? "hide" : "show"}</span>
      </button>
      {open && (
        <ul className="trun-attempts-list">
          {attempts.map((a, i) => (
            <li key={i}>
              <span className="trun-attempt-what">
                {a.outcome === "timeout" ? "Timed out" : a.outcome === "cancelled" ? "Stopped" : "Failed"}
              </span>
              {a.ms != null && <span className="trun-attempt-ms">after {formatElapsed(a.ms)}</span>}
              {a.detail && <div className="trun-attempt-detail">{a.detail}</div>}
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}

// Round LXV Batch 2b (audit #22): the settings a run actually used.
//
// The server has always shipped `arguments` on tool_start; the chat reducer
// dropped them. So the transcript could say THAT clustoCell ran but never with
// WHICH settings -- and "did it use the species/tissue I picked?" was a question
// the product could not answer, which the user asked directly.
//
// Collapsed by default and rendered only when there is something to show: this
// is provenance for whoever wants it, not another thing to read.
export function ToolArgs({ args }: { args?: Record<string, unknown> }) {
  if (!args) return null;
  const keys = Object.keys(args).filter((k) => {
    const v = (args as any)[k];
    return v !== null && v !== undefined && !(Array.isArray(v) && v.length === 0);
  });
  if (!keys.length) return null;
  const fmt = (v: unknown): string => {
    if (Array.isArray(v)) return (v as unknown[]).map(fmt).join(", ");
    if (v && typeof v === "object") return JSON.stringify(v);
    return String(v);
  };
  return (
    <details className="trun-args">
      <summary>Settings used ({keys.length})</summary>
      <dl>
        {keys.map((k) => (
          <div key={k}><dt>{k}</dt><dd>{fmt((args as any)[k])}</dd></div>
        ))}
      </dl>
    </details>
  );
}

// Round LXXIX (audit #59): the rerun affordance.
//
// "run that again with resolution 0.6" had no first-class route. The settings a
// run used were already on the card (Round LXV put them there) but only as
// something to READ -- to change one of them the user had to retype the whole
// request from memory, including the arguments they had just been shown.
//
// This composes the sentence they would have had to write, and the button
// PREFILLS THE COMPOSER rather than sending it. That is the same contract as a
// handle chip, and it is the important half: a rerun whose whole point is to
// change one number must not fire before the user has changed it.
//
// Exported and pure so it is testable without a DOM -- the convention
// splitWarnings() and formatElapsed() already set in this file.
export function rerunPrompt(tool: string, args?: Record<string, unknown>): string {
  const fmt = (v: unknown): string => {
    if (Array.isArray(v)) return (v as unknown[]).map(fmt).join(", ");
    if (v && typeof v === "object") return JSON.stringify(v);
    return String(v);
  };
  const keys = Object.keys(args ?? {}).filter((k) => {
    const v = (args as any)[k];
    return v !== null && v !== undefined && !(Array.isArray(v) && v.length === 0);
  });
  if (!keys.length) return `Run ${tool} again.`;
  // The arguments are restated in full, not summarised. A rerun sentence that
  // silently dropped one would change the analysis while looking like a repeat
  // of it -- and the user's reason for pressing this is that they intend to
  // change exactly one thing.
  return `Run ${tool} again with ` + keys.map((k) => `${k} = ${fmt((args as any)[k])}`).join(", ") + ".";
}

function RerunButton({ tool, args, onRerun }: {
  tool: string; args?: Record<string, unknown>; onRerun?: (text: string) => void;
}) {
  if (!onRerun) return null;
  return (
    <button
      type="button"
      className="trun-rerun"
      onClick={() => onRerun(rerunPrompt(tool, args))}
      title="Put this run's settings in the message box so you can change one and send it again"
    >
      Run again…
    </button>
  );
}

/** The may_invalidate block: always visible, never collapsed. */
function BlockingWarnings({ warnings }: { warnings: CvWarning[] }) {
  if (!warnings.length) return null;
  return (
    <div className="trun-warns" role="note">
      {warnings.map((w, i) => (
        <div className="trun-warn" key={i}>
          <span className="trun-warn-icon" aria-hidden>!</span>
          <span>{w.text}</span>
        </div>
      ))}
    </div>
  );
}

/** The informational notes: collapsed, muted, same shape as ToolArgs. */
function WarningNotes({ warnings }: { warnings: CvWarning[] }) {
  if (!warnings.length) return null;
  return (
    <details className="trun-args trun-notes">
      <summary>What I decided ({warnings.length})</summary>
      <ul>
        {warnings.map((w, i) => <li key={i}>{w.text}</li>)}
      </ul>
    </details>
  );
}

// Round LXXV (D5): `session` is threaded through purely so TableArtifact can
// ask /api/table for page N. ToolRun itself does not use it.
export default function ToolRun({ item, session, onRerun }: {
  item: ToolItemLike; session?: string; onRerun?: (text: string) => void;
}) {
  const running = item.status === "start";
  const now = useTicker(running);
  const elapsedMs = item.startedAt != null
    ? (item.endedAt ?? (running ? now : undefined) ?? item.startedAt) - item.startedAt
    : undefined;
  const elapsed = formatElapsed(elapsedMs);
  const pct = typeof item.progress === "number"
    ? Math.max(0, Math.min(100, Math.round(item.progress)))
    : undefined;

  // Round LXIII: a run the USER stopped is its own outcome. It was previously
  // impossible to reach -- nothing ever moved a card off "start", so a stopped
  // run kept its spinner and live clock for the rest of the session (reported
  // from live use, with the Jobs panel showing "cancelled" beside a card still
  // claiming to run). It is deliberately NOT the error state: pressing Stop is
  // the user getting what they asked for, so this reads neutral, keeps the
  // elapsed time it did run for, and keeps any progress it had reached.
  if (item.status === "stopped") {
    return (
      <div className="trun trun-skipped">
        <div className="trun-head">
          <span className="trun-icon" aria-hidden>⏹</span>
          <span className="trun-name">{item.tool}</span>
          <span className="trun-status">
            stopped{elapsed && <span className="trun-time"> · {elapsed}</span>}
          </span>
        </div>
        {(item.detail || pct != null) && (
          <div className="trun-body">
            {item.detail || "You stopped this run."}
            {pct != null && <span className="trun-pct">{pct}%</span>}
          </div>
        )}
        {item.attempts && item.attempts.length > 0 && <AttemptStrip attempts={item.attempts} />}
        <RerunButton tool={item.tool} args={item.args} onRerun={onRerun} />
      </div>
    );
  }

  if (item.status === "skipped") {
    return (
      <div className="trun trun-skipped">
        <div className="trun-head">
          <span className="trun-icon" aria-hidden>⊘</span>
          <span className="trun-name">{item.tool}</span>
          <span className="trun-status">skipped</span>
        </div>
        {item.detail && <div className="trun-body">{item.detail}</div>}
      </div>
    );
  }

  if (running) {
    const longRun = elapsedMs != null && elapsedMs > REASSURE_AFTER_MS;
    return (
      <div className="trun trun-running" role="status" aria-live="polite">
        <div className="trun-head">
          <span className="trun-spinner" aria-hidden />
          <span className="trun-name">{item.tool}</span>
          <span className="trun-status">
            running{elapsed && <span className="trun-time"> · {elapsed}</span>}
          </span>
        </div>
        <div className="trun-bar" aria-hidden>
          {/* No percent yet -> an indeterminate sweep rather than a bar stuck at
              zero, which reads as "nothing is happening". */}
          <div className={pct == null ? "trun-bar-fill indet" : "trun-bar-fill"}
               style={pct == null ? undefined : { width: `${pct}%` }} />
        </div>
        {(item.note || pct != null) && (
          <div className="trun-body">
            {item.note}
            {pct != null && <span className="trun-pct">{pct}%</span>}
          </div>
        )}
        {longRun && (
          <div className="trun-hint">
            {longRunHint(item.tool)}
          </div>
        )}
      </div>
    );
  }

  if (item.status === "error") {
    // Deliberately NOT red-alarming. A tool that ran out of time on a big
    // dataset is an outcome to act on, not a crash to panic about.
    return (
      <div className="trun trun-failed">
        <div className="trun-head">
          <span className="trun-icon" aria-hidden>!</span>
          <span className="trun-name">{item.tool}</span>
          <span className="trun-status">
            didn't finish{elapsed && <span className="trun-time"> · {elapsed}</span>}
          </span>
        </div>
        {item.detail && <div className="trun-body">{item.detail}</div>}
        {item.attempts && item.attempts.length > 0 && <AttemptStrip attempts={item.attempts} />}
        <RerunButton tool={item.tool} args={item.args} onRerun={onRerun} />
      </div>
    );
  }

  // Round LXIX (audit #25): a run that finished but raised something that may
  // invalidate it is NOT the same outcome as a clean run, and it is not a
  // failure either. It gets its own state: amber rule, amber icon, and the
  // caveat itself rendered above the result rather than pasted onto the end of
  // the summary sentence, which is where it used to live.
  const { blocking, notes } = splitWarnings(item.warnings);
  const warned = item.status === "done_with_warnings" || blocking.length > 0;
  return (
    <div className={warned ? "trun trun-done trun-warned" : "trun trun-done"}>
      <div className="trun-head">
        <span className={warned ? "trun-icon trun-warn-mark" : "trun-icon trun-ok"} aria-hidden>
          {warned ? "!" : "✓"}
        </span>
        <span className="trun-name">{item.tool}</span>
        <span className="trun-status">
          {warned ? "done, with warnings" : "done"}
          {elapsed && <span className="trun-time"> · {elapsed}</span>}
        </span>
      </div>
      <BlockingWarnings warnings={blocking} />
      {item.detail && <div className="trun-body">{item.detail}</div>}
      {item.attempts && item.attempts.length > 0 && <AttemptStrip attempts={item.attempts} />}
      <ToolArgs args={item.args} />
      <WarningNotes warnings={notes} />
      {item.artifact && <PlotArtifact art={item.artifact} />}
      {item.table && <TableArtifact art={item.table} session={session} />}
      <RerunButton tool={item.tool} args={item.args} onRerun={onRerun} />
    </div>
  );
}

/**
 * The gap between a tool finishing and the model's write-up appearing.
 *
 * WHY THIS EXISTS. A turn that calls a tool is two round-trips, not one: the
 * tool returns and its card lands immediately, then the model is called AGAIN
 * to interpret that result, and only then does the prose arrive. Between those
 * two the transcript showed nothing at all. On a fast cloud model that is a
 * couple of seconds of dead air; on a slow local one it is much longer, and in
 * both cases the user has just been given a finished-looking answer and has no
 * way to know more is coming.
 *
 * Deliberately NOT shown when no tool ran. A turn like "hi" is a single
 * round-trip whose reply IS the answer — announcing that results are being
 * interpreted when there are no results would be worse than saying nothing.
 * That is enforced by where this is inserted (only after a tool_result /
 * tool_error), not by anything here.
 *
 * The elapsed clock appears only once the wait stops being instant. Showing
 * "0:01" on a two-second cloud round-trip is noise; showing "0:45" on a local
 * model is the difference between "it is working" and "it has hung".
 */
const SHOW_ELAPSED_AFTER_MS = 5000;

export function InterpretingNote({ since }: { since?: number }) {
  const now = useTicker(true);
  const ms = since != null ? now - since : undefined;
  const show = ms != null && ms > SHOW_ELAPSED_AFTER_MS;
  return (
    <div className="interpreting" role="status" aria-live="polite">
      <span className="interpreting-dots" aria-hidden>
        <span>·</span><span>·</span><span>·</span>
      </span>
      <span>Interpreting the results…</span>
      {show && <span className="interpreting-time">{formatElapsed(ms)}</span>}
    </div>
  );
}
