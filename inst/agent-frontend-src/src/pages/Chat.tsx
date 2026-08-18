import { useEffect, useId, useRef, useState } from "react";
import { pollChat } from "../api/stream";
import { ChatEvent, ClarifyChoice, ClarifyDropdown, ClarifyNumberInput } from "../api/stream";
import Markdown from "../components/Markdown";
// Round LXVIII (audit #34): CvApiError is imported as a VALUE (instanceof), not
// a type, so the two test stub lists that fake this module -- in
// test-chat-tool-error-display-safe.R and test-round63-stopped-card-safe.R --
// must both provide it, or the transpiled module fails to load and those files
// report errors instead of results. That is the exact breakage Round XLIII
// caused with LocalModelWarning and Round XLVII documented at the stub list.
import { api, CvApiError, ObjectDescriptor, Settings } from "../api/client";
import type { Page } from "../App";
import Onboarding from "../components/Onboarding";
import LocalModelWarning from "../components/LocalModelWarning";
import ToolRun, { InterpretingNote, doneStatusFor, type ToolAttempt } from "../components/ToolRun";
import ObjectRail from "../components/ObjectRail";
import PromptRail from "../components/PromptRail";
import ErrorDetailC from "../components/ErrorDetail";
import type { CvWarning } from "../api/stream";

// A single rendered chat item. Exported because the conversation state now
// lives in App.tsx (so it survives tab switches) and is passed in as props.
export type ChatItem =
  | { kind: "user"; text: string }
  // Round LXIV (D6): `detail` carries the technical cause from the server's
  // error envelope. Optional and almost always absent -- it is set only on the
  // two error paths below, and ErrorDetail renders nothing without it, so an
  // ordinary assistant message is completely unchanged.
  // Round LXVIII (audit #34): `failed` marks a message that reports a failure
  // rather than an answer. It replaces the two "⚠ " string prefixes that used
  // to do this job by welding a glyph onto the server's own sentence -- which
  // made every failure read as an alarm and put the marker in a different voice
  // from the calm text Round LXII wrote. Styling belongs in CSS, not in the
  // message body.
  | { kind: "assistant"; text: string; detail?: string; failed?: boolean }
  // Round LI: `phase` distinguishes the turn-start "thinking…" placeholder from
  // the post-tool "interpreting the results…" note. Both are cleared by the
  // same clearThinking(), so every existing clear path keeps working.
  | { kind: "thinking"; phase?: "interpreting"; since?: number }
  | { kind: "clarification"; text: string; tool?: string; choices?: ClarifyChoice[]; dropdowns?: ClarifyDropdown[]; inputs?: ClarifyNumberInput[]; note?: string; resume_template?: string; base_request?: string }
  // Round XLVII: a tool item is now the WHOLE life of one tool call, not one
  // line per event. `progress`/`note` are fed by the worker's progress
  // events (previously dropped on the floor); `startedAt`/`endedAt` drive the
  // elapsed clock; `attempts` holds earlier tries that did not finish, folded
  // in so a retry that eventually worked reads as one slow step rather than a
  // stack of red failures.
  | { kind: "tool"; tool: string;
      // Round LXIX (audit #25): `done_with_warnings` is a THIRD terminal
      // outcome, not a flavour of done and not a failure. The run produced a
      // result AND raised something that may change what that result means.
      status: "start" | "done" | "done_with_warnings" | "error" | "skipped" | "stopped";
      // Round LXV (audit #22): the arguments the call actually started with.
      detail?: string; artifact?: any; table?: any; args?: Record<string, unknown>;
      // Round LXIX (audit #23/#24): the caveats the call raised, two severities.
      warnings?: CvWarning[];
      progress?: number; note?: string; startedAt?: number; endedAt?: number;
      attempts?: ToolAttempt[] };

// Merge a successful tool_result into the rendered item list. Exported as a
// pure function (no React state) so it can be tested directly in isolation --
// same pattern as Settings.tsx's reconcileModelForProvider().
//
// Round XLVII changed the POLICY here, not the plumbing. Earlier failed
// attempts at the same tool used to be DELETED once the tool finally succeeded:
// the transcript came out clean, but a run that had burned two 30-minute
// timeouts looked instantaneous and the user had no way to find out why an hour
// had passed. Deleting the evidence is not the same as removing the noise.
//
// They are now FOLDED INTO the result card as `attempts` and rendered as one
// quiet collapsed line ("2 earlier attempts didn't finish · show"). The red
// standalone rows are gone -- which was the real complaint, since a stack of
// them reads as "the agent failed" even when it succeeded -- while the history
// stays recoverable.
//
// Scoped to items since the last user message, so an earlier TURN's history is
// never touched, and only ever to THIS exact tool: a normal, never-failed call
// is completely unaffected.
export function mergeToolResultItem(prev: ChatItem[], toolName: string, doneItem: ChatItem): ChatItem[] {
  let cutoff = 0;
  for (let i = prev.length - 1; i >= 0; i--) {
    if (prev[i].kind === "user") { cutoff = i + 1; break; }
  }
  const attempts: ToolAttempt[] = [];
  const kept: ChatItem[] = [];
  prev.forEach((it, i) => {
    const mine = i >= cutoff && it.kind === "tool" && (it as any).tool === toolName;
    if (mine && ((it as any).status === "error" || (it as any).status === "start")) {
      if ((it as any).status === "error") {
        const detail: string = (it as any).detail || "";
        const started = (it as any).startedAt;
        const ended = (it as any).endedAt;
        attempts.push({
          outcome: /timed?\s*out|exceeded the timeout/i.test(detail)
            ? "timeout"
            : /cancel/i.test(detail) ? "cancelled" : "error",
          detail,
          ms: started != null && ended != null ? ended - started : undefined,
        });
      }
      return;   // folded into the result card below
    }
    kept.push(it);
  });
  const merged: ChatItem = attempts.length
    ? ({ ...(doneItem as any), attempts } as ChatItem)
    : doneItem;
  return [...kept, merged];
}

// Round LXXIX (audit #59): the items belonging to the turn currently in flight
// — everything after the last user message.
//
// This is the test the undo affordance is gated on: if any of them is a tool
// card, the turn has started doing real work on the server and an "undo" that
// erased the transcript would be hiding it rather than reversing it. Kept as a
// pure exported function, above the SearchableSelect cut point the three Node
// harnesses slice at, so the boundary can be asserted directly instead of
// through a rendered card.
export function itemsSinceLastUser(items: ChatItem[]): ChatItem[] {
  for (let i = items.length - 1; i >= 0; i--) {
    if (items[i].kind === "user") return items.slice(i + 1);
  }
  return items.slice();
}

// Round LXXXI (E1/E2): the width at or above which a rail is open by default.
//
// 1180 is not arbitrary: it is the max-width `.messages` has been capped at
// since Round LXXIX's #52, i.e. the point past which the conversation column
// stops growing and the extra pixels start being the empty space this round is
// filling. Below it the rails would be taking width FROM the conversation
// instead of using width it cannot use -- and the audit's own note on #52
// records that ~720px, beside RStudio, is the normal posture here.
export const RAIL_AUTO_OPEN_PX = 1180;

// Should a rail start open, given a stored preference and a window width?
//
// A STORED PREFERENCE ALWAYS WINS, at any width: someone who closed a rail on a
// wide monitor meant it, and someone who opened one on a narrow window meant
// that too. Only the absent case consults the width. Exported and pure so the
// rule can be driven directly rather than inferred from a rendered panel.
export function railInitiallyOpen(stored: string | null, width: number): boolean {
  if (stored === "1") return true;
  if (stored === "0") return false;
  return width >= RAIL_AUTO_OPEN_PX;
}

// Round LXIII: no tool card may outlive the turn that opened it.
//
// THE BUG THIS FIXES, reported from live use: pressing Stop left the card
// rendering `running · 2:55` with a live clock forever -- through later turns,
// while the Jobs panel beside it correctly read "cancelled". A card is
// "running" purely because its status is still "start", and nothing ever moved
// it off that. tool_result/tool_error close the cards they know about; a turn
// that ended any OTHER way closed none.
//
// Applied on EVERY terminal event (cancelled/done/error) rather than only on
// cancel, because the defect is structural: the transcript must not be able to
// claim work is in progress once the turn is over, whatever ended it.
// `endedAt` is stamped so the elapsed clock freezes at the real duration
// instead of ticking on or resetting to zero.
//
// Module-scope and exported, with `now` injectable, so it is testable as a pure
// function -- the same convention as mergeToolResultItem() above and
// reconcileModelForProvider() in Settings.tsx.
export function closeOpenToolCards(
  prev: ChatItem[],
  status: "stopped" | "error",
  detail?: string,
  now: number = Date.now()
): ChatItem[] {
  return prev.map((it) =>
    it.kind === "tool" && it.status === "start"
      ? { ...it, status, endedAt: it.endedAt ?? now, detail: it.detail || detail }
      : it
  );
}

// Round LXVIII (audit #32): the note a queued heavy job puts on its card.
//
// cv_launch_heavy()'s admission control emits `job_queued` with a `reason`
// separating the two ways a job waits: "pool" (every worker slot is busy) and
// "memory" (free system RAM is under heavy_job_min_free_mb, so an ADDITIONAL
// concurrent job is held rather than spawned). Both reached the browser and
// fell through the reducer's `default:`, so the Jobs panel knew the run was
// queued while the transcript -- where the user is actually looking -- showed a
// card at 0% with no explanation.
//
// Keeping the two apart is the point: a slot frees itself in seconds, memory
// may mean closing something else. Exported and pure, the same convention as
// closeOpenToolCards() below, so the behaviour is testable without a DOM.
export function queuedNote(ev: any): string {
  return ev?.reason === "memory"
    ? `Waiting for memory before starting (${ev?.available_mb ?? "?"} MB free, ${ev?.min_free_mb ?? "?"} MB needed).`
    : `Waiting for a free worker slot (${ev?.running ?? "?"}/${ev?.pool_size ?? "?"} running).`;
}

/**
 * Put that note on the running card for this tool.
 *
 * Updates in place and never appends -- exactly like `progress`. A job held for
 * a minute is re-emitted on every admission retry, so appending would print a
 * row per poll. Returns `prev` untouched when there is no open card to update,
 * so a late or duplicate event cannot invent one.
 */
export function applyQueuedNote(prev: ChatItem[], ev: any): ChatItem[] {
  const tool = ev?.tool;
  const note = queuedNote(ev);
  for (let i = prev.length - 1; i >= 0; i--) {
    const it = prev[i];
    if (it.kind === "tool" && it.status === "start" && (!tool || it.tool === tool)) {
      const copy = prev.slice();
      copy[i] = { ...it, note };
      return copy;
    }
  }
  return prev;
}

/**
 * Round LXVIII (audit #34): the item shown when the TRANSPORT fails.
 *
 * This branch fires when the fetch itself never completed, so there is no calm
 * sentence coming from R and the client has to supply one. It used to print
 * `⚠ error: ${e.message}` -- a raw JavaScript exception, verbatim, in the
 * transcript. "Failed to fetch" tells the user nothing they can act on; the
 * thing they CAN act on is that the R session may have stopped.
 *
 * A CvApiError is different in kind: the server did answer, with a sentence
 * already written in the approved voice, so that sentence is used as-is and
 * its own `detail` carries the cause. Everything else keeps the raw message,
 * but in `detail` where the contract puts a technical cause -- not in the body.
 */
export function transportErrorItem(e: any): ChatItem {
  const isApi = e instanceof CvApiError;
  return {
    kind: "assistant",
    failed: true,
    text: isApi
      ? e.message
      : "The connection to the agent dropped before this turn finished. Check that the R session is still running, then ask again.",
    detail: e?.detail || (isApi ? undefined : e?.message) || undefined,
  };
}

// Round LXXXII: ErrorDetail moved to components/ErrorDetail.tsx so the Data /
// Upload page can use the same toggle. It is imported at the top of this file
// as ErrorDetailC and used in the transcript below.
//
// It is deliberately NOT re-exported from here. The first attempt kept a
// `export { default as ErrorDetail } from "../components/ErrorDetail"` for
// compatibility, and the full suite reported errors from all four Node
// transpile runners: tsc emits a re-export as `export ... from "..."`, which is
// an import that the stub lists -- which match `import ... from "..."` -- cannot
// see, so every one of those modules failed to load. Nothing consumed the
// re-export, so it is gone rather than special-cased.

// ---------------------------------------------------------------------------
// Tissue/Condition dropdowns (markerDB annotation path)
//
// IMPORTANT: these components are defined at MODULE scope, NOT inside Chat.
// A component defined inside another component gets a new function identity on
// every parent render, so React unmounts + remounts it each render — wiping its
// local state (open list, typed query, selections). That was the Round XIX bug:
// the dropdowns vanished and cleared the instant you interacted with them.
// Module-scope definitions keep a stable identity, so state survives re-renders.
// ---------------------------------------------------------------------------

// Round LXXIX: the initial dropdown selection, seeded from the server's prefill.
//
// Pulled out of DropdownGroup and placed ABOVE the SearchableSelect cut point
// the three Node harnesses slice at, so the thing that actually broke -- a
// server field with no client reading it -- can be asserted by running this,
// rather than by grepping a component that needs a DOM.
//
// `value` is only ever a member of that dropdown's own `options` (the server
// matches against the package's vocabulary for the species), but the membership
// test is applied here anyway: this card must not be able to DISPLAY a
// selection its own list cannot offer, whatever a future payload does.
export function seedSelection(dd: ClarifyDropdown[] | undefined | null): Record<string, string> {
  const init: Record<string, string> = {};
  for (const d of dd ?? []) {
    if (d && typeof d.value === "string" && d.value && (d.options ?? []).includes(d.value)) {
      init[d.id] = d.value;
    }
  }
  return init;
}

// How many filtered options the list actually renders. Hoisted out of the JSX
// because Round LXXIX's keyboard navigation must move over the SAME rows the
// user can see: navigating the unsliced `filtered` would let ArrowDown walk off
// the end of the rendered list into options with no DOM node, so Enter would
// select something invisible and scrollIntoView would find nothing.
const SEARCH_SELECT_MAX_ROWS = 200;

// A lightweight searchable combo box: a text input that filters a drop-down
// list. Typing narrows the options; clicking one selects it. Keeps 265-value
// lists (tissues/conditions) usable where plain chips would not fit.
//
// Round LXXIX (audit #50): it is now operable from the keyboard. It was
// mouse-only -- the list opened on focus, so a keyboard user could reach the
// input and see 397 tissues, and had no key that would select one. Arrow keys
// scrolled the page behind the list instead. This is the ARIA 1.2 combobox
// pattern, implemented with the roles rather than approximated: `role=combobox`
// on the input owning a `role=listbox`, `aria-activedescendant` naming the
// focused option, and DOM focus staying in the text box the whole time so
// typing keeps filtering while the arrows move the highlight.
export function SearchableSelect({ label, options, value, disabled, onChange }:{
  label: string; options: string[]; value: string; disabled?: boolean; onChange: (v: string) => void;
}) {
  const [open, setOpen] = useState(false);
  const [query, setQuery] = useState("");
  // -1 means "no option highlighted": the state a freshly opened list is in, so
  // Enter on it does nothing rather than silently committing options[0].
  const [active, setActive] = useState(-1);
  const wrapRef = useRef<HTMLDivElement>(null);
  const listRef = useRef<HTMLDivElement>(null);
  const uid = useId();
  const inputId = `ss-${uid}`;
  const listId = `ss-list-${uid}`;
  const optId = (i: number) => `ss-opt-${uid}-${i}`;
  const shown = (value && !open) ? value : query;
  const filtered = query
    ? options.filter((o) => o.toLowerCase().includes(query.toLowerCase()))
    : options;
  const visible = filtered.slice(0, SEARCH_SELECT_MAX_ROWS);
  useEffect(() => {
    function onDoc(e: MouseEvent) {
      if (wrapRef.current && !wrapRef.current.contains(e.target as Node)) setOpen(false);
    }
    document.addEventListener("mousedown", onDoc);
    return () => document.removeEventListener("mousedown", onDoc);
  }, []);
  // Keep the highlighted row on screen. A 397-option list is ~20 screens tall,
  // so an ArrowDown that moves an offscreen highlight is indistinguishable from
  // a key that did nothing.
  useEffect(() => {
    if (!open || active < 0 || !listRef.current) return;
    const el = listRef.current.querySelector(`#${CSS.escape(optId(active))}`) as HTMLElement | null;
    el?.scrollIntoView({ block: "nearest" });
  }, [active, open]);

  function commit(o: string) {
    onChange(o);
    setOpen(false);
    setQuery("");
    setActive(-1);
  }

  function onKeyDown(e: React.KeyboardEvent<HTMLInputElement>) {
    if (disabled) return;
    // Escape closes without changing the selection, from anywhere. Deliberately
    // handled before the open check so it also clears a stale query.
    if (e.key === "Escape") {
      if (open || query) { e.preventDefault(); setOpen(false); setQuery(""); setActive(-1); }
      return;
    }
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      e.preventDefault();          // or the page scrolls behind the open list
      if (!open) { setOpen(true); setActive(e.key === "ArrowDown" ? 0 : visible.length - 1); return; }
      if (!visible.length) return;
      setActive((i) => {
        const next = e.key === "ArrowDown" ? i + 1 : i - 1;
        // Wrap, so a long list is reachable from either end without holding a key.
        if (next >= visible.length) return 0;
        if (next < 0) return visible.length - 1;
        return next;
      });
      return;
    }
    if (e.key === "Home" && open && visible.length) { e.preventDefault(); setActive(0); return; }
    if (e.key === "End" && open && visible.length) { e.preventDefault(); setActive(visible.length - 1); return; }
    if (e.key === "Enter") {
      // Only swallow Enter when it actually selects something. Otherwise it
      // must fall through -- this control sits inside a card whose Continue
      // button is the natural Enter target once the fields are filled.
      if (open && active >= 0 && active < visible.length) { e.preventDefault(); commit(visible[active]); }
      return;
    }
    if (e.key === "Tab" && open) { setOpen(false); setActive(-1); }
  }

  return (
    <div className="search-select" ref={wrapRef}>
      <label className="search-select-label" htmlFor={inputId}>{label}</label>
      <input
        id={inputId}
        type="text"
        className="search-select-input"
        placeholder={`Select ${label.toLowerCase()}…`}
        value={shown}
        disabled={disabled}
        role="combobox"
        aria-expanded={open && !disabled}
        aria-controls={listId}
        aria-autocomplete="list"
        aria-activedescendant={open && active >= 0 && active < visible.length ? optId(active) : undefined}
        onFocus={() => { setOpen(true); setQuery(""); setActive(-1); }}
        onKeyDown={onKeyDown}
        onChange={(e) => { setQuery(e.target.value); setOpen(true); setActive(-1); }}
      />
      {open && !disabled && (
        <div className="search-select-list" id={listId} role="listbox" ref={listRef} aria-label={label}>
          {visible.length === 0 && <div className="search-select-empty">no matches</div>}
          {visible.map((o, i) => (
            <button
              key={o}
              id={optId(i)}
              type="button"
              role="option"
              aria-selected={o === value}
              // -1: the arrow keys drive this list, so the options must not also
              // be in the Tab order -- Tab past the box would otherwise mean 397
              // stops before the next field.
              tabIndex={-1}
              className={"search-select-opt" + (o === value ? " selected" : "") + (i === active ? " active" : "")}
              // Mouse and keyboard converge on the same highlight, so a click
              // after an arrow press cannot select a different row than the one
              // under the pointer.
              onMouseEnter={() => setActive(i)}
              onClick={() => commit(o)}
            >
              {o}
            </button>
          ))}
        </div>
      )}
    </div>
  );
}

// A group of dropdowns (e.g. Tissue + Condition) plus a `resume_template` with
// {id} placeholders. The user picks a value for every dropdown, then presses an
// explicit Continue button to fire the composed resume message. (Round XX: an
// explicit button replaces the Round XIX auto-send, per user preference.)
export function DropdownGroup({ dd, template, busy, onFire, inputs, note }:{
  dd: ClarifyDropdown[]; template: string; busy: boolean; onFire: (msg: string) => void;
  inputs?: ClarifyNumberInput[]; note?: string;
}) {
  // Round LXXIX: seeded from each dropdown's server-supplied `value`, not from
  // {}. Round LXXVIII computed those prefills and nothing here read them, so a
  // card that said "Filled in from your message: tissue, n" showed an empty
  // Tissue box -- the defect the user reported from live use.
  //
  // Lazy initializer, so the seed is taken ONCE when the card mounts. A plain
  // useState(seedFrom(dd)) would recompute on every keystroke in a sibling
  // field and, worse, a later re-render could clobber a correction the user had
  // just made back to the server's guess.
  //
  // `value` is only ever a member of that dropdown's own `options` (the server
  // matches against the package vocabulary), but the membership test is applied
  // here anyway: this component must not be able to display a selection its own
  // list cannot offer, whatever a future payload does.
  const [sel, setSel] = useState<Record<string, string>>(() => seedSelection(dd));
  // Numeric input values, keyed by id, prefilled to each input's default.
  const [nums, setNums] = useState<Record<string, string>>(() => {
    const init: Record<string, string> = {};
    for (const inp of inputs ?? []) init[inp.id] = inp.default != null ? String(inp.default) : "";
    return init;
  });
  const [fired, setFired] = useState(false);
  // Unique per rendered card, so two clarification cards in one scrollback
  // cannot mint the same input id and steal each other's label association.
  const gid = useId();

  function compose(): string {
    let out = template;
    for (const d of dd) out = out.split(`{${d.id}}`).join(sel[d.id] ?? "");
    for (const inp of inputs ?? []) {
      // Round LXIV: a blank TEXT input means "use the default" (the species
      // field says so on the card). Substituting the default here keeps the
      // sent message readable -- "species=human" rather than "species=" -- and
      // the server treats both the same way regardless.
      const raw = (nums[inp.id] ?? "").trim();
      const val = raw || (inp.type === "text" ? String(inp.default ?? "") : (nums[inp.id] ?? ""));
      out = out.split(`{${inp.id}}`).join(val);
    }
    return out;
  }
  function pick(id: string, value: string) {
    setSel((prev) => ({ ...prev, [id]: value }));
  }
  function pickNum(id: string, value: string) {
    setNums((prev) => ({ ...prev, [id]: value }));
  }
  // A numeric input is valid when it is a positive integer >= its min.
  //
  // Round LXIV: a TEXT input (the LLM path's species field) is always valid,
  // including when blank -- the user was told they may leave it empty, and
  // compose() substitutes the default for them. Requiring a value here would
  // contradict the instruction on the card.
  function numValid(inp: ClarifyNumberInput): boolean {
    if (inp.type === "text") return true;
    const raw = (nums[inp.id] ?? "").trim();
    if (!raw) return false;
    const v = Number(raw);
    if (!Number.isInteger(v)) return false;
    return v >= (inp.min ?? 1);
  }
  const ddComplete = dd.every((d) => (sel[d.id] ?? "").length > 0);
  const numComplete = (inputs ?? []).every(numValid);
  const complete = ddComplete && numComplete;
  function go() {
    if (!complete || fired || busy) return;
    setFired(true);
    onFire(compose());
  }
  return (
    <div className="clarify-dropdowns-wrap">
      <div className="clarify-dropdowns">
        {dd.map((d) => (
          <SearchableSelect
            key={d.id}
            label={d.label}
            options={d.options}
            value={sel[d.id] ?? ""}
            disabled={busy || fired}
            onChange={(v) => pick(d.id, v)}
          />
        ))}
        {(inputs ?? []).map((inp) => (
          <div className="search-select" key={inp.id}>
            {/* Round LXXIX (audit #48): htmlFor/id. Without it a screen reader
                announces this as an unlabelled edit field, and clicking the
                word "Top markers (n)" does not focus the box. */}
            <label className="search-select-label" htmlFor={`ci-${gid}-${inp.id}`}>{inp.label}</label>
            {inp.type === "text" ? (
              <input
                id={`ci-${gid}-${inp.id}`}
                type="text"
                className="search-select-input"
                placeholder={inp.placeholder ?? String(inp.default ?? "")}
                value={nums[inp.id] ?? ""}
                disabled={busy || fired}
                onChange={(e) => pickNum(inp.id, e.target.value)}
              />
            ) : (
              <input
                id={`ci-${gid}-${inp.id}`}
                type="number"
                className="search-select-input"
                min={inp.min ?? 1}
                step={1}
                value={nums[inp.id] ?? ""}
                disabled={busy || fired}
                onChange={(e) => pickNum(inp.id, e.target.value)}
              />
            )}
          </div>
        ))}
      </div>
      {typeof note === "string" && note.length > 0 && <div className="clarify-note">{note}</div>}
      <button
        type="button"
        className="clarify-continue"
        disabled={!complete || busy || fired}
        onClick={go}
      >
        {fired ? "Continuing…" : "Continue"}
      </button>
    </div>
  );
}

interface Props {
  session: string;
  onArtifact?: (a: any) => void;
  onStateChange: () => void;
  // Controlled conversation state (owned by App so it persists across tabs).
  items: ChatItem[];
  setItems: React.Dispatch<React.SetStateAction<ChatItem[]>>;
  busy: boolean;
  setBusy: React.Dispatch<React.SetStateAction<boolean>>;
  onNavigate?: (p: Page) => void;
  // Round LXXXI (E1): the session's objects, rendered in the right-hand rail.
  // They are owned by App (it polls /api/objects every 4s and the Logs page
  // needs the jobs from the same poll), so they arrive as a prop rather than
  // being fetched a second time here.
  objects?: ObjectDescriptor[];
}

// Map server-side session history (role/content records) into renderable items.
// Used to rehydrate the chat after a page reload. Tool-call/tool-result plumbing
// is summarized compactly; full artifacts re-appear as the user continues.
export function historyToItems(history: any[]): ChatItem[] {
  const out: ChatItem[] = [];
  for (const m of history) {
    const role = m?.role;
    const content = typeof m?.content === "string" ? m.content : "";
    if (role === "user") out.push({ kind: "user", text: content });
    else if (role === "assistant" && content) out.push({ kind: "assistant", text: content });
    // "tool"/"system" messages are internal plumbing; skip for a clean transcript.
  }
  return out;
}

export default function Chat({ session, onArtifact, onStateChange, items, setItems, busy, setBusy, onNavigate, objects }: Props) {
  const [input, setInput] = useLocalInput();
  // Round LXXXI: whether each rail is showing. Remembered per browser -- this
  // is a view preference, not user content, so localStorage is the right place
  // for it (the prompts themselves are on the server; see PromptRail.tsx).
  const [leftOpen, setLeftOpen] = useStickyFlag("cv_rail_prompts");
  const [rightOpen, setRightOpen] = useStickyFlag("cv_rail_objects");
  const abortRef = useRef<AbortController | null>(null);
  const endRef = useRef<HTMLDivElement | null>(null);
  const [settings, setSettings] = useState<Settings | null>(null);
  // Round LXXIX (audit #59): the label of the clarification chip that started
  // the turn in flight, or null. Set only by a chip click, so a typed message
  // never offers an undo it could not honestly deliver.
  const [pendingChip, setPendingChip] = useState<string | null>(null);
  // Round LXXX (audit #60/#61/#62/#63): the first-screen content, from R.
  const [intro, setIntro] = useState<{ examples: { label: string; text: string }[];
                                       formats: string[]; can_do: string[] } | null>(null);
  const [demoBusy, setDemoBusy] = useState(false);
  const [demoMsg, setDemoMsg] = useState("");

  useEffect(() => { endRef.current?.scrollIntoView({ behavior: "smooth" }); }, [items]);

  // Load settings once so the onboarding card/banner can reflect the live
  // provider + whether an OpenRouter key is already configured.
  useEffect(() => {
    api.getSettings().then(setSettings).catch(() => setSettings(null));
    // Round LXXX: the first-screen content. A failure leaves `intro` null and
    // the empty state renders without the chips rather than not at all.
    api.intro().then(setIntro).catch(() => setIntro(null));
  }, []);

  // Round LXXX (audit #63): load the bundled demo dataset.
  //
  // It does NOT auto-send a first message afterwards. The user has just been
  // shown seven things to try; choosing one of them is the point, and picking
  // for them would waste the choice and spend their tokens on it.
  async function loadDemo() {
    if (!session || demoBusy) return;
    setDemoBusy(true); setDemoMsg("");
    try {
      const r = await api.loadDemo(session);
      setDemoMsg(r?.note || `Loaded ${r?.handle}.`);
      onStateChange();
    } catch (e: any) {
      setDemoMsg(e?.message || "The demo dataset could not be loaded.");
    } finally { setDemoBusy(false); }
  }

  function push(it: ChatItem) { setItems((prev) => [...prev, it]); }

  // Remove any transient "thinking" placeholder (used before first real event).
  function clearThinking(prev: ChatItem[]): ChatItem[] {
    return prev.filter((it) => it.kind !== "thinking");
  }

  function appendAssistant(text: string) {
    setItems((prev) => {
      const base = clearThinking(prev);
      const last = base[base.length - 1];
      if (last && last.kind === "assistant") {
        const copy = base.slice();
        copy[copy.length - 1] = { ...last, text: last.text + text };
        return copy;
      }
      return [...base, { kind: "assistant", text }];
    });
  }

  // Insert a clicked handle into the composer so the user can send it without
  // typing. If the composer already has text, append; otherwise prefill a
  // natural "use this object" instruction referencing the handle.
  function pickHandle(handle: string) {
    setInput((prev) => {
      const cur = prev.trim();
      if (!cur) return `use ${handle}`;
      return cur.includes(handle) ? prev : `${prev} ${handle}`;
    });
  }

  function handleEvent(ev: ChatEvent) {
    switch (ev.type) {
      case "thinking":
        // Show an immediate placeholder so the user sees instant feedback.
        setItems((prev) => (prev.some((it) => it.kind === "thinking") ? prev : [...prev, { kind: "thinking" }]));
        break;
      case "token":
        appendAssistant((ev as any).text ?? "");
        break;
      case "assistant":
        setItems((prev) => {
          const base = clearThinking(prev);
          const last = base[base.length - 1];
          if (last && last.kind === "assistant") {
            const copy = base.slice();
            copy[copy.length - 1] = { kind: "assistant", text: (ev as any).text ?? last.text };
            return copy;
          }
          return [...base, { kind: "assistant", text: (ev as any).text ?? "" }];
        });
        break;
      case "tool_start":
        // Round LXV Batch 2b (audit #22): keep the arguments. The server has
        // always shipped them on tool_start and the reducer dropped them, so a
        // user could see THAT clustoCell ran but never with WHICH settings --
        // the single most common "did it use what I chose?" question, and one
        // the transcript could not answer.
        setItems((prev) => [...clearThinking(prev),
          { kind: "tool", tool: (ev as any).tool, status: "start", startedAt: Date.now(),
            args: (ev as any).arguments }]);
        break;
      // Round XLVII: the worker emits these ~every 400 ms with a percent and its
      // own status line. The chat reducer used to have no case for them at all,
      // so they were delivered and discarded and a long run looked frozen. They
      // update the RUNNING card in place -- never append a new item, or a
      // half-hour job would print a hundred rows.
      case "progress": {
        const tool = (ev as any).tool;
        const progress = (ev as any).progress;
        const note = (ev as any).message;
        setItems((prev) => {
          for (let i = prev.length - 1; i >= 0; i--) {
            const it = prev[i];
            if (it.kind === "tool" && it.status === "start" && (!tool || it.tool === tool)) {
              const copy = prev.slice();
              copy[i] = { ...it,
                progress: typeof progress === "number" ? progress : it.progress,
                note: note || it.note };
              return copy;
            }
          }
          return prev;
        });
        break;
      }
      // Round LXVIII (audit #32): a heavy tool that cannot start yet.
      //
      // cv_launch_heavy()'s admission control emits this with a `reason`
      // distinguishing the two ways a job waits -- "pool" (every worker slot is
      // busy) and "memory" (free system RAM is below heavy_job_min_free_mb, so
      // an ADDITIONAL concurrent job is held rather than spawned). Both reached
      // the browser and fell through the reducer's `default:`, so the Jobs panel
      // knew a run was queued and the transcript, which is where the user is
      // actually looking, showed a card sitting at 0% with no explanation.
      //
      // The distinction is worth keeping: waiting for a slot clears itself in
      // seconds, waiting for memory may mean closing something. Updates the
      // running card in place -- like `progress`, never appending, or a job
      // held for a minute would print a row every poll.
      case "job_queued":
        setItems((prev) => applyQueuedNote(prev, ev));
        break;
      case "tool_result": {
        const res = (ev as any).result || {};
        // A guard-refused REPEAT (the loop's terminal-success / object re-run
        // guards) arrives with result.repeated = TRUE: the tool did NOT run
        // again. These internal guard events are HIDDEN from the user entirely
        // (a "skipped — already done" line is redundant noise); the guard still
        // works server-side to prevent loops. Also drop the matching tool_start
        // line so no orphan "running …" row is left behind.
        if (res.repeated === true) {
          setItems((prev) => {
            const copy = prev.slice();
            for (let i = copy.length - 1; i >= 0; i--) {
              const it = copy[i];
              if (it.kind === "tool" && it.tool === (ev as any).tool && it.status === "start") {
                copy.splice(i, 1);
                break;
              }
            }
            return copy;
          });
          break;
        }
        const artifact = res.artifact?.kind === "plot" ? res.artifact : undefined;
        const table = res.table_artifact;
        if (artifact) onArtifact?.(artifact);
        const toolName = (ev as any).tool;
        // Carry the running card's start time across so the finished card can
        // report how long the step actually took.
        let startedAt: number | undefined;
        let startArgs: Record<string, unknown> | undefined;
        setItems((prev) => {
          for (let i = prev.length - 1; i >= 0; i--) {
            const it = prev[i];
            if (it.kind === "tool" && it.status === "start" && it.tool === toolName) {
              startedAt = (it as any).startedAt;
              // Round LXV (audit #22): carry the arguments across. They arrive
              // on tool_start and the finished card is built fresh, so without
              // this the settings would be visible only while the run was in
              // flight -- which is precisely when nobody is reading them.
              startArgs = (it as any).args; break;
            }
          }
          return prev;
        });
        // Round LXIX (audit #23/#24/#25): the caveats now arrive as a typed
        // list on the result instead of being pasted onto the end of the
        // summary sentence, so the card can tell a results-invalidating one
        // from "one cluster had no hits" -- which the audit named as the exact
        // pair that used to read identically under the same green tick.
        //
        // doneStatusFor() is the single place that decides which, shared with
        // the renderer, so the card state and the rendering cannot disagree.
        const warnings: CvWarning[] = Array.isArray(res.warnings) ? res.warnings : [];
        const doneItem: ChatItem = {
          kind: "tool",
          tool: toolName,
          status: doneStatusFor(warnings),
          detail: (ev as any).summary || res.text,
          artifact,
          table,
          args: startArgs,
          warnings,
          startedAt,
          endedAt: Date.now(),
        };
        // Round LI: the tool is done, but the turn is not — the model is about to
        // be called again to write up what the tool returned. Say so, or the
        // user reads a finished-looking result and then watches nothing happen.
        // Cleared by clearThinking() on the very next real event (token,
        // assistant, another tool_start) and by "done" below, so it cannot stick.
        setItems((prev) => [...mergeToolResultItem(prev, toolName, doneItem),
                            { kind: "thinking", phase: "interpreting", since: Date.now() }]);
        onStateChange();
        break;
      }
      case "tool_error":
        // Convert the RUNNING card for this tool rather than pushing a second
        // row. Previously the "▶ running x…" line stayed put and a red "✗ x: …"
        // was appended under it, so every failure showed up twice.
        setItems((prev) => {
          for (let i = prev.length - 1; i >= 0; i--) {
            const it = prev[i];
            if (it.kind === "tool" && it.status === "start" && it.tool === (ev as any).tool) {
              const copy = prev.slice();
              copy[i] = { ...it, status: "error", detail: (ev as any).error, endedAt: Date.now() };
              return copy;
            }
          }
          return [...clearThinking(prev),
            { kind: "tool", tool: (ev as any).tool, status: "error",
              detail: (ev as any).error, endedAt: Date.now() }];
        });
        // A failure is also handed back to the model, which decides whether to
        // retry or explain. The wait is the same, so the note is the same.
        setItems((prev) => [...prev, { kind: "thinking", phase: "interpreting", since: Date.now() }]);
        break;
      case "clarification":
        // The agent is unsure which object to use (or stalled): show its
        // markdown list plus clickable handle chips so the user can pick one
        // without typing the handle.
        setItems((prev) => [
          ...clearThinking(prev),
          {
            kind: "clarification",
            text: (ev as any).text ?? "",
            tool: (ev as any).tool,
            choices: (ev as any).choices,
            dropdowns: (ev as any).dropdowns,
            inputs: (ev as any).inputs,
            note: (ev as any).note,
            resume_template: (ev as any).resume_template,
            base_request: (ev as any).base_request,
          },
        ]);
        break;
      // Round LI: the turn is over. Any lingering thinking/interpreting note must
      // go, even on paths that end WITHOUT an assistant message — otherwise the
      // last thing on screen is an indicator promising a reply that never comes.
      case "done":
        // A turn can finish with a tool card still open -- an iteration cap, a
        // guard refusal, a model that stopped calling tools. Closing them as
        // "stopped" is honest: the run did not report a result.
        setItems((prev) => closeOpenToolCards(clearThinking(prev), "stopped",
                                              "This run did not report a result."));
        break;
      case "cancelled":
        setItems((prev) => [
          ...closeOpenToolCards(clearThinking(prev), "stopped", "You stopped this run."),
          // Round LXXX (audit #90): the ⏹ is gone from the TEXT. The stopped
          // tool card beside this already renders one (ToolRun.tsx), so the
          // transcript showed the same glyph twice for one action; and
          // "whenever you're ready" is filler that says nothing the reader did
          // not already know.
          { kind: "assistant", text: "Stopped." },
        ]);
        break;
      case "error":
        // Round LXVIII (audit #34): this used to render `⚠ ${error}` -- a
        // warning glyph welded onto whatever the server sent. The glyph WAS the
        // whole design: it made every failure read as an alarm, and it prefixed
        // sentences Round LXII had already rewritten to be calm and actionable,
        // so the two halves of one message spoke in different voices. The
        // server's sentence now stands on its own with the technical cause
        // behind the ErrorDetail toggle, exactly as the approved contract says.
        // Nothing is hidden: `.msg.failed` marks it without shouting.
        setItems((prev) => [
          ...closeOpenToolCards(clearThinking(prev), "error", "The turn ended before this finished."),
          { kind: "assistant", failed: true,
            text: (ev as any).error || "This turn stopped before it finished.",
            detail: (ev as any).detail || undefined },
        ]);
        break;
      default:
        break; // iteration, and job_* events other than job_queued -> no direct UI here
    }
  }

  // Core send routine shared by the composer and one-click clarification chips.
  // `text` is the exact user message to send; `clearComposer` empties the input
  // (true for typed sends, false for chip auto-sends that never touched it).
  async function sendMessage(text: string, clearComposer: boolean) {
    const msg = text.trim();
    if (!msg || !session || busy) return;
    push({ kind: "user", text: msg });
    if (clearComposer) setInput("");
    setBusy(true);
    const ac = new AbortController();
    abortRef.current = ac;
    await pollChat(session, msg, {
      onEvent: handleEvent,
      onDone: () => {
        // Belt and braces: if a turn ever ends without a terminal event reaching
        // the reducer, the indicator still goes away -- and so does any card
        // still claiming to run. This is the last line of defence for the
        // Round LXIII bug: even a dropped connection cannot leave a spinner
        // running for the rest of the session.
        setItems((prev) => closeOpenToolCards(clearThinking(prev), "stopped",
                                              "This run did not report a result."));
        setBusy(false); setPendingChip(null); onStateChange();
      },
      onError: (e) => {
        // Round LXVIII (audit #34): see transportErrorItem() for what this used
        // to print and why it changed.
        setItems((prev) => [...clearThinking(prev), transportErrorItem(e)]);
        setBusy(false); setPendingChip(null);
      },
      signal: ac.signal,
    });
  }

  async function send() {
    await sendMessage(input, true);
  }

  // One-click continue: a method-choice chip carries a `resume_message`; send it
  // immediately so the task continues WITHOUT the user pressing Send again.
  //
  // Round LXXIX (audit #59): `label` is remembered so the undo affordance below
  // can name what was clicked. Undefined for anything that is not a chip, which
  // is what keeps the affordance off ordinary typed sends.
  function sendDirect(text: string, label?: string) {
    if (label) setPendingChip(label);
    void sendMessage(text, false);
  }

  // Round LXXIX (audit #59): take back a mis-clicked clarification chip.
  //
  // A method chip auto-sends on a single click -- deliberately, since Round XX;
  // that is what makes it one-click. The gap was that there was no way back
  // from the wrong one, and the two chips on the annotation card ("markerDB" /
  // "LLM") sit side by side and run entirely different analyses.
  //
  // IT IS ONLY OFFERED WHILE THE TURN HAS NOT YET DONE ANYTHING. The moment a
  // tool card appears, `canUndo` goes false and the affordance is replaced by
  // the ordinary Stop button. That boundary is the honest part: erasing the
  // transcript of a clustoCell run that has already started on the server would
  // hide work that really happened, which is the Round XLVII lesson --
  // "deleting the evidence is not the same as removing the noise".
  //
  // Before that boundary there is nothing to hide: the turn is a message the
  // server has not acted on, so cancelling it and removing the message is a
  // true undo rather than a concealment.
  const canUndo = busy && pendingChip !== null &&
    !itemsSinceLastUser(items).some((it) => it.kind === "tool");

  function undoChip() {
    if (!canUndo) return;
    abortRef.current?.abort();     // real cancel: pollChat calls api.chatCancel
    setBusy(false);
    setPendingChip(null);
    // Drop the auto-sent user message and the placeholder items that followed
    // it, putting the clarification card back at the end of the transcript with
    // its chips live again.
    setItems((prev) => {
      let cut = -1;
      for (let i = prev.length - 1; i >= 0; i--) if (prev[i].kind === "user") { cut = i; break; }
      return cut >= 0 ? prev.slice(0, cut) : prev;
    });
  }

  function stop() { abortRef.current?.abort(); setBusy(false); }

  return (
    <div className="chat-wrap">
      {/* Round LXXXI (E1 + E2): saved prompts | conversation | objects.
          Both rails scroll independently of the transcript and can be folded
          away to a labelled tab, which is what keeps this honest on a narrow
          window: nothing is ever unreachable, and the conversation never has
          to share width it does not have. See railInitiallyOpen(). */}
      <div className={"chat-3col" + (leftOpen ? "" : " no-left") + (rightOpen ? "" : " no-right")}>
        <aside className="rail rail-left" aria-label="Saved prompts">
          {leftOpen ? (
            <>
              <div className="rail-head">
                <h4>Saved prompts</h4>
                <button type="button" className="rail-toggle" title="Hide saved prompts"
                        aria-label="Hide saved prompts" aria-expanded={true}
                        onClick={() => setLeftOpen(false)}>‹</button>
              </div>
              <PromptRail onPick={setInput} disabled={!session || busy} />
            </>
          ) : (
            <button type="button" className="rail-tab" title="Show saved prompts"
                    aria-label="Show saved prompts" aria-expanded={false}
                    onClick={() => setLeftOpen(true)}>Prompts</button>
          )}
        </aside>

        <div className="chat-center">
      {/* Round LXXXIV: the onboarding card and the local-model banner live
          INSIDE the conversation column, not above the three-column grid.
          Above it they were part of the same vertical flow as the rails, so on
          the first screen — the only screen where the card is shown — the rails
          started below it and ended at the window bottom, floating in the lower
          two thirds. They snapped to full height the moment the first message
          hid the card, which is what made it read as a glitch rather than a
          layout: the same page looked different for no reason the user could
          see. Round XLIII's warning moves with it for the same reason. */}
      <Onboarding settings={settings} onNavigate={onNavigate} hasMessages={items.length > 0} />
      <LocalModelWarning provider={settings?.default_provider} />
      <div className="messages">
        {/* Round LXXX (audit #61 + #62 + #63). What this replaced: two lines of
            prose offering ONE unclickable example, and the claim that you need
            a ".rds" — while seven formats are readable and the good, parser-
            validated example prompts already existed inside cv_system_prompt()
            where only the model could see them.

            Everything here comes from /api/intro, so the screen cannot drift
            from the parser or from cv_supported_formats(). If that call fails
            the block still renders with nothing missing except the chips —
            an empty state that itself fails to load would be a poor first
            impression of a tool whose whole job is to be reliable. */}
        {items.length === 0 && (
          <div className="empty empty-intro">
            <div className="empty-lead">Ask about your data.</div>
            {intro?.examples?.length ? (
              <>
                <div className="empty-sub">Try one of these:</div>
                <div className="empty-chips">
                  {intro.examples.map((ex) => (
                    <button
                      key={ex.text}
                      type="button"
                      className="handle-chip method-chip"
                      disabled={!session || busy}
                      title={ex.text}
                      onClick={() => setInput(ex.text)}
                    >
                      {ex.label}
                    </button>
                  ))}
                </div>
                <div className="empty-note">
                  Clicking one puts it in the message box — edit it before sending if you like.
                </div>
              </>
            ) : null}
            <div className="empty-sub" style={{ marginTop: 14 }}>No data loaded yet.</div>
            <div className="empty-chips">
              <button
                type="button"
                className="btn secondary"
                disabled={!session || demoBusy}
                onClick={loadDemo}
                title="Load SeuratObject::pbmc_small (80 cells) so you can try the whole workflow now"
              >
                {demoBusy ? "Loading…" : "Load a demo dataset"}
              </button>
              <button type="button" className="btn secondary" onClick={() => onNavigate?.("upload")}>
                Upload your own
              </button>
            </div>
            {demoMsg && <div className="empty-note">{demoMsg}</div>}
            {intro?.formats?.length ? (
              <div className="empty-note">
                Readable formats: {intro.formats.join(", ")}.
              </div>
            ) : null}
          </div>
        )}
        {items.map((it, i) => {
          if (it.kind === "user") return <div key={i} className="msg user"><div className="role">you</div>{it.text}</div>;
          if (it.kind === "assistant") return <div key={i} className={it.failed ? "msg assistant failed" : "msg assistant"}><div className="role">agent</div><Markdown text={it.text} onHandleClick={pickHandle} /><ErrorDetailC detail={it.detail} /></div>;
          if (it.kind === "clarification")
            return (
              <div key={i} className="msg assistant clarify">
                <div className="role">agent</div>
                <Markdown text={it.text} onHandleClick={pickHandle} />
                {/* Round LXIV: gate on EITHER dropdowns or inputs. This used to
                    require dropdowns.length > 0, which was fine while the only
                    payload of this shape was Tissue+Condition+n -- and silently
                    rendered NOTHING for the LLM species step, whose payload is
                    one text input and no dropdowns. The card showed its
                    question with no field and no Continue button, so the flow
                    dead-ended. */}
                {((it.dropdowns && it.dropdowns.length > 0) ||
                  (it.inputs && it.inputs.length > 0)) && it.resume_template && (
                  <DropdownGroup dd={it.dropdowns ?? []} template={it.resume_template} busy={busy} onFire={sendDirect} inputs={it.inputs} note={it.note} />
                )}
                {it.choices && it.choices.length > 0 && (
                  <div className="clarify-chips">
                    {it.choices.map((c, ci) => {
                      // Method-choice chip: has a resume_message -> one-click
                      // auto-send so the task continues without pressing Send.
                      if (c.resume_message) {
                        return (
                          <button
                            key={c.id ?? c.label ?? ci}
                            type="button"
                            className="handle-chip method-chip"
                            title={c.summary || c.label}
                            onClick={() => sendDirect(c.resume_message!, String(c.label ?? c.id ?? "that"))}
                            disabled={busy}
                          >
                            {c.label ?? c.id}
                          </button>
                        );
                      }
                      // Object-handle chip: fill the composer; user presses Send.
                      return (
                        <button
                          key={c.handle ?? ci}
                          type="button"
                          className="handle-chip"
                          title={c.summary || c.handle}
                          onClick={() => pickHandle(c.handle!)}
                        >
                          {c.handle}
                          {c.type ? <span className="chip-type">{c.type}</span> : null}
                        </button>
                      );
                    })}
                  </div>
                )}
              </div>
            );
          if (it.kind === "thinking") {
            // Round LI: the post-tool wait gets its own, quieter treatment —
            // it sits under a finished result rather than standing in for a
            // whole reply, so it reads as a continuation, not a new message.
            //
            // NB the braces are load-bearing. Without them the second `return`
            // falls OUTSIDE this branch, so every tool item renders as
            // "thinking…" and the result card never appears. tsc caught it;
            // there is no runtime symptom short of the whole transcript
            // collapsing into placeholders.
            if (it.phase === "interpreting") {
              return <InterpretingNote key={i} since={it.since} />;
            }
            return (
              <div key={i} className="msg assistant thinking">
                <div className="role">agent</div>
                <span className="dots"><span>·</span><span>·</span><span>·</span></span> thinking…
              </div>
            );
          }
          // tool item — one card for the whole call (see components/ToolRun.tsx)
          // Round LXXIX (audit #59): onRerun prefills the composer with this
          // run's own settings, so changing one of them is an edit rather than
          // a retype.
          return <ToolRun key={i} item={it} session={session} onRerun={setInput} />;
        })}
        {/* Round LXXIX (audit #59): the undo window for a chip that auto-sent.
            Present only while the turn it started has not opened a tool card —
            see canUndo. */}
        {canUndo && (
          <div className="chip-undo" role="status">
            <span>Continuing with “{pendingChip}”.</span>
            <button type="button" className="chip-undo-btn" onClick={undoChip}>Undo</button>
          </div>
        )}
        <div ref={endRef} />
      </div>
      <div className="composer">
        <textarea
          value={input}
          placeholder={session ? "Message the agent…" : "connecting…"}
          disabled={!session}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={(e) => { if (e.key === "Enter" && !e.shiftKey) { e.preventDefault(); send(); } }}
        />
        {busy ? (
          // Round LXXIX (audit #53): the only button in the app whose colours
          // were an inline style rather than a class, so it was the one control
          // a theme change could not reach. Same appearance, now in the sheet.
          <button className="composer-stop" onClick={stop}>Stop</button>
        ) : (
          <button onClick={send} disabled={!input.trim() || !session}>Send</button>
        )}
      </div>
        </div>

        <aside className="rail rail-right" aria-label="Objects">
          {rightOpen ? (
            <>
              <div className="rail-head">
                <h4>Objects ({(objects ?? []).length})</h4>
                <button type="button" className="rail-toggle" title="Hide objects"
                        aria-label="Hide objects" aria-expanded={true}
                        onClick={() => setRightOpen(false)}>›</button>
              </div>
              <ObjectRail objects={objects ?? []} onPick={pickHandle}
                          onUpload={() => onNavigate?.("upload")} />
            </>
          ) : (
            <button type="button" className="rail-tab" title="Show objects"
                    aria-label="Show objects" aria-expanded={false}
                    onClick={() => setRightOpen(true)}>
              Objects{(objects ?? []).length ? ` (${(objects ?? []).length})` : ""}
            </button>
          )}
        </aside>
      </div>
    </div>
  );
}

// Round LXXXI: a boolean remembered in localStorage, defaulting on the window
// width the FIRST time (see railInitiallyOpen). Private mode and disabled
// storage are handled the same way the session-id key is in App.tsx: the read
// and the write are both wrapped, and a failure just means the preference does
// not stick.
function useStickyFlag(key: string): [boolean, (v: boolean) => void] {
  const [v, setV] = useState<boolean>(() => {
    let stored: string | null = null;
    try { stored = localStorage.getItem(key); } catch { /* private mode */ }
    const w = typeof window !== "undefined" && typeof window.innerWidth === "number"
      ? window.innerWidth : RAIL_AUTO_OPEN_PX;
    return railInitiallyOpen(stored, w);
  });
  return [v, (next: boolean) => {
    setV(next);
    try { localStorage.setItem(key, next ? "1" : "0"); } catch { /* private mode */ }
  }];
}

// The composer input text is transient (does not need to persist across tabs),
// so it stays local to this component via a tiny hook.
function useLocalInput(): [string, React.Dispatch<React.SetStateAction<string>>] {
  const [v, setV] = useState("");
  return [v, setV];
}
