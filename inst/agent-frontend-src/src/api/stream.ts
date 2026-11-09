// SSE streaming client for POST /api/chat.
//
// EventSource only supports GET, but we need to POST the message + session, so
// we stream the response body ourselves with fetch + a ReadableStream reader and
// parse the "event: <type>\ndata: <json>\n\n" frames the R backend emits
// (see cv_sse_send / cv_api_chat_stream in agent_api.R).

/**
 * Round LXIX (audit #23/#24/#25): one caveat raised by a tool call.
 *
 * EXACTLY TWO severities, and that is a decision rather than a starting point.
 * "may_invalidate" means ignoring this may leave you with a wrong conclusion
 * from the numbers on screen; "info" means the agent is telling you what it
 * did. A five-level scale is how users learn to ignore all warnings, which is
 * the failure this exists to prevent.
 *
 * Arrives inside `tool_result.result.warnings`, already sorted may_invalidate
 * first by the server (cv_warnings_merge).
 */
export interface CvWarning {
  severity: "may_invalidate" | "info";
  text: string;
  code?: string | null;
}

export type ChatEvent =
  | { type: "iteration"; n: number }
  | { type: "token"; text: string }
  | { type: "tool_start"; tool: string; arguments?: unknown }
  | { type: "tool_result"; tool: string; handle?: string; summary?: string; result?: any }
  | { type: "tool_error"; tool: string; error: string }
  // Round XLVII: emitted by the worker roughly every 400 ms for a heavy tool.
  // It was always on the wire; the chat simply had no case for it and dropped
  // it, which is why a 30-minute clustering run showed a motionless line.
  | { type: "progress"; job?: string; tool?: string; progress?: number; message?: string }
  // Round LXVIII (audit #32): a heavy job admitted to the queue but not yet
  // started. `reason` is "pool" (every worker slot is busy) or "memory" (free
  // RAM is below heavy_job_min_free_mb, so an ADDITIONAL concurrent job is held
  // rather than spawned). The two clear differently, so the transcript says
  // which one it is waiting on.
  | { type: "job_queued"; job?: string; tool?: string; reason?: "pool" | "memory";
      running?: number; pool_size?: number; available_mb?: number; min_free_mb?: number }
  | { type: "assistant"; text: string }
  | { type: "clarification"; text: string; tool?: string; choices?: ClarifyChoice[]; dropdowns?: ClarifyDropdown[]; inputs?: ClarifyNumberInput[]; note?: string; resume_template?: string; base_request?: string }
  | { type: "done"; [k: string]: unknown }
  // Round LXIV (D6): `detail` is the technical cause, shown behind a toggle.
  | { type: "error"; error: string; detail?: string }
  | { type: string; [k: string]: unknown };

// One labeled dropdown in a clarification prompt (e.g. Tissue / Condition for the
// markerDB annotation path). `options` is the full selectable list; the first is
// conventionally an "All (no filter)" sentinel. The user picks one value per
// dropdown; once ALL dropdowns have a value the UI composes a resume message from
// `resume_template` (substituting {id} placeholders) and auto-sends it.
export interface ClarifyDropdown {
  id: string;          // placeholder name in resume_template, e.g. "tissue"
  label: string;       // human label shown above the combo box, e.g. "Tissue"
  options: string[];   // full list of selectable values
  // Round LXXIX: the server's prefill, read out of the user's own message by
  // cv_annotation_options_payload() -> .cv_match_vocab().
  //
  // Round LXXVIII shipped the SERVER half of audit #38 and never added this
  // field here, so the payload carried `value: "Bone Marrow"`, the card
  // announced "Filled in from your message: tissue, n", and the Tissue box
  // still rendered empty. The number input DID prefill, because
  // ClarifyNumberInput has had `default` since Round XX -- which is exactly why
  // the symptom looked like "only n is prefilled" rather than "prefill is
  // broken". Reported from live use; it is the source-vs-behaviour gap in
  // miniature, the payload having been tested and the rendered card not.
  //
  // When set this is always a member of `options` (the server matches against
  // the package's own vocabulary for that species), so seeding a selection from
  // it cannot put a value in the box that the list does not offer.
  value?: string;
}

// One labeled numeric input in a clarification prompt (e.g. "Top markers (n)"
// for the annotation path). `default` prefills the box; `min` is the smallest
// valid value. The value is substituted into `resume_template` as {id}.
export interface ClarifyNumberInput {
  id: string;          // placeholder name in resume_template, e.g. "n"
  label: string;       // human label shown above the box
  // Round LXIV: `type` distinguishes the numeric n field from the free-text
  // species field the LLM annotation path asks for. Absent means "number", so
  // every existing payload keeps its behaviour.
  type?: "number" | "text";
  placeholder?: string;
  default?: number | string;    // prefilled value (e.g. 20, or "human")
  min?: number;        // smallest valid value (e.g. 1)
}

// One selectable object in a clarification prompt. Two shapes share this type:
//  - object-handle chips: `handle` set -> clicking fills the composer (user presses Send).
//  - method-choice chips: `resume_message` set -> clicking AUTO-SENDS that message
//    so the task continues without the user pressing Send again.
export interface ClarifyChoice {
  handle?: string;
  id?: string;
  label?: string;
  type?: string;
  summary?: string;
  resume_message?: string;
}

export interface StreamHandlers {
  onEvent: (ev: ChatEvent) => void;
  onDone?: () => void;
  onError?: (err: Error) => void;
  signal?: AbortSignal;
}

export async function streamChat(
  session: string,
  message: string,
  handlers: StreamHandlers
): Promise<void> {
  const { onEvent, onDone, onError, signal } = handlers;
  try {
    const resp = await fetch("/api/chat", {
      method: "POST",
      headers: { "Content-Type": "application/json", Accept: "text/event-stream" },
      body: JSON.stringify({ session, message }),
      signal,
    });
    if (!resp.ok || !resp.body) throw new Error(`chat stream -> ${resp.status}`);

    const reader = resp.body.getReader();
    const decoder = new TextDecoder();
    let buffer = "";

    // Frames are separated by a blank line. Accumulate, split on \n\n.
    for (;;) {
      const { value, done } = await reader.read();
      if (done) break;
      buffer += decoder.decode(value, { stream: true });

      let sep: number;
      while ((sep = buffer.indexOf("\n\n")) !== -1) {
        const frame = buffer.slice(0, sep);
        buffer = buffer.slice(sep + 2);
        const ev = parseFrame(frame);
        if (ev) onEvent(ev);
      }
    }
    // Flush a trailing frame with no blank line.
    if (buffer.trim().length) {
      const ev = parseFrame(buffer);
      if (ev) onEvent(ev);
    }
    onDone?.();
  } catch (e) {
    if ((e as any)?.name === "AbortError") { onDone?.(); return; }
    onError?.(e as Error);
  }
}

// ---------------------------------------------------------------------------
// Async-turn polling driver (the live-feedback transport).
//
// plumber cannot flush a partial HTTP response, so instead of reading a single
// streamed body we: (1) POST /api/chat/start to launch the turn, then (2) GET
// /api/chat/poll?cursor= on an interval, forwarding each new event to the same
// onEvent handler used by streamChat. This gives real live feedback: the first
// poll already returns a "thinking" event, tool_start/tool_result arrive as the
// tools run, and token events stream the final answer. Aborting cancels the
// turn server-side (POST /api/chat/cancel) and stops polling.
// ---------------------------------------------------------------------------

import { api } from "./client";

export interface PollOptions extends StreamHandlers {
  intervalMs?: number; // default 350
}

export async function pollChat(
  session: string,
  message: string,
  handlers: PollOptions
): Promise<void> {
  const { onEvent, onDone, onError, signal } = handlers;
  const interval = handlers.intervalMs ?? 350;
  let turn: string | null = null;
  let stopping = false;
  let cancelPromise: Promise<unknown> | null = null;

  // Round LXIII: aborting used to set `cancelled` and fire chatCancel WITHOUT
  // waiting, so the loop below broke out at the top of its next iteration and
  // never polled again. The server DOES append a `cancelled` event to the turn
  // buffer (cv_turn_cancel in agent_turns.R) -- the client simply stopped
  // listening one poll too early, so the reducer never saw it and never closed
  // the open tool card. That is why a stopped run kept spinning for the rest of
  // the session.
  //
  // `stopping` (not `cancelled`) lets the loop take ONE more pass to drain the
  // terminal events before it exits.
  const onAbort = () => {
    stopping = true;
    cancelPromise = turn ? api.chatCancel(session, turn).catch(() => {}) : Promise.resolve();
  };
  if (signal) {
    if (signal.aborted) { onDone?.(); return; }
    signal.addEventListener("abort", onAbort, { once: true });
  }

  const sleep = (ms: number) => new Promise((res) => setTimeout(res, ms));

  try {
    const started = await api.chatStart(session, message);
    turn = started.turn;
    let cursor = started.cursor ?? 0;

    for (;;) {
      // A stop is not a reason to stop LISTENING. Wait for the cancel to be
      // acknowledged, then poll once more: that pass carries the `cancelled`
      // event, and with it the only signal the transcript has that the run is
      // over.
      if (stopping && cancelPromise) { await cancelPromise; cancelPromise = null; }
      const res = await api.chatPoll(session, turn, cursor);
      cursor = res.cursor ?? cursor;
      if (Array.isArray(res.events)) {
        for (const ev of res.events) onEvent(ev as ChatEvent);
      }
      if (res.done || stopping) break;
      await sleep(interval);
    }
    onDone?.();
  } catch (e) {
    if ((e as any)?.name === "AbortError" || stopping) { onDone?.(); return; }
    onError?.(e as Error);
  } finally {
    if (signal) signal.removeEventListener("abort", onAbort);
  }
}

function parseFrame(frame: string): ChatEvent | null {
  let event = "message";
  const dataLines: string[] = [];
  for (const line of frame.split("\n")) {
    if (line.startsWith("event:")) event = line.slice(6).trim();
    else if (line.startsWith("data:")) dataLines.push(line.slice(5).trim());
  }
  const dataStr = dataLines.join("\n");
  if (!dataStr && event === "message") return null;
  let data: any = {};
  if (dataStr) {
    try { data = JSON.parse(dataStr); } catch { data = { raw: dataStr }; }
  }
  return { type: event, ...data } as ChatEvent;
}
