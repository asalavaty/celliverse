import { useEffect, useId, useMemo, useState } from "react";
import { api, SavedPrompt, SavedPrompts } from "../api/client";

// ===========================================================================
// Round LXXXI (E2): the saved-prompts rail.
//
// The starter prompts Round LXXX added to the empty screen disappear the
// instant the conversation has one message in it -- which is the moment they
// stop being an introduction and start being shortcuts. A user who runs the
// same six requests every week retypes them every week.
//
// So the starters move into a rail that is always there, and become editable:
// add your own, drop the ones you never use (INCLUDING the built-ins), and
// group them under whatever headings you think in. They persist SERVER-SIDE,
// in ~/.celliverse/prompts.json -- see the header of R/agent_prompts.R for why
// localStorage was rejected for something the request called "restored and
// accessible in and across all sessions".
//
// Clicking a prompt fills the message box; it does NOT send. That is the same
// contract the empty-screen chips have had since Round LXXX ("clicking one
// puts it in the message box - edit it before sending if you like"), and a
// favourites list that fires a real analysis on one stray click would be a
// worse product than one that does not.
//
// The pure helpers are exported and sit above the first hook-using component
// so they can be driven directly in Node, the same convention as Chat.tsx.
// ===========================================================================

export const PROMPT_DEFAULT_CATEGORY = "General";

export function promptCategory(p: SavedPrompt | null | undefined): string {
  const c = (p?.category ?? "").toString().trim();
  return c || PROMPT_DEFAULT_CATEGORY;
}

// Case-insensitive AND over whitespace-separated terms, matched against the
// label, the category and the prompt text -- the text included deliberately,
// because after fifty saved prompts the thing you remember is a word from the
// request, not the name you gave it.
export function filterPrompts(
  prompts: SavedPrompt[] | null | undefined,
  query: string,
  category: string
): SavedPrompt[] {
  const list = Array.isArray(prompts) ? prompts : [];
  const terms = (query ?? "").toLowerCase().split(/\s+/).filter(Boolean);
  return list.filter((p) => {
    if (category && promptCategory(p) !== category) return false;
    if (!terms.length) return true;
    const hay = `${p?.label ?? ""} ${promptCategory(p)} ${p?.text ?? ""}`.toLowerCase();
    return terms.every((t) => hay.includes(t));
  });
}

// Group into sections in first-appearance order (starters first, because the
// server returns them first). Same reasoning as ObjectRail.groupObjects():
// stable order beats sorted order for a panel that re-renders under the
// pointer.
export function groupPrompts(
  prompts: SavedPrompt[] | null | undefined
): { category: string; items: SavedPrompt[] }[] {
  const out: { category: string; items: SavedPrompt[] }[] = [];
  const index = new Map<string, number>();
  for (const p of Array.isArray(prompts) ? prompts : []) {
    const c = promptCategory(p);
    if (!index.has(c)) { index.set(c, out.length); out.push({ category: c, items: [] }); }
    out[index.get(c)!].items.push(p);
  }
  return out;
}

export default function PromptRail({ onPick, disabled }: {
  onPick: (text: string) => void;
  disabled?: boolean;
}) {
  const [data, setData] = useState<SavedPrompts | null>(null);
  const [query, setQuery] = useState("");
  const [category, setCategory] = useState("");
  const [adding, setAdding] = useState(false);
  const [label, setLabel] = useState("");
  const [text, setText] = useState("");
  const [cat, setCat] = useState("");
  const [err, setErr] = useState("");
  const [busy, setBusy] = useState(false);
  const uid = useId();
  const listId = `pr-cats-${uid}`;

  useEffect(() => {
    // A failure leaves `data` null and the rail renders its own quiet line
    // rather than nothing at all: a favourites panel that silently vanishes
    // looks identical to one you have not filled in yet.
    api.prompts().then(setData).catch(() => setData(null));
  }, []);

  const prompts = data?.prompts ?? [];
  const groups = useMemo(
    () => groupPrompts(filterPrompts(prompts, query, category)),
    [prompts, query, category]
  );
  const cats = useMemo(() => groupPrompts(prompts).map((g) => g.category), [prompts]);
  const shown = groups.reduce((n, g) => n + g.items.length, 0);

  async function run(work: () => Promise<SavedPrompts>) {
    if (busy) return;
    setBusy(true); setErr("");
    try {
      setData(await work());
    } catch (e: any) {
      setErr(e?.message || "That did not save. Check the R session is still running, then try again.");
    } finally { setBusy(false); }
  }

  async function add() {
    const t = text.trim();
    if (!t) { setErr("Type the message you want to keep, then add it."); return; }
    await run(async () => {
      const r = await api.addPrompt(label.trim() || t.slice(0, 60), t,
                                   cat.trim() || PROMPT_DEFAULT_CATEGORY);
      setLabel(""); setText(""); setAdding(false);
      return r;
    });
  }

  return (
    <div className="rail-body">
      {data === null ? (
        <div className="rail-empty">Saved prompts are not available right now.</div>
      ) : (
        <>
          {prompts.length > 6 && (
            <input
              type="search"
              className="rail-filter"
              placeholder="Filter prompts…"
              aria-label="Filter saved prompts"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
            />
          )}
          {cats.length > 1 && (
            <div className="rail-cats">
              <button
                type="button"
                className={category === "" ? "rail-cat is-on" : "rail-cat"}
                aria-pressed={category === ""}
                onClick={() => setCategory("")}
              >
                All <span className="rail-cat-n">{prompts.length}</span>
              </button>
              {cats.map((c) => (
                <button
                  key={c}
                  type="button"
                  className={category === c ? "rail-cat is-on" : "rail-cat"}
                  aria-pressed={category === c}
                  onClick={() => setCategory(category === c ? "" : c)}
                >
                  {c}
                </button>
              ))}
            </div>
          )}
          <div className="rail-scroll">
            {prompts.length === 0 && (
              <div className="rail-empty">
                No saved prompts. Add the requests you make often and they will be here next time.
              </div>
            )}
            {prompts.length > 0 && shown === 0 && (
              <div className="rail-empty">Nothing matches that filter.</div>
            )}
            {groups.map((g) => (
              <div key={g.category} className="rail-group">
                <div className="rail-group-head">
                  {g.category} <span className="rail-cat-n">{g.items.length}</span>
                </div>
                {g.items.map((p) => (
                  <div key={p.id} className="prompt-row">
                    <button
                      type="button"
                      className="prompt-btn"
                      title={p.text}
                      disabled={disabled}
                      onClick={() => onPick(p.text)}
                    >
                      {p.label}
                    </button>
                    {/* A built-in is HIDDEN and one of your own is DELETED.
                        The title says which, because they are different acts
                        and only one of them is undoable from this panel. */}
                    <button
                      type="button"
                      className="prompt-del"
                      aria-label={p.builtin ? `Hide ${p.label}` : `Remove ${p.label}`}
                      title={p.builtin
                        ? "Hide this starter. Restore starters puts it back."
                        : "Remove this saved prompt."}
                      disabled={busy}
                      onClick={() => run(() => api.removePrompt(p.id))}
                    >
                      ×
                    </button>
                  </div>
                ))}
              </div>
            ))}
          </div>
          {err && <div className="rail-err">{err}</div>}
          {adding ? (
            <div className="prompt-add">
              <label className="rail-lab" htmlFor={`pr-t-${uid}`}>Prompt</label>
              <textarea
                id={`pr-t-${uid}`}
                className="rail-input"
                rows={3}
                placeholder="cluster this dataset and annotate the cell types"
                value={text}
                onChange={(e) => setText(e.target.value)}
              />
              <label className="rail-lab" htmlFor={`pr-l-${uid}`}>Name (optional)</label>
              <input
                id={`pr-l-${uid}`}
                type="text"
                className="rail-input"
                placeholder="Cluster and annotate"
                value={label}
                onChange={(e) => setLabel(e.target.value)}
              />
              <label className="rail-lab" htmlFor={`pr-c-${uid}`}>Category (optional)</label>
              <input
                id={`pr-c-${uid}`}
                type="text"
                className="rail-input"
                list={listId}
                placeholder={PROMPT_DEFAULT_CATEGORY}
                value={cat}
                onChange={(e) => setCat(e.target.value)}
              />
              <datalist id={listId}>
                {cats.map((c) => <option key={c} value={c} />)}
              </datalist>
              <div className="rail-row">
                <button type="button" className="btn" disabled={busy || !text.trim()} onClick={add}>
                  {busy ? "Saving…" : "Add"}
                </button>
                <button type="button" className="btn secondary" disabled={busy}
                        onClick={() => { setAdding(false); setErr(""); }}>
                  Cancel
                </button>
              </div>
            </div>
          ) : (
            <div className="rail-row">
              <button type="button" className="btn secondary rail-add-btn"
                      onClick={() => { setAdding(true); setErr(""); }}>
                + Add a prompt
              </button>
              {(data?.hidden ?? 0) > 0 && (
                <button type="button" className="rail-link" disabled={busy}
                        onClick={() => run(() => api.restorePrompts())}>
                  Restore starters
                </button>
              )}
            </div>
          )}
          <div className="rail-foot">Click a prompt to put it in the message box.</div>
        </>
      )}
    </div>
  );
}
