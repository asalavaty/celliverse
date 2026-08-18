import { useEffect, useMemo, useRef, useState } from "react";
import { ModelChoice } from "../api/client";

// Human-friendly context-length label, e.g. 128000 -> "128k".
export function ctxLabel(n?: number | null): string | null {
  if (n == null || !isFinite(n) || n <= 0) return null;
  if (n >= 1000) return `${Math.round(n / 1000)}k ctx`;
  return `${n} ctx`;
}

// Case-insensitive substring filter over id + label. Exported for unit tests.
export function filterModels(models: ModelChoice[], query: string): ModelChoice[] {
  const q = (query || "").trim().toLowerCase();
  if (!q) return models;
  return models.filter(
    (m) =>
      m.id.toLowerCase().includes(q) ||
      (m.label ? m.label.toLowerCase().includes(q) : false),
  );
}

interface Props {
  value: string;                       // the current model id (free-text allowed)
  onChange: (v: string) => void;
  models: ModelChoice[];               // options to show (may be empty)
  loading?: boolean;
  placeholder?: string;
  onRefresh?: () => void;              // manual "Refresh" button (re-fetch list)
  // Round LXXIX (audit #48): lets the OWNING page put a real <label htmlFor>
  // on this control. The aria-label below already gave it an accessible name,
  // but a label that is not associated is also not CLICKABLE -- and Settings
  // renders the word "Model" directly above a box the click does not reach.
  inputId?: string;
}

// A searchable combobox that ALSO accepts any manually typed model slug.
// - Typing filters the option list AND updates the value (so a slug the backend
//   doesn't know about, e.g. a brand-new OpenRouter model, is still accepted).
// - Clicking an option sets the value.
// - The list shows the model id, an optional friendly label, a context-length
//   badge and a "free" badge. Closing/opening is handled on blur/focus with a
//   small delay so option clicks register.
export default function ModelCombobox({
  value, onChange, models, loading, placeholder, onRefresh, inputId,
}: Props) {
  const [open, setOpen] = useState(false);
  const [active, setActive] = useState(0);
  const wrapRef = useRef<HTMLDivElement | null>(null);

  // Filter against what the user has typed so far.
  const filtered = useMemo(() => filterModels(models, value), [models, value]);

  // Keep the highlighted option in range as the filtered list changes.
  useEffect(() => { setActive(0); }, [value, models]);

  // Close when clicking outside the widget.
  useEffect(() => {
    function onDoc(e: MouseEvent) {
      if (wrapRef.current && !wrapRef.current.contains(e.target as Node)) setOpen(false);
    }
    document.addEventListener("mousedown", onDoc);
    return () => document.removeEventListener("mousedown", onDoc);
  }, []);

  function choose(id: string) {
    onChange(id);
    setOpen(false);
  }

  function onKeyDown(e: React.KeyboardEvent) {
    if (!open && (e.key === "ArrowDown" || e.key === "ArrowUp")) { setOpen(true); return; }
    if (e.key === "ArrowDown") { e.preventDefault(); setActive((a) => Math.min(a + 1, filtered.length - 1)); }
    else if (e.key === "ArrowUp") { e.preventDefault(); setActive((a) => Math.max(a - 1, 0)); }
    else if (e.key === "Enter") {
      if (open && filtered[active]) { e.preventDefault(); choose(filtered[active].id); }
    } else if (e.key === "Escape") { setOpen(false); }
  }

  return (
    <div className="combo" ref={wrapRef}>
      <div className="combo-row">
        <input
          id={inputId}
          value={value}
          placeholder={placeholder}
          onChange={(e) => { onChange(e.target.value); setOpen(true); }}
          onFocus={() => setOpen(true)}
          onKeyDown={onKeyDown}
          aria-label="Model"
          role="combobox"
          aria-expanded={open}
          aria-controls="combo-model-list"
          aria-autocomplete="list"
          aria-activedescendant={open && filtered[active] ? `combo-opt-${active}` : undefined}
          autoComplete="off"
          spellCheck={false}
        />
        {onRefresh && (
          <button type="button" className="btn secondary" onClick={onRefresh} disabled={loading}
                  title="Re-fetch the model list from the provider">
            {loading ? "…" : "Refresh"}
          </button>
        )}
      </div>
      {open && (
        <div className="combo-list" role="listbox" id="combo-model-list" aria-label="Model">
          {loading && <div className="combo-empty">Loading models…</div>}
          {!loading && filtered.length === 0 && (
            <div className="combo-empty">
              No matching models — press Enter to use “{value}” as a custom model id.
            </div>
          )}
          {!loading && filtered.map((m, i) => {
            const cl = ctxLabel(m.context_length);
            return (
              <div
                key={m.id}
                id={`combo-opt-${i}`}
                className={`combo-opt${i === active ? " active" : ""}`}
                role="option"
                aria-selected={value === m.id}
                onMouseEnter={() => setActive(i)}
                onMouseDown={(e) => { e.preventDefault(); choose(m.id); }}
              >
                <span className="id">{m.id}</span>
                {m.label && m.label !== m.id && (
                  <span className="muted" style={{ marginLeft: 8, fontSize: 12 }}>{m.label}</span>
                )}
                <span className="spacer" />
                {m.free && <span className="badge free">free</span>}
                {cl && <span className="badge ctx">{cl}</span>}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}
