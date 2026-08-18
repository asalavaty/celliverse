import { useEffect, useState } from "react";
import { api, ToolSpec } from "../api/client";

// Package Browser + Tool Inspector, backed by /api/registry. Shows every tool
// the agent can call, its description and its parameters.
//
// Round LXXIX (audit #49 + #54). Two defects, one page:
//
//   #49 — the tool list was a stack of `<div onClick>`. A div is not in the tab
//   order and does not respond to Enter or Space, so the ONLY way to inspect a
//   tool was to click it with a mouse. They are real <button>s now, which is
//   also why none of the keyboard handling is written by hand: a button already
//   does focus, Enter, Space, and the announced role.
//
//   #54 — the Inspector's whole content was `JSON.stringify(sel.parameters)`.
//   A raw JSON Schema shipped as a feature: to find out whether `resolution`
//   was required, or what its default was, the reader had to parse
//   `{"type":"object","properties":{...},"required":[...]}` by eye. The schema
//   is now READ and rendered as a parameter list, with the raw JSON kept behind
//   a disclosure for anyone who wants it — nothing is hidden, it just is not
//   the first thing on screen any more.

// A JSON Schema `properties` entry, as much of it as we render.
type SchemaProp = {
  type?: string | string[];
  description?: string;
  enum?: unknown[];
  default?: unknown;
  items?: { type?: string | string[] };
};

/**
 * Flatten a JSON Schema object into the rows the inspector shows.
 *
 * Exported and pure so the rendering contract is testable without a DOM — the
 * convention splitWarnings()/mergeToolResultItem() already set in this app.
 *
 * Deliberately TOLERANT: a schema with no `properties`, or one that is not an
 * object at all, yields an empty list rather than throwing. The registry is
 * server-generated and this page must not be the thing that breaks when a tool
 * grows an unusual parameter shape.
 */
export function schemaRows(parameters: unknown): Array<{
  name: string; type: string; required: boolean; description?: string;
  choices?: string[]; deflt?: string;
}> {
  const p = parameters as any;
  if (!p || typeof p !== "object") return [];
  const props = p.properties;
  if (!props || typeof props !== "object") return [];
  const required: string[] = Array.isArray(p.required) ? p.required.map(String) : [];
  const typeOf = (s: SchemaProp): string => {
    const t = Array.isArray(s?.type) ? s.type.join(" | ") : (s?.type ?? "any");
    // An array's ELEMENT type is the informative half — "array" alone does not
    // tell the reader whether it wants gene symbols or cluster numbers.
    if (t === "array" && s?.items?.type) {
      const it = Array.isArray(s.items.type) ? s.items.type.join(" | ") : s.items.type;
      return `array of ${it}`;
    }
    return String(t);
  };
  return Object.keys(props).map((name) => {
    const s: SchemaProp = props[name] ?? {};
    return {
      name,
      type: typeOf(s),
      required: required.includes(name),
      description: typeof s.description === "string" ? s.description : undefined,
      choices: Array.isArray(s.enum) ? s.enum.map(String) : undefined,
      deflt: s.default === undefined || s.default === null ? undefined
        : typeof s.default === "object" ? JSON.stringify(s.default) : String(s.default),
    };
  });
}

export default function PackageBrowser() {
  const [tools, setTools] = useState<ToolSpec[]>([]);
  const [meta, setMeta] = useState<any>(null);
  const [sel, setSel] = useState<ToolSpec | null>(null);
  const [err, setErr] = useState("");

  useEffect(() => {
    api.registry().then((r) => { setTools(r.tools ?? []); setMeta(r.metadata ?? null); })
      .catch((e) => setErr(e.message));
  }, []);

  if (err) return <div className="err-text">Could not load registry: {err}</div>;

  const rows = sel ? schemaRows(sel.parameters) : [];

  return (
    <div className="pkg-split">
      <div className="card">
        <h3>Tools ({tools.length})</h3>
        {meta && <p className="muted" style={{ fontSize: 12 }}>
          package: {meta.package ?? "celliverse"} · core tools exposed to the model
        </p>}
        <div className="pkg-list">
          {tools.map((t) => (
            <button
              key={t.name}
              type="button"
              className={"pkg-row mono" + (sel?.name === t.name ? " is-sel" : "")}
              aria-pressed={sel?.name === t.name}
              onClick={() => setSel(t)}
            >
              {t.name}
            </button>
          ))}
        </div>
      </div>
      <div className="card">
        <h3>Inspector</h3>
        {!sel && <div className="muted">Select a tool to see its schema.</div>}
        {sel && (
          <div>
            <h4 className="mono">{sel.name}</h4>
            <p>{sel.description}</p>
            <h4 style={{ marginTop: 10 }}>Parameters ({rows.length})</h4>
            {rows.length === 0 && <div className="muted">This tool takes no parameters.</div>}
            {rows.length > 0 && (
              <dl className="pkg-params">
                {rows.map((r) => (
                  <div key={r.name} className="pkg-param">
                    <dt>
                      <span className="mono pkg-param-name">{r.name}</span>
                      <span className="pkg-param-type">{r.type}</span>
                      {r.required
                        ? <span className="pkg-param-req">required</span>
                        : <span className="pkg-param-opt">optional</span>}
                    </dt>
                    <dd>
                      {r.description && <div>{r.description}</div>}
                      {r.deflt !== undefined && (
                        <div className="muted">default: <span className="mono">{r.deflt}</span></div>
                      )}
                      {r.choices && r.choices.length > 0 && (
                        <div className="muted">one of: <span className="mono">{r.choices.join(", ")}</span></div>
                      )}
                    </dd>
                  </div>
                ))}
              </dl>
            )}
            {/* Kept, not deleted. Someone writing a client against this registry
                wants the exact schema, and this page was the only place it was
                visible. */}
            <details className="pkg-raw">
              <summary>Raw JSON schema</summary>
              <pre className="mono">{JSON.stringify(sel.parameters ?? {}, null, 2)}</pre>
            </details>
          </div>
        )}
      </div>
    </div>
  );
}
