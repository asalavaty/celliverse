import { useMemo, useState } from "react";
import type { ObjectDescriptor } from "../api/client";

// ===========================================================================
// Round LXXXI (E1): the Objects rail.
//
// WHAT THIS REPLACED, and why it had to move.
//
// The object list lived in the 240px left sidebar, under the nav, with `flex:
// 1` and `overflow-y: auto`. That worked for the three objects a demo session
// has. The user's actual complaint is about the shape of the analysis: a
// clustering run plus sub-clusters plus a marker table plus an annotation is
// four objects per dataset, and a session that touches twenty datasets has
// eighty. In a 240px column shared with nine nav buttons there is no width for
// a summary, no room for a filter, and no way to tell a Seurat from a
// ClustoCell except by reading the handle -- while the whole right half of a
// wide window sat empty, because .messages is capped at 1180px and centred.
//
// So the panel moves to that empty space and gains the two things a long list
// needs: GROUPING BY TYPE, which is the categorisation the descriptors already
// carry and nothing rendered, and a FILTER over handle, type and summary.
// Everything the old cards showed is kept -- Round LXXIX put the summary on
// screen for a reason, and the `title` tooltip stays because the visible line
// is still clamped.
//
// The card is now a BUTTON. Handles were already clickable everywhere else in
// this app (the clarification chips, and every handle inside a markdown reply
// via Markdown's onHandleClick), and the one place that listed them all was
// the one place you could not click them.
//
// The pure functions are exported and sit ABOVE the first hook-using
// component, so the grouping and filtering can be driven directly in Node --
// the same cut-point convention Chat.tsx uses for mergeToolResultItem().
// ===========================================================================

// The group a descriptor belongs to. `type` is what the R descriptor carries
// (Seurat, ClustoCell, MarkoCell, data.frame, ...) and is the categorisation
// the user asked for. A descriptor with no type is not dropped -- it is put in
// a named group, because an object you cannot see is worse than an ugly
// heading.
export function objectCategory(o: ObjectDescriptor | null | undefined): string {
  const t = (o?.type ?? "").toString().trim();
  return t || "Other";
}

// Case-insensitive AND over whitespace-separated terms, matched against the
// handle, the type and the summary.
//
// AND rather than OR because that is what makes a filter useful at a hundred
// objects: "seurat pbmc" should mean both, and an OR would return every Seurat
// in the session the moment you typed the first word.
export function filterObjects(
  objects: ObjectDescriptor[] | null | undefined,
  query: string,
  category: string
): ObjectDescriptor[] {
  const list = Array.isArray(objects) ? objects : [];
  const terms = (query ?? "").toLowerCase().split(/\s+/).filter(Boolean);
  return list.filter((o) => {
    if (category && objectCategory(o) !== category) return false;
    if (!terms.length) return true;
    const hay = `${o?.handle ?? ""} ${objectCategory(o)} ${o?.summary ?? ""}`.toLowerCase();
    return terms.every((t) => hay.includes(t));
  });
}

// Group into sections, preserving FIRST-APPEARANCE order of the categories.
//
// Not alphabetical and not by size: the server returns objects in the order
// they were created, so first-appearance order means the panel does not
// reshuffle itself under the pointer every four seconds when the poll returns
// a new object -- which sorting by count would do the moment a group grew.
export function groupObjects(
  objects: ObjectDescriptor[] | null | undefined
): { category: string; items: ObjectDescriptor[] }[] {
  const out: { category: string; items: ObjectDescriptor[] }[] = [];
  const index = new Map<string, number>();
  for (const o of Array.isArray(objects) ? objects : []) {
    const c = objectCategory(o);
    if (!index.has(c)) { index.set(c, out.length); out.push({ category: c, items: [] }); }
    out[index.get(c)!].items.push(o);
  }
  return out;
}

// The category filter buttons: every category with its count, in the same
// first-appearance order, computed over the UNFILTERED list so the counts do
// not change as you type (a count that moves while you filter cannot be used
// to decide what to filter for).
export function categoryCounts(
  objects: ObjectDescriptor[] | null | undefined
): { category: string; n: number }[] {
  return groupObjects(objects).map((g) => ({ category: g.category, n: g.items.length }));
}

export default function ObjectRail({ objects, onPick, onUpload }: {
  objects: ObjectDescriptor[];
  onPick: (handle: string) => void;
  onUpload?: () => void;
}) {
  const [query, setQuery] = useState("");
  const [category, setCategory] = useState("");
  const counts = useMemo(() => categoryCounts(objects), [objects]);
  const groups = useMemo(
    () => groupObjects(filterObjects(objects, query, category)),
    [objects, query, category]
  );
  const shown = groups.reduce((n, g) => n + g.items.length, 0);

  return (
    <div className="rail-body">
      {objects.length === 0 ? (
        <div className="rail-empty">
          No objects yet.
          {onUpload && (
            <>
              {" "}
              <button type="button" className="rail-link" onClick={onUpload}>
                Load one
              </button>
              .
            </>
          )}
        </div>
      ) : (
        <>
          {/* The filter is offered only once there is enough to filter. Below
              that it is a control that cannot change anything, taking space
              from the list it sits above. */}
          {objects.length > 4 && (
            <input
              type="search"
              className="rail-filter"
              placeholder="Filter objects…"
              aria-label="Filter objects"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
            />
          )}
          {counts.length > 1 && (
            <div className="rail-cats">
              <button
                type="button"
                className={category === "" ? "rail-cat is-on" : "rail-cat"}
                aria-pressed={category === ""}
                onClick={() => setCategory("")}
              >
                All <span className="rail-cat-n">{objects.length}</span>
              </button>
              {counts.map((c) => (
                <button
                  key={c.category}
                  type="button"
                  className={category === c.category ? "rail-cat is-on" : "rail-cat"}
                  aria-pressed={category === c.category}
                  onClick={() => setCategory(category === c.category ? "" : c.category)}
                >
                  {c.category} <span className="rail-cat-n">{c.n}</span>
                </button>
              ))}
            </div>
          )}
          <div className="rail-scroll">
            {shown === 0 && <div className="rail-empty">Nothing matches that filter.</div>}
            {groups.map((g) => (
              <div key={g.category} className="rail-group">
                <div className="rail-group-head">
                  {g.category} <span className="rail-cat-n">{g.items.length}</span>
                </div>
                {g.items.map((o) => (
                  <button
                    key={o.handle}
                    type="button"
                    className="obj-card obj-card-btn"
                    title={o.summary ? `${o.handle} — ${o.summary}` : o.handle}
                    onClick={() => onPick(o.handle)}
                  >
                    <span className="obj-handle mono">{o.handle}</span>
                    {o.summary && <span className="obj-summary">{o.summary}</span>}
                  </button>
                ))}
              </div>
            ))}
          </div>
          <div className="rail-foot">Click an object to put its handle in the message box.</div>
        </>
      )}
    </div>
  );
}
