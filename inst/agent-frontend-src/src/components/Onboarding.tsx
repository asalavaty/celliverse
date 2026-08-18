import { useEffect, useState } from "react";
import { api, Settings } from "../api/client";
import type { Page } from "../App";

// Round LXXX: was "#0279ee" -- a hardcoded copy of the accent Round LXXIX
// retired for failing contrast (4.22:1). It lives in a .tsx, so the
// stylesheet sweep did not reach it. Reads the token now.
const A = { color: "var(--accent)", textDecoration: "none" } as const;
const DISMISS_KEY = "celliverse_onboarding_dismissed";

interface Props {
  settings: Settings | null;
  onNavigate?: (p: Page) => void;
  hasMessages: boolean;
}

// First-run onboarding. Two coordinated pieces:
//  - a dismissible banner (top of Chat) shown until the user dismisses it;
//  - a richer welcome card in the empty chat state.
// Both explain that the agent runs on an OpenRouter model by default (free for
// a limited quota up to the user's OpenRouter credit), that
// OpenRouter needs a free API key (with an inline paste-and-save affordance),
// that Ollama is an optional fully-local alternative, and that any provider /
// key / model can be set in Settings. Nothing secret is shipped — the user
// pastes their own key.
export default function Onboarding({ settings, onNavigate, hasMessages }: Props) {
  const [dismissed, setDismissed] = useState<boolean>(() => {
    try { return localStorage.getItem(DISMISS_KEY) === "1"; } catch { return false; }
  });
  const [key, setKey] = useState("");
  const [saving, setSaving] = useState(false);
  const [saved, setSaved] = useState(false);
  const [err, setErr] = useState("");
  // Round LXXX (audit #60): the capability lines, from R.
  const [canDo, setCanDo] = useState<string[]>([]);
  useEffect(() => {
    api.intro().then((r) => setCanDo(Array.isArray(r?.can_do) ? r.can_do : [])).catch(() => setCanDo([]));
  }, []);

  const provider = settings?.default_provider;
  const model = settings?.default_model ?? "qwen/qwen3-30b-a3b-instruct-2507";
  const needsKey = provider === "openrouter" && !settings?.has_openrouter_key;
  // Hardware-aware local recommendation (OS + RAM -> best tier that fits),
  // computed server-side. Shown as the "run locally" alternative.
  // Defensive: only render local_recommendation fields as STRINGS. The backend
  // once serialized a NULL legacy_alt as `{}` (empty object); rendering that
  // object as a React child threw "Objects are not valid as a React child"
  // (error #31) and blanked the whole page on >16 GB machines. Coerce every
  // field to a string and drop any non-string so a malformed payload can never
  // crash the render again.
  const asStr = (v: unknown): string => (typeof v === "string" ? v : typeof v === "number" ? String(v) : "");
  const rawRec = settings?.local_recommendation;
  const localRec = rawRec
    ? { headline: asStr(rawRec.headline), model: asStr(rawRec.model), legacy_alt: asStr(rawRec.legacy_alt) }
    : undefined;
  const tiers = settings?.ollama_model_tiers;
  const lightTier = asStr(tiers?.light) || "qwen3:8b";

  // Once a key is saved the inline form collapses to a confirmation.
  useEffect(() => { if (!needsKey) setSaved(false); }, [needsKey]);

  function dismiss() {
    setDismissed(true);
    try { localStorage.setItem(DISMISS_KEY, "1"); } catch { /* ignore */ }
  }

  async function saveKey() {
    const k = key.trim();
    if (!k || saving) return;
    setSaving(true); setErr("");
    try {
      await api.updateSettings({ openrouter_key: k });
      setSaved(true); setKey("");
    } catch (e: any) {
      setErr(e?.message ?? "save failed");
    } finally {
      setSaving(false);
    }
  }

  const go = (p: Page) => (e: React.MouseEvent) => {
    e.preventDefault();
    onNavigate?.(p);
  };

  // The banner is a slim always-visible-until-dismissed strip. The card is the
  // rich empty-state welcome (only when there are no messages yet).
  return (
    <>
      {!dismissed && (
        <div className="onboard-banner">
          <span>
            The agent runs on an <strong>OpenRouter model</strong> (free for a limited quota up to your credit)
            {provider === "openrouter" ? <> (<span className="mono">{model}</span>)</> : null}.
            {needsKey && <> Add a <a style={A} href="https://openrouter.ai/keys" target="_blank" rel="noreferrer">free OpenRouter key</a> to start, or </>}
            {!needsKey && <> You can also </>}
            run locally with <a style={A} href="#" onClick={go("help")}>Ollama</a>, or bring your own key/provider in{" "}
            <a style={A} href="#" onClick={go("settings")}>Settings</a>.
          </span>
          <button className="onboard-close" onClick={dismiss} aria-label="Dismiss">×</button>
        </div>
      )}

      {!hasMessages && (
        <div className="card onboard-card">
          <h3 style={{ marginTop: 0 }}>Welcome to the CelliVerse Agent</h3>
          {/* Round LXXX (audit #60): what this thing DOES, first.
              The card used to open on billing and never mention analysis at
              all -- a first screen explaining how to pay for something without
              saying what it is. The lines come from cv_capability_lines() over
              /api/intro so they cannot drift from the tools that back them. */}
          <p style={{ marginTop: 0, marginBottom: 8 }}>
            Describe a single-cell analysis in plain English and I run it on your data
            with the CelliVerse toolkit.
          </p>
          {canDo.length > 0 && (
            <ul className="onboard-can">
              {canDo.map((line) => <li key={line}>{line}</li>)}
            </ul>
          )}
          <p className="muted" style={{ marginBottom: 10 }}>
            Powered by an <strong>OpenRouter model</strong>
            {provider === "openrouter" ? <> (<span className="mono">{model}</span>)</> : null} by default — free for a limited quota up to your OpenRouter credit.
          </p>

          {needsKey && !saved && (
            <div className="onboard-key">
              <strong>1. Add a free OpenRouter API key to get started.</strong>
              <p className="muted" style={{ margin: "6px 0" }}>
                OpenRouter requires a key even for free models. Create one free at{" "}
                <a style={A} href="https://openrouter.ai/keys" target="_blank" rel="noreferrer">openrouter.ai/keys</a>,
                then paste it here (stored only on your machine in <span className="mono">~/.celliverse/config.json</span>).
              </p>
              <div className="onboard-keyrow">
                <input
                  type="password"
                  placeholder="sk-or-v1-…"
                  value={key}
                  onChange={(e) => setKey(e.target.value)}
                  onKeyDown={(e) => { if (e.key === "Enter") saveKey(); }}
                />
                <button className="btn" onClick={saveKey} disabled={!key.trim() || saving}>
                  {saving ? "Saving…" : "Save key"}
                </button>
              </div>
              {err && <div className="onboard-err">Save failed: {err}</div>}
            </div>
          )}
          {needsKey && saved && (
            <div className="onboard-ok">✓ Key saved. Ask the agent to cluster your data.</div>
          )}
          {!needsKey && provider === "openrouter" && (
            <div className="onboard-ok">✓ An OpenRouter key is configured.</div>
          )}

          <div className="onboard-alt">
            <strong>Prefer to run fully offline?</strong>{" "}
            {localRec ? (
              <>
                {localRec.headline} Install{" "}
                <a style={A} href="https://ollama.com/download" target="_blank" rel="noreferrer">Ollama</a> (free, local,
                no API key), then <span className="mono">ollama pull {localRec.model}</span> and pick the{" "}
                <span className="mono">ollama</span> provider in <a style={A} href="#" onClick={go("settings")}>Settings</a>.
                {localRec.legacy_alt && <> On this machine the lighter <span className="mono">{localRec.legacy_alt}</span> also works.</>}
                {" "}Or use LM Studio (Apple Silicon MLX models) — see the <a style={A} href="#" onClick={go("help")}>Help</a> page.
              </>
            ) : (
              <>
                Install{" "}
                <a style={A} href="https://ollama.com/download" target="_blank" rel="noreferrer">Ollama</a> (free, local,
                no API key), then <span className="mono">ollama pull {lightTier}</span> and pick the{" "}
                <span className="mono">ollama</span> provider in <a style={A} href="#" onClick={go("settings")}>Settings</a>.
                See the <a style={A} href="#" onClick={go("help")}>Help</a> page for step-by-step setup.
              </>
            )}
          </div>
          <div className="onboard-alt muted" style={{ fontSize: 12 }}>
            You can change the provider, model, or use your own API key at any time in{" "}
            <a style={A} href="#" onClick={go("settings")}>Settings</a>.
          </div>
        </div>
      )}
    </>
  );
}
