import { useEffect, useState } from "react";
import { api, ModelChoice, OllamaModels, Provider, ProviderModels, Settings } from "../api/client";
import ModelCombobox from "../components/ModelCombobox";

const PROVIDERS: Provider[] = [
  "ollama", "lmstudio", "openai", "anthropic", "gemini",
  "deepseek", "groq", "openrouter", "cerebras",
];

// Fallback local model tiers if the backend hasn't advertised them yet. Kept
// in sync with .cv_model_tiers in R/agent_utils.R.
const TIER_FALLBACK = { light: "qwen3:8b", recommended: "qwen3:30b-instruct", strong: "qwen3.6:35b" };

// Minimum total RAM (GB) per tier, for the RAM-gated tier button labels.
const TIER_MIN_RAM: Record<string, number> = { light: 8, recommended: 24, strong: 36 };

const MODEL_HINTS: Record<Provider, string> = {
  ollama: "qwen3:8b, qwen3:30b-instruct, llama3.1:8b, …",
  lmstudio: "qwen/qwen3-8b, ibm/granite-4-micro, …",
  openai: "gpt-4o, gpt-4o-mini, …",
  anthropic: "claude-3-5-sonnet-latest, …",
  gemini: "gemini-2.5-flash, gemini-flash-latest, gemini-2.5-pro, …",
  deepseek: "deepseek-chat, deepseek-reasoner",
  groq: "llama-3.3-70b-versatile, llama-3.1-8b-instant, …",
  openrouter: "qwen/qwen3-30b-a3b-instruct-2507, anthropic/claude-3-haiku, openai/gpt-4o-mini, …",
  cerebras: "llama-3.3-70b, llama3.1-8b, …",
};

// Sensible default model per provider (used when reconciling on a provider switch).
// NOTE: gemini-1.5-* were retired by Google in 2025; the default must be a
// currently-served model or the very first Gemini turn 404s. gemini-2.5-flash is
// a current GA, tool-capable, free-tier model; gemini-flash-latest is the
// hot-swapped alias if you prefer to always track the newest Flash.
// The DeepSeek/Groq/OpenRouter/Cerebras defaults are all tool-calling-capable
// models on their respective (OpenAI-compatible) endpoints.
export const PROVIDER_DEFAULT_MODEL: Record<Provider, string> = {
  ollama: "qwen3:8b",
  lmstudio: "qwen/qwen3-8b",
  openai: "gpt-4o-mini",
  anthropic: "claude-3-5-sonnet-latest",
  gemini: "gemini-2.5-flash",
  deepseek: "deepseek-chat",
  groq: "llama-3.3-70b-versatile",
  // OpenRouter default: Qwen3-30B-A3B-Instruct-2507 — strong tool-calling at a
  // low price point, and the same weights as the recommended Ollama tier
  // (qwen3:30b-instruct), so local->cloud switches keep a familiar model.
  openrouter: "qwen/qwen3-30b-a3b-instruct-2507",
  cerebras: "llama-3.3-70b",
};

// Recognize which provider a model-id string most likely belongs to, by shape.
// Returns null if it doesn't clearly match any single provider (in which case
// the UI shows no mismatch warning — we only warn when we're confident).
//
// Ordering matters:
//  - OpenRouter ids are "vendor/model" slugs (contain "/") and MUST be checked
//    before the openai/anthropic/gemini prefix tests, because "openai/gpt-4o"
//    starts with a vendor name but is an OpenRouter id.
//  - Groq and Cerebras both serve bare "llama-*"/"llama3*" ids, so a plain
//    Llama name is genuinely ambiguous between them -> return null (no warning)
//    rather than guess wrong. DeepSeek's "deepseek-*" ids are unambiguous.
export function providerOfModel(model: string): Provider | null {
  const m = (model || "").trim().toLowerCase();
  if (!m) return null;
  // "vendor/model" slug -> OpenRouter (check before prefix tests below).
  if (m.includes("/")) return "openrouter";
  if (m.startsWith("deepseek")) return "deepseek";
  if (m.startsWith("claude")) return "anthropic";
  if (m.startsWith("gemini")) return "gemini";
  if (m.startsWith("gpt") || m.startsWith("o1") || m.startsWith("o3")) return "openai";
  // Ollama tags look like "name:tag" (contain a colon) and aren't a cloud id.
  if (m.includes(":")) return "ollama";
  // Bare Llama ids (Groq/Cerebras) are ambiguous -> don't claim a provider.
  return null;
}

// Provider-aware wrapper: a "/" in the id is only an OpenRouter signal when the
// selected provider is a CLOUD one. Ollama community models
// ("dengcao/Qwen3-30B-A3B-Instruct-2507") and LM Studio ids ("qwen/qwen3-8b")
// legitimately contain slashes, so for local providers we never claim a
// different owner — the heuristic must not block a valid local id.
export function modelOwnerForProvider(model: string, provider: Provider): Provider | null {
  if (provider === "ollama" || provider === "lmstudio") return null;
  return providerOfModel(model);
}

// Normalize an Ollama model name for installed-list matching: lowercase and
// strip a trailing ":latest" (Ollama reports "qwen3:8b" pulled implicitly as
// "qwen3:8b:latest", and community repacks as "user/Name:latest").
export function normalizeOllamaName(x: string): string {
  return (x || "").trim().toLowerCase().replace(/:latest$/, "");
}

// Is `model` present in the installed list? Exact-normalized match, plus a
// community-repack fallback: "dengcao/Qwen3-30B-A3B-Instruct-2507" counts as
// the same weights as the official "qwen3:30b-instruct" tier id.
export function ollamaModelInstalled(model: string, installed: string[]): boolean {
  const want = normalizeOllamaName(model);
  if (!want) return false;
  const wantBase = want.includes("/") ? want.split("/").slice(1).join("/") : want;
  return installed.some((raw) => {
    const got = normalizeOllamaName(raw);
    if (got === want) return true;
    const gotBase = got.includes("/") ? got.split("/").slice(1).join("/") : got;
    // Community repack match only when one side has a namespace and the base
    // names match (avoids "qwen3:8b" matching "other/qwen3:8b" incorrectly is
    // fine — same base name IS the same model family).
    return got.includes("/") !== want.includes("/") && gotBase === wantBase;
  });
}

// Pure, unit-testable reconciliation: when the provider changes, decide which
// model id the field should show. Provider-aware: a "/" in the current id is
// only treated as "belongs to OpenRouter" when switching AWAY from a cloud
// provider — local providers (Ollama/LM Studio) legitimately use slash ids
// ("qwen/qwen3-8b", "dengcao/Qwen3-30B-A3B-Instruct-2507"), so a local->cloud
// switch must NEVER keep the local id just because it contains a slash.
//
// `lastModelByProvider` is the per-provider memory of the model last shown for
// each provider (most recently saved or selected). On a switch we restore the
// new provider's remembered model; if none exists we fall back to its default.
export function reconcileModelForProvider(
  nextProvider: Provider,
  currentModel: string,
  prevProvider: Provider,
  lastModelByProvider?: Partial<Record<Provider, string>>,
): string {
  const cur = (currentModel || "").trim();
  const remembered = (lastModelByProvider?.[nextProvider] || "").trim();
  const fallback = remembered || PROVIDER_DEFAULT_MODEL[nextProvider];
  if (!cur) return fallback;

  const prevIsLocal = prevProvider === "ollama" || prevProvider === "lmstudio";
  const nextIsLocal = nextProvider === "ollama" || nextProvider === "lmstudio";

  // Keep the current id only when it genuinely fits the NEW provider:
  //  - cloud -> same-cloud shape match (e.g. openrouter -> openrouter is a
  //    no-op switch; anthropic -> openrouter keeps nothing since claude ids
  //    have no slash and map to anthropic).
  //  - For a LOCAL next provider any non-empty id is plausible (free-text local
  //    ids have no reliable shape), but a local id typed under a local previous
  //    provider must not leak into a CLOUD provider.
  if (!prevIsLocal && providerOfModel(cur) === nextProvider) return cur;
  if (prevIsLocal && nextIsLocal) {
    // local -> local: keep only if the id was typed for the previous provider
    // and the user is switching between two local servers where the same id
    // may exist; otherwise prefer the remembered/default for the new provider.
    // In practice LM Studio and Ollama ids differ (slash vs colon), so only
    // keep when the shape is ambiguous (no slash AND no colon).
    if (!cur.includes("/") && !cur.includes(":")) return cur;
    return fallback;
  }
  // Any cross local<->cloud switch, or a cloud->cloud switch whose id does not
  // match the new provider: restore the remembered model, else the default.
  return fallback;
}

export default function SettingsPage() {
  const [s, setS] = useState<Settings | null>(null);
  const [provider, setProvider] = useState<Provider>("ollama");
  const [model, setModel] = useState("");
  const [temperature, setTemperature] = useState(0.2);
  // Round LXXX (audit #92): nucleus sampling. 0.9 mirrors cv_default_config().
  const [topP, setTopP] = useState(0.9);
  const [ollamaHost, setOllamaHost] = useState("http://localhost:11434");
  const [ollamaNumCtx, setOllamaNumCtx] = useState(8192);
  const [ollamaKeepAlive, setOllamaKeepAlive] = useState("30m");
  const [keys, setKeys] = useState<Record<string, string>>({
    openai_key: "", anthropic_key: "", gemini_key: "",
    deepseek_key: "", groq_key: "", openrouter_key: "", cerebras_key: "",
  });
  const [msg, setMsg] = useState<string>("");
  const [saving, setSaving] = useState(false);
  const [ollama, setOllama] = useState<OllamaModels | null>(null);
  const [lmstudioHost, setLmstudioHost] = useState("http://localhost:1234/v1");
  const [pullModelId, setPullModelId] = useState("");
  const [pullState, setPullState] = useState<{ job: string; status: string; message: string } | null>(null);

  // Model dropdown state. `modelCache` memoises each provider's fetched list for
  // this app-run so we don't re-hit the network on every provider toggle; the
  // Refresh button (and a mid-session key save) force a re-fetch.
  const [modelInfo, setModelInfo] = useState<ProviderModels | null>(null);
  const [modelsLoading, setModelsLoading] = useState(false);
  const [modelCache, setModelCache] = useState<Record<string, ProviderModels>>({});
  // Per-provider model memory: the model last shown for each provider this
  // app-run. Seeded from the saved config; updated on every manual model edit
  // and on save. Restored when the user switches back to a provider.
  const [lastModelByProvider, setLastModelByProvider] = useState<Partial<Record<Provider, string>>>({});

  useEffect(() => {
    api.getSettings().then((cfg) => {
      setS(cfg);
      setProvider(cfg.default_provider);
      setModel(cfg.default_model);
      setLastModelByProvider({ [cfg.default_provider]: cfg.default_model });
      setTemperature(cfg.temperature ?? 0.2);
      setTopP(cfg.top_p ?? 0.9);
      setOllamaHost(cfg.ollama_host ?? "http://localhost:11434");
      setLmstudioHost(cfg.lmstudio_host ?? "http://localhost:1234/v1");
      setOllamaNumCtx(cfg.ollama_num_ctx ?? 8192);
      setOllamaKeepAlive(cfg.ollama_keep_alive ?? "30m");
    }).catch((e) => setMsg(`Failed to load settings: ${e.message}`));
  }, []);

  // Fetch the selectable model list for a provider. Cached per app-run unless
  // force=true (Refresh button / a just-saved key). Never throws into the UI.
  function loadModels(p: Provider, force = false) {
    if (!force && modelCache[p]) { setModelInfo(modelCache[p]); return; }
    setModelsLoading(true);
    api.providerModels(p)
      .then((info) => {
        setModelInfo(info);
        setModelCache((c) => ({ ...c, [p]: info }));
      })
      .catch(() => setModelInfo({ provider: p, reachable: false, source: "curated", models: [], note: "Could not reach the model service; you can still type a model id." }))
      .finally(() => setModelsLoading(false));
  }

  // Fetch on Settings-open and on every provider switch (uses cache when warm).
  useEffect(() => { if (provider) loadModels(provider); /* eslint-disable-next-line */ }, [provider]);

  // Fetch installed local models so we can flag a not-yet-pulled Ollama model.
  // Never throws into the UI: on failure we just treat Ollama as unreachable.
  function refreshOllama() {
    api.ollamaModels()
      .then((m) => setOllama(m))
      .catch(() => setOllama({ reachable: false, installed: [], tiers: TIER_FALLBACK, has_light: false, has_recommended: false, has_strong: false }));
  }
  useEffect(() => { if (provider === "ollama") refreshOllama(); }, [provider]);

  // Download a local model in the background (ollama pull / lms get --yes) and
  // poll the job until it finishes. On success, refresh the provider's list.
  async function startPull(p: "ollama" | "lmstudio", id: string) {
    const m = id.trim();
    if (!m) return;
    try {
      const sess = (await api.listSessions()).sessions?.[0]?.id;
      let sessionId = sess;
      if (!sessionId) sessionId = (await api.newSession()).session_id;
      const job = await api.pullModel(sessionId, p, m);
      setPullState({ job: job.job_id, status: "running", message: `Downloading ${m}…` });
      const t = setInterval(async () => {
        try {
          const d = await api.jobStatus(sessionId, job.job_id);
          if (d.status === "running") {
            setPullState({ job: job.job_id, status: "running", message: d.message || `Downloading ${m}…` });
          } else {
            clearInterval(t);
            if (d.status === "done") {
              setPullState({ job: job.job_id, status: "done", message: `${m} downloaded.` });
              if (p === "ollama") refreshOllama(); else loadModels("lmstudio", true);
            } else {
              setPullState({ job: job.job_id, status: "error", message: d.error || "Download failed." });
            }
          }
        } catch { /* keep polling */ }
      }, 1500);
    } catch (e: any) {
      setPullState({ job: "", status: "error", message: e.message });
    }
  }

  // Recommended tier ids: prefer what the backend advertises, else fall back.
  const tiers = s?.ollama_model_tiers ?? ollama?.tiers ?? TIER_FALLBACK;

  // Options shown in the combobox. For cloud providers this is the fetched list
  // (live or curated); for Ollama it's ALL installed local models, annotated so
  // the tiers stand out; for LM Studio it's ALL models the server has
  // downloaded (from /v1/models). A typed slug is always accepted regardless.
  const modelChoices: ModelChoice[] =
    provider === "ollama"
      ? (ollama?.installed ?? []).map((id) => ({
          id,
          label: id === tiers.light ? "light tier"
               : id === tiers.recommended ? "recommended tier"
               : id === tiers.strong ? "strong tier" : undefined,
        }))
      : (modelInfo?.provider === provider ? modelInfo.models : []);

  // Is the model list live or a curated fallback? (cloud providers + lmstudio)
  const modelSource = provider === "ollama" ? null : modelInfo?.provider === provider ? modelInfo.source : null;
  const modelNote = provider === "ollama" ? null : modelInfo?.provider === provider ? modelInfo.note : null;

  function onProviderChange(next: Provider) {
    if (next === provider) return;
    // Remember the model being left behind so switching back restores it.
    const memory = { ...lastModelByProvider, [provider]: model.trim() || lastModelByProvider[provider] };
    setLastModelByProvider(memory);
    // Reconcile the (free-text) model field so we never carry a stale model
    // from the previous provider into the new one. Provider-aware: a local
    // (Ollama/LM Studio) slash id must NOT survive a switch to a cloud
    // provider just because it contains "/". Restore the new provider's
    // remembered model when available, else its default.
    setModel((cur) => reconcileModelForProvider(next, cur, provider, memory));
    setProvider(next);
    setMsg("");
  }

  // Track manual model edits so the per-provider memory reflects the latest
  // choice even before Save is pressed.
  function onModelChange(next: string) {
    setModel(next);
    const v = next.trim();
    if (v) setLastModelByProvider((m) => ({ ...m, [provider]: v }));
  }

  // A model whose shape clearly belongs to a DIFFERENT provider than selected.
  // Provider-aware: local providers (ollama/lmstudio) never flag slash ids.
  const modelOwner = modelOwnerForProvider(model, provider);
  const modelMismatch = model.trim() !== "" && modelOwner !== null && modelOwner !== provider;

  // Non-blocking "not pulled yet" flag: only when we could actually enumerate
  // installed models (Ollama reachable). If the daemon is down we can't tell,
  // so we DON'T nag. This never blocks Save — selection is always persisted.
  // Matching normalizes ":latest"/case and accepts community repacks.
  const modelNotInstalled =
    provider === "ollama" &&
    model.trim() !== "" &&
    !!ollama &&
    ollama.reachable &&
    !ollamaModelInstalled(model.trim(), ollama.installed);
  const pullCmd = `ollama pull ${model.trim()}`;

  // LM Studio: is the typed model among the server's downloaded models? Only
  // flag when we could actually enumerate them (server reachable).
  const lmstudioModels = provider === "lmstudio" && modelInfo?.provider === "lmstudio" ? modelInfo.models : [];
  const lmstudioReachable = provider === "lmstudio" && modelInfo?.provider === "lmstudio" && modelInfo.reachable;
  const lmstudioNotDownloaded =
    provider === "lmstudio" &&
    model.trim() !== "" &&
    lmstudioReachable &&
    !lmstudioModels.some((m) => m.id === model.trim());

  async function save() {
    // A provider/shape mismatch is a WARNING, not a hard block: the heuristic
    // can misfire on unusual-but-valid ids, and the user's explicit selection
    // should always be persisted. We still surface the hint inline.
    setSaving(true); setMsg("");
    // Only send non-empty keys so we never blank an existing key by accident.
    const patch: any = {
      default_provider: provider,
      default_model: model,
      temperature: Number(temperature),
      top_p: Number(topP),
      ollama_host: ollamaHost,
      lmstudio_host: lmstudioHost,
      ollama_num_ctx: Number(ollamaNumCtx),
      ollama_keep_alive: ollamaKeepAlive,
    };
    // Did the user just enter a key for the CURRENTLY selected provider? If so
    // we re-fetch its model list after saving so the live catalog appears.
    const keyForCurrent = provider !== "ollama" && provider !== "lmstudio" && !!keys[`${provider}_key`]?.trim();
    for (const k of Object.keys(keys)) if (keys[k].trim()) patch[k] = keys[k].trim();
    try {
      const updated = await api.updateSettings(patch);
      setS(updated);
      // Persist the saved model as this provider's remembered choice.
      if (model.trim()) setLastModelByProvider((m) => ({ ...m, [provider]: model.trim() }));
      setKeys({
        openai_key: "", anthropic_key: "", gemini_key: "",
        deepseek_key: "", groq_key: "", openrouter_key: "", cerebras_key: "",
      });
      setMsg("Saved. Changes take effect immediately — no restart needed.");
      // Mid-session key entry -> force a fresh (now-authenticated) model fetch.
      if (keyForCurrent) loadModels(provider, true);
    } catch (e: any) {
      setMsg(`Save failed: ${e.message}`);
    } finally {
      setSaving(false);
    }
  }

  if (!s) return <div className="muted">{msg || "Loading settings…"}</div>;

  return (
    <div style={{ maxWidth: 560 }}>
      <div className="card">
        <h3>Active model</h3>
        <div className="form-row">
          {/* Round LXXIX (audit #48): htmlFor/id on every control in this
              page. Settings had eleven bare labels, so a screen reader read the
              whole tab as a column of unlabelled edit fields, and clicking a
              label focused nothing. */}
          <label htmlFor="set-provider">Provider</label>
          <select id="set-provider" value={provider} onChange={(e) => onProviderChange(e.target.value as Provider)}>
            {PROVIDERS.map((p) => <option key={p} value={p}>{p}</option>)}
          </select>
        </div>
        <div className="form-row">
          <label htmlFor="set-model">Model</label>
          <ModelCombobox
            inputId="set-model"
            value={model}
            onChange={onModelChange}
            models={modelChoices}
            loading={provider === "ollama" ? false : modelsLoading}
            placeholder={MODEL_HINTS[provider]}
            onRefresh={provider === "ollama" ? refreshOllama : () => loadModels(provider, true)}
          />
          <span className="hint">
            Pick from the list or type any model id. Examples: {MODEL_HINTS[provider]}
          </span>
          {modelSource === "live" && (
            <span className="hint">Live list from {provider}{modelChoices.length ? ` (${modelChoices.length} models)` : ""}. Use Refresh to update.</span>
          )}
          {modelSource === "curated" && (
            <span className="hint">
              {modelNote || "Showing a curated shortlist. Add your API key and click Refresh to load the full live list."}
              {" "}You can still type any model id.
            </span>
          )}
          {modelMismatch && (
            <span className="err-text" style={{ fontSize: 12 }}>
              This looks like a {modelOwner} model, but the provider is {provider}. Use e.g. {PROVIDER_DEFAULT_MODEL[provider]}.
            </span>
          )}
        </div>
        <div className="form-row">
          <label htmlFor="set-temperature">Temperature ({temperature})</label>
          <input id="set-temperature" type="range" min={0} max={1} step={0.05} value={temperature}
                 onChange={(e) => setTemperature(parseFloat(e.target.value))} />
        </div>
        {/* Round LXXX (audit #92): nucleus sampling was not set at all, so every
            request went out at the provider default of 1.0 — the entire tail
            kept, however low the temperature. */}
        <div className="form-row">
          <label htmlFor="set-top-p">Top-p ({topP})</label>
          <input id="set-top-p" type="range" min={0.5} max={1} step={0.05} value={topP}
                 onChange={(e) => setTopP(parseFloat(e.target.value))} />
          <span className="hint">
            Cuts the unlikely tail of each token choice. Temperature rescales probabilities;
            only this removes them. Lower is more reliable at calling tools and blander in prose —
            0.9 is the default.
          </span>
        </div>
        {provider === "ollama" && (
          <>
            <div className="form-row">
              {/* A group of buttons, not one control, so it takes its name
                  from aria-labelledby rather than a htmlFor that would have
                  had to pick one of the three arbitrarily. */}
              <label id="set-tier-label">Local model tier</label>
              <div role="group" aria-labelledby="set-tier-label" style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
                <button type="button" className="btn secondary" onClick={() => onModelChange(tiers.light)}>
                  Light — {tiers.light}
                </button>
                <button type="button" className="btn secondary" onClick={() => onModelChange(tiers.recommended)}>
                  Recommended — {tiers.recommended}
                </button>
                <button type="button" className="btn secondary" onClick={() => onModelChange(tiers.strong)}>
                  Strong — {tiers.strong}
                </button>
              </div>
              <span className="hint">
                Light needs ~{TIER_MIN_RAM.light} GB RAM; recommended needs ~{TIER_MIN_RAM.recommended} GB;
                strong needs ~{TIER_MIN_RAM.strong} GB. Bigger tiers call tools more reliably.
                On ≤16 GB machines the legacy <code>qwen2.5:7b-instruct</code> is a lighter fallback.
              </span>
            </div>
            <div className="form-row">
              <label htmlFor="set-pull-ollama">Download model</label>
              <div style={{ display: "flex", gap: 8, flex: 1 }}>
                <input id="set-pull-ollama" value={pullModelId} onChange={(e) => setPullModelId(e.target.value)}
                       placeholder={`e.g. ${tiers.recommended}`} style={{ flex: 1 }} />
                <button type="button" className="btn secondary"
                        disabled={!pullModelId.trim() || pullState?.status === "running"}
                        onClick={() => startPull("ollama", pullModelId)}>
                  {pullState?.status === "running" ? "Downloading…" : "Download"}
                </button>
              </div>
              {pullState && (
                <span className={pullState.status === "error" ? "err-text" : "hint"} style={{ fontSize: 12 }}>
                  {pullState.message}
                </span>
              )}
              <span className="hint">Runs <code>ollama pull</code> in the background; the list above refreshes when it finishes.</span>
            </div>
            {modelNotInstalled && (
              <div className="form-row">
                <span className="err-text" style={{ fontSize: 12 }}>
                  "{model.trim()}" is not installed locally yet. Your selection is still saved — to
                  actually use it, download it above or run in R/terminal:&nbsp;<code>{pullCmd}</code>, then reload.
                  {" "}
                  <a href="#" onClick={(e) => { e.preventDefault(); refreshOllama(); }}>Re-check</a>
                </span>
              </div>
            )}
            {ollama && !ollama.reachable && (
              <div className="form-row">
                <span className="hint" style={{ fontSize: 12 }}>
                  Ollama daemon not reachable at {ollamaHost} — can't list installed models. Start
                  Ollama (or set the host) to enable model checks.{" "}
                  <a href="#" onClick={(e) => { e.preventDefault(); refreshOllama(); }}>Re-check</a>
                </span>
              </div>
            )}
            <div className="form-row">
              <label htmlFor="set-ollama-host">Ollama host</label>
              <input id="set-ollama-host" value={ollamaHost} onChange={(e) => { setOllamaHost(e.target.value); }} onBlur={refreshOllama} />
            </div>
            <div className="form-row">
              <label htmlFor="set-num-ctx">Context window (num_ctx)</label>
              <input id="set-num-ctx" type="number" min={2048} step={1024} value={ollamaNumCtx}
                     onChange={(e) => setOllamaNumCtx(parseInt(e.target.value || "8192", 10))} />
              <span className="hint">
                Ollama defaults to 4096 tokens, which silently truncates the agent's instructions.
                8192+ is recommended for reliable tool-calling (needs enough RAM).
              </span>
            </div>
            <div className="form-row">
              <label htmlFor="set-keep-alive">Keep model loaded</label>
              <input id="set-keep-alive" value={ollamaKeepAlive} onChange={(e) => setOllamaKeepAlive(e.target.value)} placeholder="30m" />
              <span className="hint">
                How long Ollama keeps the model in memory between requests (e.g. 30m, 1h, -1 = forever).
                Avoids a reload pause between agent steps.
              </span>
            </div>
          </>
        )}
        {provider === "lmstudio" && (
          <>
            <div className="form-row">
              <label htmlFor="set-lmstudio-host">LM Studio host</label>
              <input id="set-lmstudio-host" value={lmstudioHost} onChange={(e) => setLmstudioHost(e.target.value)}
                     onBlur={() => loadModels("lmstudio", true)} placeholder="http://localhost:1234/v1" />
              <span className="hint">
                OpenAI-compatible endpoint of the LM Studio local server (Developer page → Start Server,
                default port 1234). No API key needed. You can also point this at another
                OpenAI-compatible local server (llama.cpp :8080/v1, Jan :1337/v1).
              </span>
            </div>
            {modelInfo?.provider === "lmstudio" && !modelInfo.reachable && (
              <div className="form-row">
                <span className="hint" style={{ fontSize: 12 }}>
                  {modelInfo.note || (
                    <>LM Studio server not reachable at {lmstudioHost}.</>
                  )}{" "}
                  <a href="#" onClick={(e) => { e.preventDefault(); loadModels("lmstudio", true); }}>Re-check</a>
                </span>
              </div>
            )}
            {lmstudioNotDownloaded && (
              <div className="form-row">
                <span className="err-text" style={{ fontSize: 12 }}>
                  "{model.trim()}" is not downloaded in LM Studio yet. Download it below or in the
                  LM Studio app, then Refresh.
                </span>
              </div>
            )}
            <div className="form-row">
              <label htmlFor="set-pull-lmstudio">Download model</label>
              <div style={{ display: "flex", gap: 8, flex: 1 }}>
                <input id="set-pull-lmstudio" value={pullModelId} onChange={(e) => setPullModelId(e.target.value)}
                       placeholder="e.g. qwen/qwen3-8b or ibm/granite-4-micro" style={{ flex: 1 }} />
                <button type="button" className="btn secondary"
                        disabled={!pullModelId.trim() || pullState?.status === "running"}
                        onClick={() => startPull("lmstudio", pullModelId)}>
                  {pullState?.status === "running" ? "Downloading…" : "Download"}
                </button>
              </div>
              {pullState && (
                <span className={pullState.status === "error" ? "err-text" : "hint"} style={{ fontSize: 12 }}>
                  {pullState.message}
                </span>
              )}
              <span className="hint">
                Runs <code>lms get --yes</code> in the background (requires LM Studio installed once).
                On Apple Silicon, MLX builds (e.g. <code>--mlx</code> picks in the LM Studio app) are fastest.
              </span>
            </div>
          </>
        )}
      </div>

      <div className="card">
        <h3>API keys</h3>
        <p className="muted" style={{ fontSize: 12 }}>
          Keys are stored server-side in ~/.celliverse/config.json and never sent back to the browser.
          Leave blank to keep the existing key.
        </p>
        {/* Round LXXX (audit #66): what actually leaves the machine, stated
            where the decision is made rather than only in the README. The
            README's old claim — "your data never leaves it" — was the
            most-read sentence in the project and was wrong for the default
            cloud path: marker GENE SYMBOLS are sent. */}
        <p className="muted" style={{ fontSize: 12 }}>
          <strong>What is sent to a cloud provider:</strong> your messages, object summaries
          (counts, cluster ids, column names), and any gene symbols or result tables the agent
          produces — ranked markers, purity scores, annotation labels. Expression matrices and
          barcodes are never sent. If a gene list is embargoed, use <strong>Ollama</strong> or{" "}
          <strong>LM Studio</strong> above; those run on your machine and send nothing at all.
        </p>
        {(["openai", "anthropic", "gemini", "deepseek", "groq", "openrouter", "cerebras"] as const).map((p) => {
          const has = (s as any)[`has_${p}_key`] as boolean;
          return (
            <div className="form-row" key={p}>
              <label htmlFor={`set-key-${p}`}>{p} key <span className={`badge ${has ? "on" : "off"}`}>{has ? "set" : "not set"}</span></label>
              <input id={`set-key-${p}`} type="password" value={keys[`${p}_key`]} placeholder={has ? "•••••• (keep)" : "paste key"}
                     onChange={(e) => setKeys({ ...keys, [`${p}_key`]: e.target.value })} />
            </div>
          );
        })}
      </div>

      <button className="btn" onClick={save} disabled={saving}>{saving ? "Saving…" : "Save settings"}</button>
      {msg && <p style={{ marginTop: 10 }} className={msg.includes("fail") || msg.includes("Failed") ? "err-text" : "muted"}>{msg}</p>}
    </div>
  );
}
