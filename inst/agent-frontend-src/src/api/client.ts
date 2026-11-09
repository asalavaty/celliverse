// Typed API client for the CelliVerse Agent Plumber backend.
// Every call targets a route defined in inst/plumber/plumber.R.
// The base is relative ("") because the R server serves both the SPA and /api.

export type Provider =
  | "ollama"
  | "lmstudio"
  | "openai"
  | "anthropic"
  | "gemini"
  | "deepseek"
  | "groq"
  | "openrouter"
  | "cerebras";

export interface ModelTiers {
  light: string;
  recommended: string;
  strong: string;
}

// Hardware-aware local recommendation computed by the backend (OS + RAM ->
// best tier that fits). Surfaced on the onboarding card.
export interface LocalRecommendation {
  os: string;
  ram_gb: number | null;
  tier: string;
  model: string;
  min_ram_gb: number;
  est_speed: string;
  headline: string;
  legacy_alt?: string | null;
}

export interface Settings {
  default_provider: Provider;
  default_model: string;
  temperature: number;
  // Round LXXX (audit #92): nucleus sampling and an optional fixed seed.
  top_p?: number;
  seed?: number | null;
  ollama_host: string;
  lmstudio_host?: string;
  ollama_keep_alive?: string;
  ollama_num_ctx?: number;
  has_openai_key: boolean;
  has_anthropic_key: boolean;
  has_gemini_key: boolean;
  has_deepseek_key: boolean;
  has_groq_key: boolean;
  has_openrouter_key: boolean;
  has_cerebras_key: boolean;
  // Recommended local model tiers (light/recommended/strong) advertised by the backend.
  ollama_model_tiers?: ModelTiers;
  ollama_model_tiers_legacy?: string[];
  ollama_tier_min_ram_gb?: Record<string, number>;
  local_recommendation?: LocalRecommendation;
  [k: string]: unknown;
}

// GET /api/ollama/models — which local models are actually pulled, plus the
// recommended tier ids and convenience presence flags. `reachable` is false
// when the Ollama daemon is not running / not installed.
export interface OllamaModels {
  reachable: boolean;
  installed: string[];
  tiers: ModelTiers;
  has_light: boolean;
  has_recommended: boolean;
  has_strong: boolean;
}

// One selectable model returned by GET /api/models.
export interface ModelChoice {
  id: string;
  label?: string;
  context_length?: number | null;
  free?: boolean;
}

// GET /api/models?provider=<p> — the model list for a provider.
//  - source="live"    : fetched from the provider's own endpoint (key present,
//                        or OpenRouter's public list).
//  - source="curated" : a curated fallback shortlist (offline / no key / fetch
//                        failed). `note` explains why and how to get the live list.
// A typed model id can always be entered manually regardless of this list.
export interface ProviderModels {
  provider: Provider;
  reachable: boolean;
  source: "live" | "curated";
  models: ModelChoice[];
  note?: string | null;
  // Present only for the ollama provider (mirrors OllamaModels tiers).
  tiers?: ModelTiers;
}

export interface ObjectDescriptor {
  handle: string;
  type: string;
  summary: string;
  [k: string]: unknown;
}

// Round LXXXI (E2): one entry in the saved-prompts rail.
//
// `builtin` comes from the SERVER rather than being inferred here from the
// `builtin:` id prefix, because it decides what the remove button does --
// hiding a starter is not the same act as deleting something the user wrote --
// and that rule must live in one place. See cv_prompts_all() in R.
export interface SavedPrompt {
  id: string;
  label: string;
  text: string;
  category: string;
  builtin: boolean;
}

export interface SavedPrompts {
  prompts: SavedPrompt[];
  categories: string[];
  // How many built-in starters are currently hidden. The rail offers to put
  // them back only when this is above zero.
  hidden: number;
  max: number;
}

// ---- Results artifacts (GET /api/artifacts) ---------------------------------
// The Results tab is driven by a server-built manifest of every downloadable
// file in the session artifacts dir: rendered figures (svg/png grouped by
// stem), CSV tables, and — new in Round IV — one portable .rds per server-side
// object (plus convenience .txt exports), each carrying its provenance so the
// user knows which handle/tool produced it.
export interface ArtifactFormat {
  format: string;        // "svg" | "png" | "pdf"
  filename: string;
  url: string;
  size?: number;
}
export interface ResultArtifact {
  kind: "figure" | "rds" | "table" | "text" | "other";
  // figure entries (svg/png/pdf grouped under one stem):
  name?: string;
  formats?: ArtifactFormat[];
  primary?: string;
  thumb?: string;
  // file entries (rds / table / text / other):
  filename?: string;
  url?: string;
  size?: number;
  // object provenance (rds + object-derived txt/csv):
  handle?: string;
  type?: string;
  summary?: string;
  source?: string;
  // Round LIV: the object is indexed but its file has not been written yet.
  // The server writes it on the first request for `url`, so the link still
  // works — this flag only changes what the row SAYS, so the user understands
  // why a click takes a few seconds. Absent means the file is already on disk.
  pending?: boolean;
}
export interface ResultsManifest {
  session: string;
  generated: string;
  n: number;
  artifacts: ResultArtifact[];
}

export interface ToolSpec {
  name: string;
  description: string;
  parameters?: unknown;
}

export interface JobInfo {
  id: string;
  tool: string;
  status: string;
  progress?: number;
  message?: string;
}

// An API error that carries the server's optional technical `detail` alongside
// the calm user-facing sentence, so the UI can put it behind a toggle.
//
// Round LXIV (D6). Round LXII defined the contract -- cv_api_err(message,
// status, detail): "a calm sentence in `error`, the raw cause in `detail`,
// revealed by the UI behind a toggle rather than shown by default" -- and
// implemented and tested the SERVER half. The client half never existed:
// errMsg() read j.error and returned a bare string, so `detail` was computed,
// serialized over the wire, and dropped on the floor on arrival. Nothing in
// src/ read it and there was no toggle anywhere.
export class CvApiError extends Error {
  detail?: string;
  status?: number;
  // Round LXXXIV: the server sets this on the one failure that has a specific
  // remedy -- a body R cannot hold in one piece -- so the UI can act instead of
  // asking the user to read.
  usePathBox?: boolean;
  constructor(message: string, detail?: string, status?: number, usePathBox?: boolean) {
    super(message);
    this.name = "CvApiError";
    this.detail = detail;
    this.status = status;
    this.usePathBox = usePathBox;
  }
}

// Pull the server's human-readable error message out of an error response body
// (the API wraps errors as {ok:false, error:"...", detail?:"..."}), falling back
// to a generic status string. Lets the UI show e.g. the duplicate-name warning
// verbatim, and keeps the technical cause available for the details toggle.
async function apiError(r: Response, fallback: string): Promise<CvApiError> {
  let message = `${fallback} -> ${r.status}`;
  let detail: string | undefined;
  let usePathBox: boolean | undefined;
  try {
    const j = await r.clone().json();
    if (j && typeof j.error === "string" && j.error) message = j.error;
    // Only a non-empty string counts: the server already drops an empty detail
    // (cv_api_err uses nzchar), and a toggle that opens onto nothing is worse
    // than no toggle.
    if (j && typeof j.detail === "string" && j.detail) detail = j.detail;
    if (j && j.use_path_box === true) usePathBox = true;
  } catch { /* not JSON */ }
  return new CvApiError(message, detail, r.status, usePathBox);
}

async function jget<T>(path: string): Promise<T> {
  const r = await fetch(`/api${path}`, { headers: { Accept: "application/json" } });
  if (!r.ok) throw await apiError(r, `GET ${path}`);
  return r.json() as Promise<T>;
}

async function jpost<T>(path: string, body?: unknown): Promise<T> {
  const r = await fetch(`/api${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Accept: "application/json" },
    body: body === undefined ? undefined : JSON.stringify(body),
  });
  if (!r.ok) throw await apiError(r, `POST ${path}`);
  return r.json() as Promise<T>;
}

// Round LXVIII (audit #67): the first DELETE in this client. It goes through
// the same apiError() path as the others, so a refusal -- 409 when a tool is
// still running, 400 on an id the server will not act on -- arrives as a
// CvApiError carrying the server's own sentence. That is what lets "Clear
// history" say which conversations survived and why, instead of reporting a
// clean sweep it did not perform.
async function jdel<T>(path: string): Promise<T> {
  const r = await fetch(`/api${path}`, {
    method: "DELETE",
    headers: { Accept: "application/json" },
  });
  if (!r.ok) throw await apiError(r, `DELETE ${path}`);
  return r.json() as Promise<T>;
}

// ---- unwrap helper: the API wraps payloads as {ok:true, data:...} -----------
function unwrap<T>(resp: any): T {
  if (resp && typeof resp === "object" && "ok" in resp && "data" in resp) return resp.data as T;
  return resp as T;
}

export const api = {
  health: () => jget<any>("/health").then((r) => unwrap<any>(r)),

  getSettings: () => jget<any>("/settings").then((r) => unwrap<Settings>(r)),
  updateSettings: (patch: Partial<Settings> & Record<string, unknown>) =>
    jpost<any>("/settings", patch).then((r) => unwrap<Settings>(r)),

  // Which local Ollama models are installed (defensive; reachable=false if the
  // daemon is down). Used by Settings to flag a not-yet-pulled model.
  ollamaModels: () => jget<any>("/ollama/models").then((r) => unwrap<OllamaModels>(r)),

  // Selectable models for a provider (live when a key is present / OpenRouter's
  // public list; otherwise a curated fallback shortlist). Drives the Settings
  // model dropdown. Never blocks manual entry of a typed model id.
  providerModels: (provider: Provider) =>
    jget<any>(`/models?provider=${encodeURIComponent(provider)}`).then((r) => unwrap<ProviderModels>(r)),

  registry: () => jget<any>("/registry").then((r) => unwrap<{ tools: ToolSpec[]; metadata: any }>(r)),

  newSession: () => jpost<any>("/session", {}).then((r) => unwrap<{ session_id: string }>(r)),
  getSession: (id: string) => jget<any>(`/session/${encodeURIComponent(id)}`).then((r) => unwrap<any>(r)),
  listSessions: () => jget<any>("/sessions").then((r) => unwrap<{ sessions: any[] }>(r)),
  // Deletes the transcript. The session's artifacts/ directory is KEPT server
  // side -- a figure already on disk must not vanish because the conversation
  // around it was cleared.
  deleteSession: (id: string) =>
    jdel<any>(`/session/${encodeURIComponent(id)}`).then((r) =>
      unwrap<{ session_id: string; deleted: boolean }>(r)),

  listObjects: (session: string) =>
    jget<any>(`/objects?session=${encodeURIComponent(session)}`).then((r) => unwrap<{ objects: ObjectDescriptor[] }>(r)),
  objectDetail: (session: string, handle: string) =>
    jget<any>(`/objects/${encodeURIComponent(handle)}?session=${encodeURIComponent(session)}`).then((r) => unwrap<any>(r)),
  loadObject: (session: string, path: string, name?: string) =>
    jpost<any>("/objects/load", { session, path, name }).then((r) => unwrap<any>(r)),
  // Round LXXX (audit #63): the bundled demo dataset (SeuratObject::pbmc_small,
  // 80 cells) so a configured user has something to run without going to find
  // a file first.
  loadDemo: (session: string) =>
    jpost<any>(`/objects/demo?session=${encodeURIComponent(session)}`, { session })
      .then((r) => unwrap<{ handle: string; descriptor: ObjectDescriptor; note?: string }>(r)),
  // Round LXXX (audit #60/#61/#62): what the agent can do, what to try, and
  // which formats it reads -- all from R, so the screen cannot drift from the
  // parser or from cv_supported_formats().
  intro: () =>
    jget<any>("/intro").then((r) => unwrap<{
      examples: { label: string; text: string }[];
      formats: string[];
      can_do: string[];
    }>(r)),
  // Round LXXXII: what a browser upload of `bytes` would cost this machine, and
  // what it has. Asked when the file is CHOSEN -- File.size is known without
  // reading a byte -- so the answer arrives before the upload rather than after
  // several GB have failed. Advice, never a limit: the Upload button stays live.
  uploadAdvice: (bytes: number) =>
    jget<any>(`/upload-advice?bytes=${encodeURIComponent(String(Math.round(bytes)))}`)
      .then((r) => unwrap<{
        bytes: number; needs_mb: number; available_mb: number;
        advisable: boolean; message?: string | null;
      }>(r)),

  // Round LXXXIII: build a Seurat from a matrix already loaded server-side.
  // Same code path as the `toSeurat` tool -- one implementation, three doors.
  toSeurat: (session: string, handle: string, name?: string) =>
    jpost<any>("/objects/to-seurat", { session, handle, name })
      .then((r) => unwrap<{ handle: string; descriptor: ObjectDescriptor; note?: string }>(r)),

  // Round LXXXI (E2): the saved-prompts rail. Persisted server-side in
  // ~/.celliverse/prompts.json so a favourite is there in a different browser,
  // a private window and on a second machine pointed at the same R server --
  // which is what "across all sessions" has to mean. Every call returns the
  // whole list, so two open tabs cannot drift.
  prompts: () => jget<any>("/prompts").then((r) => unwrap<SavedPrompts>(r)),
  addPrompt: (label: string, text: string, category: string) =>
    jpost<any>("/prompts", { label, text, category }).then((r) => unwrap<SavedPrompts>(r)),
  removePrompt: (id: string) =>
    jpost<any>("/prompts/remove", { id }).then((r) => unwrap<SavedPrompts>(r)),
  restorePrompts: () => jpost<any>("/prompts/restore", {}).then((r) => unwrap<SavedPrompts>(r)),

  // Round LXXX (audit #71): the LOCAL turn summary. Nothing leaves the machine.
  logSummary: (days = 7) =>
    jget<any>(`/log-summary?days=${days}`).then((r) => unwrap<{
      summary: Record<string, number> | null;
      enabled: boolean; log_dir: string; keep_days: number;
    }>(r)),

  // Results manifest (read-only; cheap to poll). Lists every downloadable file
  // in the session: grouped figures, CSV tables, per-object .rds + .txt exports.
  listArtifacts: (session: string) =>
    jget<any>(`/artifacts?session=${encodeURIComponent(session)}`).then((r) => unwrap<ResultsManifest>(r)),
  // Round LXXV (D5): one page of a table artifact, re-sliced server-side from
  // the CSV cv_render_table() already wrote. `name` is the artifact's
  // csv.filename. Out-of-range pages clamp server-side rather than erroring.
  tablePage: (session: string, name: string, page: number, pageSize: number) =>
    jget<any>(
      `/table?session=${encodeURIComponent(session)}&name=${encodeURIComponent(name)}` +
      `&page=${page}&page_size=${pageSize}`
    ).then((r) => unwrap<{
      columns: string[]; nrow: number; ncol: number;
      page: number; page_size: number; n_pages: number; rows: any[];
    }>(r)),
  // Direct download URL for the "download everything" zip (used as an <a href>).
  artifactsZipUrl: (session: string) =>
    `/api/artifacts.zip?session=${encodeURIComponent(session)}`,

  listJobs: (session: string) =>
    jget<any>(`/jobs?session=${encodeURIComponent(session)}`).then((r) => unwrap<{ jobs: JobInfo[] }>(r)),
  jobStatus: (session: string, id: string) =>
    jget<any>(`/jobs/${encodeURIComponent(id)}?session=${encodeURIComponent(session)}`).then((r) => unwrap<any>(r)),
  // Batch 8b: this sent an EMPTY body and no session, while the route needs one
  // to find the job -- so the "Cancel job" button on the Logs page could only
  // ever fail. Found by probing the API's failure paths, not by a bug report,
  // which is the point of probing them.
  cancelJob: (session: string, id: string) =>
    jpost<any>(`/jobs/${encodeURIComponent(id)}/cancel`, { session }).then((r) => unwrap<any>(r)),

  // Download a local model in the background (ollama pull / lms get --yes).
  // Returns {job_id, model, provider, status}; poll with jobStatus(session, job_id).
  pullModel: (session: string, provider: "ollama" | "lmstudio", model: string) =>
    jpost<any>(`/${provider}/pull`, { session, model }).then((r) =>
      unwrap<{ job_id: string; model: string; provider: string; status: string }>(r)),

  // Multipart upload of an .rds file -> becomes a server object with a handle.
  async uploadObject(session: string, file: File, name?: string) {
    const fd = new FormData();
    fd.append("session", session);
    if (name) fd.append("name", name);
    fd.append("file", file, file.name);
    const r = await fetch("/api/objects/upload", { method: "POST", body: fd });
    if (!r.ok) throw await apiError(r, "upload");
    return unwrap<any>(await r.json());
  },

  // Non-streaming chat (used as a fallback / for tests).
  chatSync: (session: string, message: string) =>
    jpost<any>("/chat/sync", { session, message }).then(unwrap),

  // ---- Async chat (live-feedback transport) --------------------------------
  // start -> returns a turn id; poll -> returns new events after a cursor.
  chatStart: (session: string, message: string) =>
    jpost<any>("/chat/start", { session, message }).then((r) =>
      unwrap<{ turn: string; cursor: number; status: string }>(r)),
  chatPoll: (session: string, turn: string, cursor: number) =>
    jget<any>(
      `/chat/poll?session=${encodeURIComponent(session)}&turn=${encodeURIComponent(turn)}&cursor=${cursor}`
    ).then((r) => unwrap<{ turn: string; status: string; done: boolean; cursor: number; events: any[]; error?: string }>(r)),
  chatCancel: (session: string, turn: string) =>
    jpost<any>("/chat/cancel", { session, turn }).then((r) => unwrap<any>(r)),
};
