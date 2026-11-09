// Local-model stability warning (Round XLIII).
//
// WHY THIS EXISTS. A local model does not run inside this app. Ollama and LM
// Studio are separate processes that hold the model weights and the KV cache in
// memory — on Apple Silicon, in memory shared with the GPU — for as long as the
// model stays loaded. CelliVerse cannot bound, measure or reclaim that memory,
// and nothing in this package can: the agent's own memory guards only govern
// the R process.
//
// On a machine that is already close to its limit, the combination of a loaded
// local model, a raw single-cell dataset in R, and the rest of the user's
// desktop can push the system into a state where macOS's WindowServer watchdog
// fires and the machine restarts. That has now been observed repeatedly on one
// user's machine — across different agent versions, different tools, and after
// reverting to a pre-fix build — while the SAME work on a cloud model completed
// without incident every time. It is a property of the machine's memory budget,
// not of any one code path, so the honest response is to warn rather than to
// pretend a guard exists.
//
// The warning is shown ONLY when a local provider is selected (a cloud user does
// not need it) and is dismissible, because a user who has read it once and knows
// their machine copes should not keep seeing it.

import { useState } from "react";

const DISMISS_KEY = "cv_local_model_warning_dismissed";

export const LOCAL_PROVIDERS = ["ollama", "lmstudio"];

export function isLocalProvider(p?: string | null): boolean {
  return typeof p === "string" && LOCAL_PROVIDERS.includes(p.toLowerCase());
}

/** Banner for the top of the Chat page. Renders nothing for a cloud provider. */
export default function LocalModelWarning({ provider }: { provider?: string | null }) {
  const [dismissed, setDismissed] = useState<boolean>(() => {
    try { return localStorage.getItem(DISMISS_KEY) === "1"; } catch { return false; }
  });

  if (!isLocalProvider(provider) || dismissed) return null;

  function dismiss() {
    setDismissed(true);
    try { localStorage.setItem(DISMISS_KEY, "1"); } catch { /* ignore */ }
  }

  return (
    <div className="local-warn" role="status">
      <span>
        <strong>Heads-up: you are running a local model.</strong> Local models hold several GB of
        memory outside this app, and on a machine that is already near its limit they can make the
        whole system freeze or restart — losing anything unsaved, in any application. Save your work
        as you go, and prefer a <strong>cloud model</strong> (Settings) for long or heavy runs.
      </span>
      <button type="button" className="onboard-close" onClick={dismiss} aria-label="Dismiss">×</button>
    </div>
  );
}

/** The same guidance as prose, for the Help tab. */
export function LocalModelWarningText() {
  return (
    <>
      <p style={{ marginTop: 0 }}>
        Local models (<strong>Ollama</strong>, <strong>LM Studio</strong>) run in their own process,
        not inside CelliVerse. While a model is loaded it holds its weights and context cache in
        memory — on Apple Silicon, in memory shared with the GPU — and CelliVerse can neither
        measure nor reclaim it.
      </p>
      <p>
        Add a single-cell dataset loaded in R and a normal desktop's worth of other applications,
        and a machine that is already close to its memory limit can be pushed over it. When that
        happens the symptom is not an error message: the system freezes, or macOS restarts the
        machine outright, and <strong>anything unsaved in any application is lost</strong>. This has
        been observed with different models, different tools and different versions of the agent —
        it reflects the machine's total memory budget rather than a specific bug.
      </p>
      <p style={{ marginBottom: 0 }}>
        <strong>What to do.</strong> Save your work before long or heavy runs, and keep exporting
        results as you go (<strong>Results</strong> tab). If it happens on your machine, prefer a{" "}
        <strong>cloud model</strong> for the heavy steps — clustering a raw dataset, LLM-based cell
        type annotation — and keep the local model for lighter questions. Reducing the pressure also
        helps: load a smaller local model, lower its context length, close other large applications,
        and avoid running several heavy tools at once.
      </p>
    </>
  );
}
