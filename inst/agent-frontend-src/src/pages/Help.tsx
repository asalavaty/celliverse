// Help / documentation tab.
//
// This is a static content page (no data fetching). It walks a new user from
// zero to a working chat, then documents EVERY supported model provider:
// the fully-offline Ollama path plus each OpenAI-compatible / cloud provider,
// with the key sign-up link, where to paste the key, example (tool-capable)
// model ids, and provider-specific gotchas. Keep this in sync with the
// providers in Settings (PROVIDERS / PROVIDER_DEFAULT_MODEL) and the R config.
//
// The page is organised as an ACCORDION of categories so the (now long) content
// stays scannable: each category is a clickable header that expands/collapses
// its content inline. ALL content is preserved — only the presentation changed.

import { useState, type CSSProperties, type ReactNode } from "react";
import { LocalModelWarningText } from "../components/LocalModelWarning";

const A: CSSProperties = { color: "#0279ee", textDecoration: "none" };

// One collapsible category. `defaultOpen` expands it on first render.
function Section({ id, title, sub, defaultOpen = false, children }: {
  id: string;
  title: string;
  sub?: string;         // short one-line summary shown on the collapsed header
  defaultOpen?: boolean;
  children: ReactNode;
}) {
  const [open, setOpen] = useState(defaultOpen);
  return (
    <div className={`acc-item ${open ? "open" : ""}`}>
      <button
        type="button"
        className="acc-head"
        aria-expanded={open}
        aria-controls={`acc-${id}`}
        onClick={() => setOpen((o) => !o)}
      >
        <span className="acc-chev" aria-hidden>▸</span>
        <span className="acc-title">{title}</span>
        {sub && !open && <span className="acc-sub">{sub}</span>}
      </button>
      {open && (
        <div className="acc-body" id={`acc-${id}`}>
          {children}
        </div>
      )}
    </div>
  );
}

// One cloud provider's documentation block.
interface CloudProvider {
  id: string;
  name: string;
  keyUrl: string;      // where to create an API key
  keyLabel: string;    // human label for that page
  models: string;      // example, tool-capable model ids
  note: string;        // provider-specific caveat / tip
}

// NOTE: the model ids below are examples that support tool/function calling
// (the agent is useless without tool calling). Model availability changes over
// time — if a model 404s, open the provider's model list and pick a current one.
const CLOUD: CloudProvider[] = [
  {
    id: "openai",
    name: "OpenAI",
    keyUrl: "https://platform.openai.com/api-keys",
    keyLabel: "platform.openai.com/api-keys",
    models: "gpt-4o, gpt-4o-mini",
    note: "Billing must be enabled on the OpenAI account. gpt-4o-mini is the cheap, fast default; gpt-4o is stronger.",
  },
  {
    id: "anthropic",
    name: "Anthropic (Claude)",
    keyUrl: "https://console.anthropic.com/settings/keys",
    keyLabel: "console.anthropic.com → Settings → API Keys",
    models: "claude-3-5-sonnet-latest, claude-3-5-haiku-latest",
    note: "Use the *-latest aliases so you always get the current snapshot. All modern Claude models support tools.",
  },
  {
    id: "gemini",
    name: "Google Gemini",
    keyUrl: "https://aistudio.google.com/app/apikey",
    keyLabel: "aistudio.google.com/app/apikey",
    models: "gemini-2.5-flash, gemini-flash-latest, gemini-2.5-pro",
    note:
      "Has a free tier. IMPORTANT: the gemini-1.5-* models were retired by Google in 2025 and now return 404 — " +
      "use a current 2.x model. gemini-flash-latest always tracks the newest Flash.",
  },
  {
    id: "deepseek",
    name: "DeepSeek",
    keyUrl: "https://platform.deepseek.com/api_keys",
    keyLabel: "platform.deepseek.com → API keys",
    models: "deepseek-chat, deepseek-reasoner",
    note:
      "OpenAI-compatible endpoint (api.deepseek.com). deepseek-chat is the general tool-calling model; " +
      "deepseek-reasoner is the reasoning model. Very low cost.",
  },
  {
    id: "groq",
    name: "Groq",
    keyUrl: "https://console.groq.com/keys",
    keyLabel: "console.groq.com/keys",
    models: "llama-3.3-70b-versatile, llama-3.1-8b-instant",
    note:
      "Extremely fast inference on open models via an OpenAI-compatible endpoint. Pick a tool-capable model " +
      "(the -versatile Llama-3.3 works well); some smaller/guard models don't support tools.",
  },
  {
    id: "openrouter",
    name: "OpenRouter",
    keyUrl: "https://openrouter.ai/keys",
    keyLabel: "openrouter.ai/keys",
    models: "qwen/qwen3-30b-a3b-instruct-2507 (default), anthropic/claude-3-haiku, openai/gpt-4o-mini, anthropic/claude-3.5-sonnet, google/gemini-2.5-flash",
    note:
      "A single key that routes to many providers — this is the DEFAULT provider, on the qwen/qwen3-30b-a3b-instruct-2507 " +
      "model (strong tool-calling at a low price point). OpenRouter requires a key even for free models (there is no key-less endpoint); create one free at " +
      "openrouter.ai/keys. Model ids are \"vendor/model\" slugs — copy the exact id from openrouter.ai/models. " +
      "Free models are tagged \":free\" / carry a free badge. Make sure the chosen model advertises tool calling.",
  },
  {
    id: "cerebras",
    name: "Cerebras",
    keyUrl: "https://cloud.cerebras.ai/",
    keyLabel: "cloud.cerebras.ai → API Keys",
    models: "llama-3.3-70b, llama3.1-8b",
    note:
      "Very fast inference on Llama models via an OpenAI-compatible endpoint. Use a tool-capable Llama-3.3 model " +
      "for the agent.",
  },
];

export default function Help() {
  return (
    <div style={{ maxWidth: 780 }} className="help-acc">
      <p className="muted" style={{ marginTop: 0 }}>
        Everything you need to run the CelliVerse Agent. Click a category to expand it.
      </p>

      {/* ---- Getting started ---- */}
      <Section id="getting-started" title="Getting started" sub="Upload data, pick a model, start chatting" defaultOpen>
        <ol>
          <li>Go to <strong>Data / Upload</strong> and upload your dataset (or load one by server path). Supported: <span className="mono">.rds</span>, <span className="mono">.RData</span>/<span className="mono">.rda</span>, count tables as <span className="mono">.csv</span>/<span className="mono">.tsv</span>/<span className="mono">.txt</span> (also <span className="mono">.gz</span>), Matrix Market <span className="mono">.mtx</span>, and a <span className="mono">.zip</span> of the three 10x files (<span className="mono">matrix.mtx</span> + <span className="mono">barcodes.tsv</span> + <span className="mono">features.tsv</span>). 10x <span className="mono">.h5</span> works if the optional <span className="mono">hdf5r</span> package is installed. <span className="mono">.h5ad</span>, <span className="mono">.loom</span>, <span className="mono">.qs2</span> and Zarr need packages CelliVerse does not install — convert those to <span className="mono">.rds</span> or 10x MTX first, and the app will tell you so if you try.</li>
          <li>By default the agent runs on an <strong>OpenRouter model</strong> (<span className="mono">qwen/qwen3-30b-a3b-instruct-2507</span>, strong tool-calling at a low price point) — just paste a free key from <a style={A} href="https://openrouter.ai/keys" target="_blank" rel="noreferrer">openrouter.ai/keys</a> in the welcome card or <strong>Settings</strong>. For a zero-cost, fully offline start use <strong>Ollama</strong> instead (no API key).</li>
          <li>Open <strong>Chat</strong> and describe what you want, e.g. <em>“cluster this and annotate the cell types”</em> or <em>“show the top 10 markers of cluster C1”</em>.</li>
          <li>The agent calls CelliVerse tools (clustoCell, getDatasetMarkers, getClusterMarkers, markoClustVis, typoClust / annotateCellsLLM, …), streams progress, and shows plots &amp; tables inline.</li>
          <li>Switch model/provider any time — changes take effect on the next turn, no restart needed.</li>
        </ol>
      </Section>

      {/* ---- Local models & system stability (Round XLIII) ----
          Kept near the top, and open by default, because the failure it
          describes costs the user unsaved work in OTHER applications, not just
          in this one — which is not something they can discover by trying. */}
      <Section
        id="local-model-stability"
        title="Local models & system stability"
        sub="Local models are memory-heavy and can freeze or restart your machine"
        defaultOpen
      >
        <LocalModelWarningText />
      </Section>

      {/* ---- Glossary (Round LXXX, audit #64) ----
          Every term below appears on screen, in a tool card or a result, and
          none of them was defined anywhere in the product. "set id" in
          particular is a parameter the agent ASKS the user for. */}
      <Section id="glossary" title="Glossary" sub="The words this agent uses on screen">
        <dl className="glossary">
          <div><dt>Handle</dt><dd>
            The short id a loaded object is referred to by, e.g. <span className="mono">obj_a1b2c3</span> or
            <span className="mono"> clusto_d4e5f6</span>. Your data stays on the server; only the handle
            travels in the conversation, so the model never sees an expression matrix.
          </dd></div>
          <div><dt>Major cluster</dt><dd>
            A top-level cluster from <span className="mono">clustoCell</span>, named
            <span className="mono"> C1</span>, <span className="mono">C2</span>, … These are the
            coarse populations — the ones you would point at on a UMAP.
          </dd></div>
          <div><dt>Sub-cluster</dt><dd>
            A subdivision of one major cluster, named <span className="mono">C1-Sub1</span>,
            <span className="mono"> C1-Sub2</span>, … A sub-cluster is annotated <em>within</em> its
            parent's identity, so a sub-cluster of an NK cluster is a kind of NK cell rather than
            an unrelated type.
          </dd></div>
          <div><dt>Set id</dt><dd>
            Whatever you are asking about, named exactly as the object stores it: a major cluster
            (<span className="mono">C1</span>), a sub-cluster (<span className="mono">C1-Sub1</span>),
            or a named cell subset you defined. When a tool asks for
            <span className="mono"> desired_sets</span>, these are the strings it wants — the
            agent will not invent one, and neither should you.
          </dd></div>
          <div><dt>Cell subset</dt><dd>
            A group of cells you named yourself rather than one clustering produced — for example
            the cells of C2 saved under a name so you can re-use them. Carried as a
            <span className="mono"> cellset_…</span> handle.
          </dd></div>
          <div><dt>Positive / negative / medium markers</dt><dd>
            Three marker classes, not one. <strong>Positive</strong>: enriched in the set.
            <strong> Negative</strong>: depleted in it — as informative as positive ones for
            telling two similar lineages apart. <strong>Medium</strong>: intermediate expression.
            A gene absent from the positive markers may still be a medium marker, so
            "not a marker" and "not a <em>positive</em> marker" are different answers.
          </dd></div>
          <div><dt>Purity</dt><dd>
            How specific a marker is to one set: roughly, how much of that gene's expression sits
            inside the set rather than outside it. High purity means seeing the gene tells you
            you are probably in that set. Purity is reported per marker class, so a gene can have
            a purity as a medium marker and none as a positive one.
          </dd></div>
          <div><dt>Rank</dt><dd>
            A marker's position in its set's ranked list, 1 being the strongest. Ranks can TIE, so
            "markers ranked 3 or better" can legitimately return more than three rows — the agent
            keeps ties rather than truncating to a round number.
          </dd></div>
          <div><dt>markerDB vs LLM annotation</dt><dd>
            Two ways to name a cell type. <strong>markerDB</strong> scores your markers against the
            curated CelliVerse Marker DB — deterministic and repeatable.
            <strong> LLM</strong> (<span className="mono">annotateCellsLLM</span> / ceLLMarkup) asks a
            language model — broader coverage, not repeatable. The agent uses markerDB unless you
            ask for the other.
          </dd></div>
          <div><dt>Tissue / condition filter</dt><dd>
            Narrows the Marker DB before scoring. Filtering to the tissue you actually sampled
            usually sharpens an annotation; filtering to the wrong one quietly ruins it, which is
            why the agent asks rather than guessing.
          </dd></div>
        </dl>
      </Section>

      {/* ---- How it works ---- */}
      <Section id="how-it-works" title="How it works" sub="Handles, in-place updates, background workers, artifacts">
        <ul>
          <li>Objects live on the server and are passed between tools by <em>handle</em>; the raw data never enters the prompt.</li>
          <li>Tools that <em>add results back</em> to an object (addClustoData, addTypoData) update it <strong>in place</strong> — the same handle, no duplicate copy of your (large) dataset.</li>
          <li>When a request could apply to more than one loaded object, the agent <strong>asks which one</strong> instead of guessing.</li>
          <li>Heavy steps run on a background worker — watch progress under <strong>Logs</strong>.</li>
          <li>Plots are saved as editable <span className="mono">SVG</span> (plus PNG) and tables as <span className="mono">CSV</span>; download links sit under each result.</li>
          <li><strong>Package Browser</strong> lists every tool the agent can use and its parameters.</li>
        </ul>
      </Section>

      {/* ---- Models & providers ---- */}
      <Section id="providers" title="Choosing a provider & model" sub="Tool-calling models, live lists, keys">
        <h4 style={{ margin: "2px 0 6px" }}>Choosing a model provider</h4>
        <p className="muted">
          The agent needs a <strong>tool-calling-capable</strong> model — that's how it drives the CelliVerse
          functions. Set the provider, model, and any API key in <strong>Settings</strong>. Keys are stored
          server-side in <span className="mono">~/.celliverse/config.json</span> and are never sent back to the
          browser. Every provider below can also be configured with an environment variable instead of the UI.
        </p>

        <h4>Choosing a model</h4>
        <p>
          The <strong>Model</strong> field in Settings is a searchable dropdown. When you open Settings or
          switch provider, CelliVerse fetches that provider's model list:
        </p>
        <ul>
          <li>
            <strong>With a key set</strong> (or for OpenRouter, whose catalog is public) it shows the
            provider's <strong>live</strong> model list, each with a context-length badge and a <span className="mono">free</span> badge where applicable. Click <strong>Refresh</strong> to re-fetch the latest catalog.
          </li>
          <li>
            <strong>Without a key</strong> (or if the provider can't be reached) it shows a <strong>curated
            shortlist</strong> of well-known models so you can get started; add your key and click Refresh to
            load the full live list.
          </li>
          <li>
            <strong>You can always type any model id</strong> — the field accepts a manually entered slug even
            if it isn't in the list. This matters because provider catalogs change faster than any built-in
            list: if a brand-new model isn't shown yet, just paste its id and press Enter.
          </li>
          <li>
            For <strong>Ollama</strong>, the dropdown lists ALL the models you've actually pulled and marks the
            recommended light/recommended/strong tiers; Refresh re-checks the local daemon. For{" "}
            <strong>LM Studio</strong>, it lists ALL models downloaded in LM Studio (from its{" "}
            <span className="mono">/v1/models</span> endpoint).
          </li>
        </ul>
        <p className="muted" style={{ fontSize: 12 }}>
          <strong>OpenRouter model ids</strong> are <span className="mono">vendor/model</span> slugs — e.g.
          <span className="mono"> openai/gpt-4o-mini</span>, <span className="mono">anthropic/claude-sonnet-4</span>,
          <span className="mono"> qwen/qwen3.7-flash</span>. Copy the exact id from the list or from
          openrouter.ai/models. Direct providers use their native ids (e.g. <span className="mono">gpt-4o-mini</span>,
          <span className="mono"> claude-3-5-sonnet-latest</span>, <span className="mono">gemini-2.5-flash</span>).
        </p>
        <p className="muted" style={{ fontSize: 12 }}>
          <strong>Key precedence:</strong> if the matching environment variable (e.g.
          <span className="mono"> OPENROUTER_API_KEY</span>) is set, it overrides a key saved in the UI. Either
          one enables the live model list for that provider.
        </p>

        </Section>

<Section id="providers-ollama" title="Ollama (local, offline)" sub="No API key — run open models on your machine"><h4>Ollama — local &amp; fully offline (no API key)</h4>
        <p>Run open models on your own machine; nothing leaves your computer.</p>
        <ol>
          <li>Install Ollama from <a style={A} href="https://ollama.com/download" target="_blank" rel="noreferrer">ollama.com/download</a>.</li>
          <li>Start the server: <span className="mono">ollama serve</span> (it usually auto-starts after install).</li>
          <li>Pull a tool-capable model, e.g. <span className="mono">ollama pull qwen3:8b</span> — or let the installer pick the best tier for your machine: <span className="mono">install_celliverse_agent()</span> (it detects your RAM and pulls the strongest tier that fits).</li>
          <li>In <strong>Settings</strong> choose provider <span className="mono">ollama</span>, set the model to the tag you pulled, and (if not local) set the <strong>Ollama host</strong> — default <span className="mono">http://localhost:11434</span>, or the <span className="mono">OLLAMA_HOST</span> env var. You can also download models without leaving the app: <strong>Settings → Download model</strong> runs <span className="mono">ollama pull</span> in the background.</li>
        </ol>
        <p className="muted" style={{ fontSize: 12 }}>
          Not every local model supports tool calling — the Qwen3 instruct models and the
          Llama-3.1 instruct models do. If the agent says the model can't use tools, pull one of those. If it says
          it can't reach Ollama, confirm <span className="mono">ollama serve</span> is running and the host is
          correct.
        </p>
        <div style={{ marginTop: 10 }}>
          <strong>Three recommended tiers (pick the best your RAM allows):</strong>
          <ul>
            <li>
              <strong>Light</strong> — <span className="mono">qwen3:8b</span> (~5&nbsp;GB, needs ~8&nbsp;GB RAM).
              Fast on most machines. Tool-calling works but is less reliable on this size of model.
              On ≤16&nbsp;GB machines the legacy <span className="mono">qwen2.5:7b-instruct</span> (~4.7&nbsp;GB) is a lighter fallback.
            </li>
            <li>
              <strong>Recommended</strong> — <span className="mono">qwen3:30b-instruct</span> (~19&nbsp;GB, needs ~24&nbsp;GB RAM).
              A mixture-of-experts model with only ~3.3B active parameters — near-light speed with much stronger,
              tool-trained instruction following. The sweet spot for most workstations.
            </li>
            <li>
              <strong>Strong</strong> — <span className="mono">qwen3.6:35b</span> (~24&nbsp;GB, needs ~36&nbsp;GB RAM).
              The most reliable local tool-calling tier (35B-A3B MoE, 256k context). Pull it on demand:
              <span className="mono"> install_celliverse_agent(tier = "strong")</span> (or
              <span className="mono"> ollama pull qwen3.6:35b</span>), then select it in <strong>Settings</strong>.
            </li>
          </ul>
          <span className="mono">install_celliverse_agent()</span> with no arguments auto-detects your RAM and pulls
          the best tier that fits; the launcher prints a one-line tip when your machine can run a stronger tier than
          the one selected. In <strong>Settings</strong> you can pick a tier with one click, download any model
          in-app, and if the chosen model isn't pulled yet you'll see a non-blocking reminder.
        </div>

        <h4 style={{ marginTop: 12 }}>Speed tuning (macOS / Linux)</h4>
        <p>
          Ollama runs as a background command-line service, so configuration is handled via variables added to your
          shell profile (<span className="mono">~/.zshrc</span>; edit with <span className="mono">nano ~/.zshrc</span>,
          then save) followed by a server restart: <span className="mono">killall ollama &amp;&amp; ollama serve</span>.
        </p>
        <ul>
          <li>
            <strong>Flash Attention</strong> — add <span className="mono">export OLLAMA_FLASH_ATTENTION=1</span>.
            Speeds up prompt evaluation times.
          </li>
          <li>
            <strong>Memory purging</strong> — add <span className="mono">export OLLAMA_KEEP_ALIVE=0</span>.
            Instantly ejects idle models from your RAM, preventing memory-stacking crashes when switching models.
          </li>
          <li>
            <strong>GPU buffer safety</strong> — add <span className="mono">export OLLAMA_GPU_OVERHEAD=536870912</span>.
            Reserves a 512&nbsp;MB memory buffer to stop macOS from crashing Ollama on tight fits.
          </li>
        </ul>

        </Section>

<Section id="providers-lmstudio" title="LM Studio (local, GUI model manager)" sub="Apple Silicon / MLX-friendly local server">
        <h4 style={{ margin: "2px 0 6px" }}>What is LM Studio?</h4>
        <p>
          <a style={A} href="https://lmstudio.ai" target="_blank" rel="noreferrer">LM Studio</a> is a free desktop
          app (macOS, Windows, Linux) that finds, downloads, and runs open-weight language models on your own
          machine, and can serve them behind an <strong>OpenAI-compatible local API</strong>. Like Ollama, nothing
          leaves your computer and no API key is needed. The difference is the interface: LM Studio gives you a
          polished <strong>graphical model manager</strong> (search, download, and configure models with clicks
          instead of terminal commands), and on <strong>Apple Silicon</strong> its <strong>MLX builds</strong> are
          often the fastest way to run Qwen/Llama models — frequently faster than the equivalent Ollama build.
        </p>
        <p>
          <strong>When to use LM Studio instead of Ollama:</strong> prefer LM Studio if you are on a Mac with Apple
          Silicon and want the MLX speed-up, if you prefer a GUI over the terminal for managing models, or if a
          model you want is only published in MLX/GGUF form on the LM Studio hub. Prefer Ollama if you want a
          minimal headless daemon, scripted installs, or the one-command <span className="mono">install_celliverse_agent()</span>
          flow (which drives Ollama). You can have both installed; the agent talks to whichever provider you select.
        </p>

        <h4 style={{ marginTop: 10 }}>Step by step</h4>
        <ol>
          <li>
            <strong>Download &amp; install</strong> LM Studio from{" "}
            <a style={A} href="https://lmstudio.ai" target="_blank" rel="noreferrer">lmstudio.ai</a>{" "}
            (pick your OS), then open the app once so it finishes first-run setup.
          </li>
          <li>
            <strong>Download a model.</strong> Two ways:
            <ul>
              <li>
                <em>In the app:</em> use the Discover/Search tab, find a tool-capable model (e.g.{" "}
                <span className="mono">qwen/qwen3-8b</span>, <span className="mono">ibm/granite-4-micro</span>) and
                click Download. On Apple Silicon prefer the <strong>MLX</strong> build when offered.
              </li>
              <li>
                <em>From this agent:</em> <strong>Settings → Download model</strong> runs{" "}
                <span className="mono">lms get &lt;model&gt; --yes</span> in the background (requires the{" "}
                <span className="mono">lms</span> CLI, installed by LM Studio).
              </li>
            </ul>
          </li>
          <li>
            <strong>Start the server — this is the step most people miss.</strong> Downloading a model does
            <em> not</em> start the API server, and the agent cannot see your models until it is running. In the
            LM Studio app open the <strong>Developer page</strong> (called the <strong>"Local Model API"</strong>{" "}
            tab in recent versions) and toggle the server to <strong>Running</strong> / click{" "}
            <strong>Start Server</strong> (default port 1234) — it is <strong>off/stopped by default</strong>. Or
            run <span className="mono">lms server start</span> in a terminal. The server must stay running while
            you use the agent.
          </li>
          <li>
            <strong>Configure the agent.</strong> In <strong>Settings</strong> choose provider{" "}
            <span className="mono">lmstudio</span>. The <strong>LM Studio host</strong> defaults to{" "}
            <span className="mono">http://localhost:1234/v1</span> — leave it unless you changed the port. Open the
            model dropdown (click <strong>Refresh</strong> if needed): it lists every model downloaded in LM
            Studio. Pick one and <strong>Save settings</strong>. No API key is needed.
          </li>
        </ol>

        <h4 style={{ marginTop: 10 }}>Speed tuning</h4>
        <p>
          In the left panel, open the <strong>Library</strong> tab, then click the <strong>gear icon</strong> beside
          your desired model to open its settings:
        </p>
        <ul>
          <li>
            <strong>Context and Performance → Max Concurrent Predictions: set to 1.</strong> This stops the app
            from dividing your GPU power to prepare for multiple simultaneous prompts.
          </li>
          <li>
            <strong>Generation → Context Overflow: set to Rolling Window.</strong> It discards old history cleanly,
            avoiding the heavy reprocessing math of "Truncate Middle".
          </li>
          <li>
            <strong>Model Formats: prefer MLX native models over GGUFs.</strong> Whenever possible, look for and
            download MLX native models rather than traditional GGUFs — they leverage Apple's memory architecture
            far better.
          </li>
        </ul>

        <h4 style={{ marginTop: 10 }}>Troubleshooting</h4>
        <ul>
          <li>
            <strong>"LM Studio server not reachable"</strong> — the server is not running (see step 3: the
            Developer / "Local Model API" tab shows it as Stopped until you start it), or it is on a different
            port. Start it in the app, then click <strong>Re-check</strong> in Settings.
          </li>
          <li>
            <strong>Your model doesn't appear in the dropdown</strong> — it was downloaded in the app but the
            server wasn't started, or the host/port is wrong. Start the server and click Refresh.
          </li>
          <li>
            <strong>Tool-calling is unreliable / the model forgets instructions</strong> — LM Studio serves each
            model with the context length from its metadata (sometimes only 4k/8k). Raise it in the app's model
            load settings (e.g. 8192+) for reliable tool-calling, just like Ollama's num_ctx.
          </li>
        </ul>
        <p className="muted" style={{ fontSize: 12 }}>
          The <strong>LM Studio host</strong> field accepts any OpenAI-compatible local endpoint, so the same
          provider slot covers <span className="mono">llama.cpp</span> (<span className="mono">http://localhost:8080/v1</span>)
          and <span className="mono">Jan</span> (<span className="mono">http://localhost:1337/v1</span>). On a
          fully headless server (no GUI), LM Studio's <span className="mono">llmster</span> daemon provides the same
          server without the desktop app — see lmstudio.ai/docs.
        </p>
      </Section>

<Section id="providers-ollama-trouble" title="Ollama troubleshooting" sub="command not found & can’t connect"><h4 style={{ marginTop: 14 }}>Ollama troubleshooting — “command not found” &amp; can’t connect</h4>
        <p>
          Two checks fix almost every local-Ollama problem: <strong>(1)</strong> can your shell find the
          <span className="mono"> ollama</span> command, and <strong>(2)</strong> is the Ollama server actually running?
        </p>
        <p style={{ marginBottom: 4 }}><strong>1. “zsh: command not found: ollama” → your PATH is missing the install location.</strong></p>
        <ul>
          <li>Find where Ollama is installed:
            <ul>
              <li>Apple Silicon (Homebrew): <span className="mono">ls -l /opt/homebrew/bin/ollama</span></li>
              <li>Intel Mac (Homebrew): <span className="mono">ls -l /usr/local/bin/ollama</span></li>
              <li>macOS app: <span className="mono">/Applications/Ollama.app</span></li>
            </ul>
          </li>
          <li>
            If it's under <span className="mono">/opt/homebrew/bin</span> (the usual Apple-Silicon case) but
            <span className="mono"> which ollama</span> finds nothing, add Homebrew to your PATH. Edit
            <span className="mono"> ~/.zshrc</span> and add this line near the top:
            <div className="mono" style={{ background: "#f6f5f1", border: "1px solid var(--border)", borderRadius: 6, padding: "6px 10px", margin: "6px 0" }}>
              eval "$(/opt/homebrew/bin/brew shellenv)"
            </div>
            Then reload: <span className="mono">source ~/.zshrc</span>
          </li>
          <li>
            Verify: <span className="mono">which ollama</span> should print the path, and
            <span className="mono"> ollama --version</span> / <span className="mono">ollama list</span> should run.
          </li>
        </ul>
        <p style={{ marginBottom: 4 }}><strong>2. “cannot reach Ollama” / nothing on <span className="mono">http://localhost:11434</span> → the server isn't running.</strong></p>
        <ul>
          <li>Start it: <span className="mono">ollama serve</span> (or just open the Ollama macOS app — it runs the server in the background).</li>
          <li>Confirm it's up: <span className="mono">ollama list</span> (or open <span className="mono">http://localhost:11434</span> in a browser — it should respond).</li>
          <li>In <strong>Settings</strong>, set the <strong>Ollama host</strong> to <span className="mono">http://localhost:11434</span> (the default) and pick a model you've pulled.</li>
        </ul>
        <p className="muted" style={{ fontSize: 12 }}>
          <strong>Note:</strong> <span className="mono">systemctl</span> (e.g.
          <span className="mono"> sudo systemctl start ollama</span>) is <strong>Linux-only</strong> — it does not
          exist on macOS. On a Mac, start Ollama with <span className="mono">ollama serve</span> or the app. Once
          the command is on your PATH and the server is running, the agent's Ollama provider connects automatically.
        </p>

        </Section>

<Section id="providers-cloud" title="Cloud providers (API key)" sub="OpenAI, Anthropic, Gemini, DeepSeek, Groq, OpenRouter, Cerebras"><h4 style={{ marginTop: 14 }}>Cloud providers (API key)</h4>
        <p className="muted" style={{ fontSize: 12 }}>
          All of these are OpenAI-compatible from CelliVerse's point of view: create a key, paste it into the
          matching field in <strong>Settings → API keys</strong> (or set the env var), pick the provider, and
          enter a model id.
        </p>
        {CLOUD.map((p) => (
          <div className="provider-block" key={p.id}>
            <strong>{p.name}</strong>
            <ul>
              <li><strong>Get a key:</strong> <a style={A} href={p.keyUrl} target="_blank" rel="noreferrer">{p.keyLabel}</a></li>
              <li><strong>Where it goes:</strong> Settings → API keys → <span className="mono">{p.id}</span> (env var <span className="mono">{envVar(p.id)}</span>)</li>
              <li><strong>Provider in Settings:</strong> <span className="mono">{p.id}</span></li>
              <li><strong>Example models:</strong> <span className="mono">{p.models}</span></li>
              <li className="muted">{p.note}</li>
            </ul>
          </div>
        ))}
      </Section>

      {/* ---- Troubleshooting ---- */}
      <Section id="troubleshooting" title="Troubleshooting" sub="Common errors and what they mean">
        <ul>
          <li><strong>“model … not found / 404”</strong> — the model id isn't valid for the selected provider. Check the provider and use a current model id (for Gemini, avoid the retired gemini-1.5-*).</li>
          <li><strong>“API key missing/invalid”</strong> — re-enter the key for that provider in Settings; confirm billing is enabled where required.</li>
          <li><strong>“model doesn't support tools”</strong> — switch to a tool-capable model (see each provider's examples).</li>
          <li><strong>“cannot reach Ollama”</strong> — start <span className="mono">ollama serve</span>, or point the host at the right address, or switch to a cloud provider.</li>
        </ul>
      </Section>
    </div>
  );
}

// Map a provider id to its environment-variable name (documentation only).
function envVar(id: string): string {
  switch (id) {
    case "openai": return "OPENAI_API_KEY";
    case "anthropic": return "ANTHROPIC_API_KEY";
    case "gemini": return "GEMINI_API_KEY";
    case "deepseek": return "DEEPSEEK_API_KEY";
    case "groq": return "GROQ_API_KEY";
    case "openrouter": return "OPENROUTER_API_KEY";
    case "cerebras": return "CEREBRAS_API_KEY";
    default: return "";
  }
}
