import { useState } from "react";
import { api } from "../api/client";
import ErrorDetail from "../components/ErrorDetail";

export default function FileUpload({ session, onUploaded }: { session: string; onUploaded: () => void }) {
  const [file, setFile] = useState<File | null>(null);
  const [name, setName] = useState("");
  const [path, setPath] = useState("");
  const [busy, setBusy] = useState(false);
  const [msg, setMsg] = useState<string>("");
  // Round LXXIX: whether `msg` is a failure. It used to be inferred with
  // msg.includes("failed"), which meant a new error phrasing rendered in the
  // muted "everything is fine" colour -- the class of bug where the styling
  // silently disagrees with the sentence.
  const [msgErr, setMsgErr] = useState(false);
  const [handle, setHandle] = useState<string>("");
  // Round LXXXII: the server's technical cause. It has been in the error
  // envelope since Round LXII and this page dropped it, so a 500 on a 3.8 GB
  // upload showed one calm sentence and nothing anyone could act on. Same
  // toggle the transcript uses (components/ErrorDetail.tsx).
  const [detail, setDetail] = useState<string>("");
  // Round LXXXII: what a browser upload of THIS file would cost, asked the
  // moment it is chosen. `advice` is null until the server answers and stays
  // null when it has no opinion. It never disables Upload -- see
  // cv_upload_advice() in R/agent_bigdata.R for why this informs and does not
  // refuse.
  const [advice, setAdvice] = useState<{ advisable: boolean; message?: string | null } | null>(null);
  // Round LXXXIII: the matrix handle whose Seurat was SKIPPED on load, so the
  // offer can be made here instead of only in prose. Round LXXXII decided for
  // the user and left them no button -- and the chat sentence it suggested
  // resolved to a tool that did not exist.
  const [pendingSeurat, setPendingSeurat] = useState<string>("");

  // File chosen: ask the server whether sending it this way is wise, using the
  // size the browser already knows. No bytes move.
  async function chooseFile(f: File | null) {
    setFile(f); setAdvice(null); setMsg(""); setDetail(""); setMsgErr(false);
    if (!f) return;
    try {
      const a = await api.uploadAdvice(f.size);
      if (a && a.advisable === false) setAdvice(a);
    } catch { /* no advice is not an error: the upload path is unchanged */ }
  }

  // Round LXXIX (audit #56): refuse a format this build cannot read BEFORE the
  // bytes leave the machine.
  //
  // The server refuses it too (cv_upload_extension_problem), and THAT is the
  // guard that counts -- this one exists because the client is the only place
  // that can save the transfer itself. A 650 MB .h5ad otherwise uploads in
  // full, is written to a temp file, and is then declined for a reason its NAME
  // gave away.
  //
  // Kept in step with CV_UPLOAD_EXTS in R/agent_ingest.R and with the accept=
  // list below. There is deliberately NO size check: the project's standing
  // policy is no upload limits, and a 40 GB atlas is a normal file here.
  function extProblem(f: File): string | null {
    const name = f.name.toLowerCase().replace(/\.gz$/, "");
    const dot = name.lastIndexOf(".");
    if (dot < 0) return null;                    // no extension -> let the server decide
    const ext = name.slice(dot + 1);
    const ok = ["rds", "rdata", "rda", "csv", "tsv", "txt", "tab", "mtx", "zip", "h5"];
    if (ok.includes(ext)) return null;
    if (["h5ad", "loom", "qs", "qs2", "zarr"].includes(ext)) {
      return `This build cannot read a .${ext} file — it needs an R package CelliVerse does not install. ` +
        `Convert it to a Seurat .rds or 10x MTX and upload that. Nothing was sent.`;
    }
    return `This build cannot read a .${ext} file. It reads .rds, .RData/.rda, .csv/.tsv/.txt (optionally .gz), ` +
      `.mtx, a .zip of 10x files, and .h5. Nothing was sent.`;
  }

  async function upload() {
    if (!file || !session) return;
    const problem = extProblem(file);
    if (problem) { setMsg(problem); setMsgErr(true); setHandle(""); return; }
    setBusy(true); setMsg(""); setDetail(""); setMsgErr(false); setHandle("");
    try {
      const r = await api.uploadObject(session, file, name || undefined);
      setHandle(r?.handle ?? "");
      setMsg(
        `Uploaded. Server handle: ${r?.handle ?? "?"} (${r?.descriptor?.summary ?? ""})` +
          (r?.note ? `\n${r.note}` : "")
      );
      setPendingSeurat(r?.seurat_skipped ? (r?.handle ?? "") : "");
      setMsgErr(false);
      onUploaded();
    } catch (e: any) {
      setMsg(`Upload failed: ${e.message}`); setDetail(e?.detail || ""); setMsgErr(true);
      // Round LXXXIV: this failure has ONE remedy, so take the user to it.
      // The browser never reveals a file's folder, so the name is prefilled and
      // the caret is put in the box for them to prepend the directory --
      // better than a sentence telling them to retype what they just picked.
      if (e?.usePathBox && file) {
        setPath((p) => (p && p !== file.name ? p : file.name));
        const el = document.getElementById("up-path") as HTMLInputElement | null;
        if (el) { el.focus(); el.setSelectionRange(0, 0); }
      }
    } finally { setBusy(false); }
  }

  async function loadFromPath() {
    if (!path || !session) return;
    setBusy(true); setMsg(""); setDetail(""); setMsgErr(false); setHandle("");
    try {
      const r = await api.loadObject(session, path, name || undefined);
      setHandle(r?.handle ?? "");
      setMsg(
        `Loaded. Server handle: ${r?.handle ?? "?"} (${r?.descriptor?.summary ?? ""})` +
          (r?.note ? `\n${r.note}` : "")
      );
      setPendingSeurat(r?.seurat_skipped ? (r?.handle ?? "") : "");
      setMsgErr(false);
      onUploaded();
    } catch (e: any) {
      setMsg(`Load failed: ${e.message}`); setDetail(e?.detail || ""); setMsgErr(true);
    } finally { setBusy(false); }
  }

  // Round LXXXIII: do it anyway. The estimate is an estimate; the person whose
  // machine it is gets the final say, which is the same rule the upload warning
  // above already follows.
  async function buildSeurat() {
    if (!pendingSeurat || !session || busy) return;
    setBusy(true); setMsg(""); setDetail(""); setMsgErr(false);
    try {
      const r = await api.toSeurat(session, pendingSeurat, name || undefined);
      setPendingSeurat("");
      setHandle(r?.handle ?? "");
      setMsg(r?.note || `Built ${r?.handle}.`);
      setMsgErr(false);
      onUploaded();
    } catch (e: any) {
      setMsg(`Could not build the Seurat object: ${e.message}`);
      setDetail(e?.detail || ""); setMsgErr(true);
    } finally { setBusy(false); }
  }

  return (
    <div style={{ maxWidth: 560 }}>
      <div className="card">
        <h3>Upload a dataset</h3>
        <p className="muted" style={{ fontSize: 12 }}>
          A saved Seurat / SingleCellExperiment / SpatialExperiment or matrix
          (<span className="mono">.rds</span>, <span className="mono">.RData</span>), a count table
          (<span className="mono">.csv</span> / <span className="mono">.tsv</span> /
          <span className="mono">.txt</span>, optionally <span className="mono">.gz</span>),
          Matrix Market (<span className="mono">.mtx</span>), or a
          <span className="mono">.zip</span> of the three 10x files
          (<span className="mono">matrix.mtx</span>, <span className="mono">barcodes.tsv</span>,
          <span className="mono">features.tsv</span>). A 10x
          <span className="mono">.h5</span> also works if you have the
          <span className="mono">hdf5r</span> package installed.
          It is stored server-side and referenced by a handle — the raw object never enters the chat.
        </p>
        <div className="form-row">
          {/* Round LXXIX (audit #48): htmlFor/id. */}
          <label htmlFor="up-file">File</label>
          {/* Round XLIX: the picker used to filter to .rds ONLY, so a user with a
              CSV or a 10x MTX could not even select their file — the format
              support behind it was invisible. Keep the list in step with
              cv_supported_formats() in R/agent_ingest.R. */}
          <input
            id="up-file"
            type="file"
            accept=".rds,.Rds,.RDS,.rdata,.RData,.rda,.csv,.tsv,.txt,.tab,.gz,.mtx,.zip,.h5"
            onChange={(e) => chooseFile(e.target.files?.[0] ?? null)}
          />
        </div>
        {/* Round LXXXII: the size warning. Deliberately BETWEEN the picker and
            the Upload button, and deliberately not a blocker -- the button
            below stays enabled, because the project's standing policy is no
            upload limits and the ceiling here belongs to one transport on one
            machine, not to the product. */}
        {advice?.message && (
          <div className="upload-warn" role="status">
            {advice.message}
            <button type="button" className="rail-link" style={{ marginLeft: 6 }}
                    onClick={() => document.getElementById("up-path")?.focus()}>
              Use the path box
            </button>
          </div>
        )}
        <div className="form-row">
          <label htmlFor="up-name">Optional name</label>
          <input id="up-name" value={name} onChange={(e) => setName(e.target.value)} placeholder="e.g. pbmc3k" />
        </div>
        <button className="btn" onClick={upload} disabled={!file || busy || !session}>
          {busy ? "Uploading…" : "Upload"}
        </button>
      </div>

      <div className="card">
        <h3>…or load a file already on the server</h3>
        <p className="muted" style={{ fontSize: 12 }}>
          Handy for large files: give a path readable by the R process (avoids re-uploading GBs).
        </p>
        <div className="form-row">
          <label htmlFor="up-path">Server file path</label>
          <input id="up-path" value={path} onChange={(e) => setPath(e.target.value)} placeholder="/data/pbmc3k.rds" />
        </div>
        <button className="btn secondary" onClick={loadFromPath} disabled={!path || busy || !session}>
          {busy ? "Loading…" : "Load from path"}
        </button>
      </div>

      {pendingSeurat && (
        <div className="upload-warn" role="status" style={{ marginTop: 12 }}>
          Clustering with clustoCell does not need a Seurat object — it reads the matrix
          directly. Everything else (markers, purity, UMAP) does.
          <button type="button" className="btn" style={{ marginLeft: 10, padding: "4px 12px", fontSize: 12 }}
                  disabled={busy} onClick={buildSeurat}>
            {busy ? "Building…" : "Build the Seurat anyway"}
          </button>
        </div>
      )}
      {handle && <div className="chip">{handle}</div>}
      {msg && <p style={{ marginTop: 10 }} className={msgErr ? "err-text" : "muted"}>{msg}</p>}
      {msgErr && <ErrorDetail detail={detail} />}
    </div>
  );
}
