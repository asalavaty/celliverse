import { useEffect, useState, useCallback } from "react";
import { api, ObjectDescriptor, JobInfo } from "./api/client";
import Chat, { ChatItem, historyToItems } from "./pages/Chat";
import SettingsPage from "./pages/Settings";
import Results from "./pages/Results";
import FileUpload from "./pages/FileUpload";
import History from "./pages/History";
import PackageBrowser from "./pages/PackageBrowser";
import Logs from "./pages/Logs";
import Help from "./pages/Help";
import About from "./pages/About";

export type Page = "chat" | "results" | "upload" | "settings" | "history" | "package" | "logs" | "help" | "about";

const NAV: { id: Page; label: string }[] = [
  { id: "chat", label: "Chat" },
  { id: "results", label: "Results" },
  { id: "upload", label: "Data / Upload" },
  { id: "settings", label: "Settings" },
  { id: "history", label: "History" },
  { id: "package", label: "Package Browser" },
  { id: "logs", label: "Logs" },
  { id: "help", label: "Help" },
  { id: "about", label: "About" },
];

// Round LXIV (D3): the last session id, so a reload resumes instead of
// discarding the conversation. Namespaced like the other two localStorage keys
// in this app (Onboarding, LocalModelWarning).
const SESSION_KEY = "cv_last_session_id";

export default function App() {
  const [page, setPage] = useState<Page>("chat");
  const [session, setSession] = useState<string>("");
  const [healthy, setHealthy] = useState<boolean | null>(null);
  const [version, setVersion] = useState<string>("");
  const [objects, setObjects] = useState<ObjectDescriptor[]>([]);
  const [jobs, setJobs] = useState<JobInfo[]>([]);
  // Chat conversation state lives HERE (in App), not inside <Chat/>, so it
  // survives switching tabs (Chat unmounts on tab change). This fixes the
  // "chat history disappears when I switch tabs" bug.
  const [chatItems, setChatItems] = useState<ChatItem[]>([]);
  const [chatBusy, setChatBusy] = useState(false);

  // Bootstrap: health + a fresh session.
  useEffect(() => {
    (async () => {
      try {
        const h = await api.health();
        setHealthy(true);
        setVersion(h?.version ?? "");
      } catch {
        setHealthy(false);
      }
      // Round LXIV (D3): resume the last session across a reload.
      //
      // This used to call newSession() unconditionally and then getSession() on
      // the id it had just been handed, under a comment claiming it "covers a
      // full page reload". It could not: the id was one second old, so the
      // fetch was always empty and an accidental refresh threw the whole
      // conversation away. The backend had atomic snapshot + restore the entire
      // time -- the work survived on disk and the product offered no way back
      // to it.
      //
      // Now: remember the id, try to restore it first, and fall back to a new
      // session if it is gone. The 404 path is real since Round LXII gave
      // /api/session/<id> a proper HTTP status.
      try {
        let sid: string | null = null;
        let hist: unknown[] = [];
        const saved = (() => {
          try { return localStorage.getItem(SESSION_KEY); } catch { return null; }
        })();
        if (saved) {
          try {
            const full = await api.getSession(saved);
            // Only adopt a session the server actually knows about AND that
            // reports its own id back: a restored-but-empty session is fine
            // (the user may have reloaded before saying anything), but a
            // malformed response must not strand us without a session.
            if (full && (full as any).session_id) {
              sid = saved;
              hist = (full as any).history ?? [];
            }
          } catch { /* gone or unreadable -> fall through to a new session */ }
        }
        if (!sid) {
          const s = await api.newSession();
          sid = s.session_id;
        }
        setSession(sid);
        try { localStorage.setItem(SESSION_KEY, sid); } catch { /* private mode */ }
        if (Array.isArray(hist) && hist.length) setChatItems(historyToItems(hist));
      } catch (e) {
        console.error("session create failed", e);
      }
    })();
  }, []);

  // Round LXIV: start a clean conversation on demand.
  //
  // Needed because Round LXIV also made a reload RESUME the last session (D3):
  // once the session is sticky, the user needs an explicit way to get a fresh
  // one. The old accidental behaviour -- refresh and everything is gone -- was
  // never a feature, but it did double as the only way to start over.
  //
  // The old session is NOT deleted: it stays on disk and in History. This
  // starts a new one and points the app at it.
  const newChat = useCallback(async () => {
    try {
      const s = await api.newSession();
      setSession(s.session_id);
      try { localStorage.setItem(SESSION_KEY, s.session_id); } catch { /* private mode */ }
      setChatItems([]);
      setChatBusy(false);
      setObjects([]);
      setJobs([]);
      setPage("chat");
    } catch (e) {
      console.error("new chat failed", e);
    }
  }, []);

  // Round LXVIII: adopt a past session chosen in History.
  //
  // The machinery already existed -- D3 built restore-by-id for the reload
  // path -- and this reuses it rather than adding a second way into a session.
  // What is NOT just wiring is the note at the end.
  //
  // A restored session's OBJECTS are gone. The store is deliberately never
  // serialized (cv_session_snapshot persists history and descriptors, never the
  // Seurat objects), so the backend returns the transcript plus `detached`
  // descriptors recording what USED to be loaded. Nothing could reach that
  // state on purpose before, so nothing rendered it. Restore makes it reachable
  // on purpose, and a transcript full of handles above an empty Objects panel
  // would be the product quietly implying the agent can still act on them.
  // Say it plainly instead, once, at the point of restore.
  const restoreSession = useCallback(async (id: string, full: any) => {
    setSession(id);
    try { localStorage.setItem(SESSION_KEY, id); } catch { /* private mode */ }
    const hist = Array.isArray(full?.history) ? full.history : [];
    const items: ChatItem[] = hist.length ? historyToItems(hist) : [];
    const detached: any[] = Array.isArray(full?.detached) ? full.detached : [];
    const live: any[] = Array.isArray(full?.objects) ? full.objects : [];
    if (detached.length && !live.length) {
      const names = detached
        .map((d) => d?.handle)
        .filter((h) => typeof h === "string" && h)
        .slice(0, 6);
      items.push({
        kind: "assistant",
        text:
          "Restored this conversation. The objects it used are not loaded any more" +
          (names.length ? ` (${names.join(", ")})` : "") +
          " — load them again from the Data / Upload tab before asking me to work on them.",
      });
    }
    setChatItems(items);
    setChatBusy(false);
    setObjects(live);
    setJobs([]);
    setPage("chat");
  }, []);

  const refreshSidebar = useCallback(async () => {
    if (!session) return;
    try {
      const [o, j] = await Promise.all([api.listObjects(session), api.listJobs(session)]);
      setObjects(o.objects ?? []);
      setJobs(j.jobs ?? []);
    } catch {
      /* non-fatal */
    }
  }, [session]);

  useEffect(() => {
    refreshSidebar();
    const t = setInterval(refreshSidebar, 4000);
    return () => clearInterval(t);
  }, [refreshSidebar]);

  return (
    <div className="app">
      <aside className="sidebar">
        <div className="brand">
          <img className="brand-logo" src={`${import.meta.env.BASE_URL}Symbol.png`} alt="CelliVerse logo" />
          <div className="brand-text">
            CelliVerse Agent
            <small>single-cell analysis agent {version && `· v${version}`}</small>
          </div>
        </div>
        <nav className="nav">
          {NAV.map((n) => (
            <button key={n.id} className={page === n.id ? "active" : ""} onClick={() => setPage(n.id)}>
              {n.label}
            </button>
          ))}
        </nav>
        <button className="btn secondary new-chat" onClick={newChat} title="Start a fresh conversation. The current one stays in History.">
          + New chat
        </button>
        {/* Round LXXXI (E1): the Objects panel used to live HERE, in a 240px
            column shared with nine nav buttons, with `flex: 1` so it took
            whatever was left. It is now the right-hand rail on the Chat page
            (components/ObjectRail.tsx), where there is width for the summary
            Round LXXIX put on screen, plus grouping by type and a filter — the
            three things a session with a hundred objects needs and this column
            had no room for. `flex: 1` moves to the spacer so the Jobs panel
            still sits at the bottom of the sidebar. */}
        <div style={{ flex: 1 }} />
        {jobs.length > 0 && (
          <div className="side-section">
            <h4>Jobs</h4>
            {jobs.map((j) => (
              <div key={j.id} className="mono" style={{ fontSize: 11 }}>
                {j.tool}: {j.status} {j.progress != null ? `(${Math.round(Math.min(100, Math.max(0, j.progress || 0)))}%)` : ""}
              </div>
            ))}
          </div>
        )}
      </aside>

      <main className="main">
        <div className="topbar">
          <strong style={{ textTransform: "capitalize" }}>{page}</strong>
          <span className="status">
            <span className={`dot ${healthy ? "ok" : "err"}`} />
            {healthy === null ? "connecting…" : healthy ? "connected" : "backend offline"}
            {session && ` · ${session}`}
          </span>
        </div>
        <div className="content">
          {page === "chat" && (
            <Chat
              session={session}
              onStateChange={refreshSidebar}
              items={chatItems}
              setItems={setChatItems}
              busy={chatBusy}
              setBusy={setChatBusy}
              onNavigate={setPage}
              objects={objects}
            />
          )}
          {page === "results" && <Results session={session} />}
          {page === "upload" && <FileUpload session={session} onUploaded={refreshSidebar} />}
          {page === "settings" && <SettingsPage />}
          {page === "history" && <History current={session} onRestore={restoreSession} />}
          {page === "package" && <PackageBrowser />}
          {page === "logs" && <Logs session={session} jobs={jobs} />}
          {page === "help" && <Help />}
          {page === "about" && <About />}
        </div>
      </main>
    </div>
  );
}
