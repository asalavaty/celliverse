// Renders assistant message text as GitHub-Flavored Markdown.
//
// The LLM writes its replies in markdown (bold, headers, bullet lists, tables,
// inline code). react-markdown (with remark-gfm for tables/strikethrough)
// converts it to real markup, which styles.css then styles under the `.md`
// class.
//
// Used ONLY for assistant (agent) messages — user messages stay plain text.
//
// Clickable handles: when the agent lists loaded objects (e.g. a clarification
// prompt), each handle appears either as a `handle:<handle>` link or as inline
// `code`. Both render as a clickable chip that hands the handle to the parent
// (via onHandleClick) so the user never has to type it.
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

// A handle looks like <prefix>_<id>: clusto_083933z9tcs9, obj_083828k34f1t,
// mat_084255mdgh99, typo_084039xcl5ha, df_..., sce_..., etc.
const HANDLE_RE = /^(clusto|markoclust|markocell|purity|features|typo|cellset|obj|sce|spe|mat|df|art)_[A-Za-z0-9_]+$/;

interface Props {
  text: string;
  onHandleClick?: (handle: string) => void;
}

export default function Markdown({ text, onHandleClick }: Props) {
  return (
    <div className="md">
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{
          // `handle:<handle>` links -> clickable chip (no navigation).
          a({ href, children }) {
            if (href && href.startsWith("handle:")) {
              const h = decodeURIComponent(href.slice("handle:".length));
              return (
                <button type="button" className="handle-chip" onClick={() => onHandleClick?.(h)}>
                  {children}
                </button>
              );
            }
            return (
              <a href={href} target="_blank" rel="noreferrer">
                {children}
              </a>
            );
          },
          // Inline `code` that is exactly a handle -> clickable chip.
          code({ children }) {
            const s = String(children ?? "").trim();
            if (onHandleClick && HANDLE_RE.test(s)) {
              return (
                <button type="button" className="handle-chip" onClick={() => onHandleClick(s)}>
                  {s}
                </button>
              );
            }
            return <code>{children}</code>;
          },
        }}
      >
        {text}
      </ReactMarkdown>
    </div>
  );
}
