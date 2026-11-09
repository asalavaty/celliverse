// Round LXIV (D6): the disclosure half of the approved error contract.
//
// Round LXII settled the shape with the user -- a calm sentence saying what
// happened and what to do next, with the technical cause in a separate `detail`
// field "revealed by the UI behind a toggle rather than shown by default".
//
// Uses a native <details>, deliberately: it is keyboard-operable and
// screen-reader-announced with no ARIA of our own, collapsed by default, and
// needs no state. Renders NOTHING without a detail, so it is safe to place on
// every message and every form.
//
// Round LXXXII moved it out of pages/Chat.tsx into its own module. It was
// written for the transcript and only the transcript used it, so when the
// Data / Upload page reported a 500 the server's `detail` -- the actual R error
// that would have named the cause of a failed 3.8 GB upload -- arrived in the
// browser and was dropped on the floor. That is the same producer-with-no-
// consumer shape as Round LXXIX's dropdown prefill and Round LXXX's token
// usage, three rounds running; a shared component is the structural answer.
export default function ErrorDetail({ detail }: { detail?: string }) {
  if (!detail) return null;
  return (
    <details className="err-detail">
      <summary>Technical details</summary>
      <pre>{detail}</pre>
    </details>
  );
}
