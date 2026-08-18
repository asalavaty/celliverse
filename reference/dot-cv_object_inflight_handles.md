# Handles of a session's job(s) still \`queued\`/\`running\`, i.e. handles that must not be evicted right now regardless of how long they've sat unused.

A heavy job's child process already received its own COPY of every input
object at launch time (via callr's serialization boundary - see
cv_launch_heavy() in agent_worker.R), so evicting the PARENT store's
copy mid-job can't corrupt the job itself. The real risk this guards
against is an update-style tool (addClustoData/addTypoData) finishing a
long-running heavy job and calling cv_object_update() on a handle that
got evicted in the meantime – cv_object_update() already resurrects an
evicted handle safely (see above), so this protection is a
belt-and-suspenders measure against an unnecessary/confusing
evict-then-immediately-resurrect cycle for an object that was never
actually idle, not a correctness requirement.

## Usage

``` r
.cv_object_inflight_handles(sess)
```
