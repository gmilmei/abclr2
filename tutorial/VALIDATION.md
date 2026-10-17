# Book validation

Validation date: 2026-10-17.

This records the acceptance scope of *Programming in ABCL/R2*, not a
certification of every execution path in the historical implementation.

## Reproducing the automated checks

From a source distribution with no generated FASLs, saved image, or build
directory, run from its root:

~~~sh
make
make -C tutorial check heavy-old-check layout-check pdf
~~~

`heavy-old-check` is an optional historical comparison, not a prerequisite
for the book's learning path or part of `check`.

The final acceptance run used SBCL 2.6.9-1.fc44 and LuaHBTeX 1.24.0
(TeX Live 2026), installed Libertinus fonts and Noto Sans Mono. The runtime
checks started in a separate source-only copy, without existing generated artifacts or
any additional local development directories. The interpreter was built
there before running the checks. No downloads or Python were required.

Results:

- 720 regular runtime assertions passed, including 88 further group-ownership,
  18 Inherit group, 53 Time-Warp variant,
  23 Copy-state, 38 script-update,
  ten Report formatter, 32 staged Express queue, five active non-atomic
  Express, and 13 serialization regressions.
  The former Express teaching completion assertion is now a separate
  expected-failure observation, not included in this count.
- 20 optional historical heavy-meta comparison assertions passed.
- 21 test-runner self-checks passed.
- All 34 distribution example files and nine libraries have unique reading-map
  entries pointing to existing explanation sections.
- Five source-listing normalization cases passed. All 81 displayed source
  excerpts passed the range/content check against the active code-only view;
  only common outer indentation is removed for presentation.
- The runner's source-integrity checks passed. Twenty-four of 34 examples
  and three libraries match the historical archive byte-for-byte. Replaced original lines are commented beside each correction;
  no duplicate source backups are distributed. The baseline reconstructs
  original example code from these notes, while keeping corrected libraries.
  Twenty-four source-view checks cover the sixteen corrected files and text boundaries.

The public build/test entry points use SBCL, not Python, and the TeX source
contains no embedded Lua. LuaLaTeX is still required by the current font
configuration; portability to a different engine has not been certified.

## PDF and editorial scope

The book has 97 pages, including its syntax overview, original-source
reading map, and linked concept/protocol index. All pages were rendered
and visually reviewed for spacing, code alignment, clipping, tables,
headings, and page breaks. The final LaTeX pass has no unresolved references,
missing citations, or overfull/underfull box warnings. The installed TeX
packages produce an existing LaTeX release-version warning; the build
completes successfully. Fonts are embedded with Unicode mappings.
Temporary review images were removed after inspection.

The editorial review covers the guided learning sequence, Lisp prerequisites,
protocol roles, original-source explanations, exercise/solution agreement,
and the distinction between intended semantics and bounded observations.
Historical documentation remains supplementary rather than required reading.
The PDF teaches the current distributed code, not its defect history.
Before/after comparisons and port diagnoses are kept in implementation notes
and this report. Its Express introduction uses the existing checked idle-clock
exchanges; the IF and interrupt walkthroughs use prepared executable excerpts.
Source excerpts show active code with provenance comments omitted; the copied
source files and the interpreter are not modified for PDF presentation.

## Deliberate limits

The original-example baseline remains observational: the complete
`reify-args` file has a reader error and the original Express clock blocks
on an evaluator-front `:grab` request. The bounded Express teaching program
blocks there too. These results are recorded, not counted as successful functionality.
Separate unmatched-dispatch observations characterize the retained behavior,
not successful recovery or an agreed replacement policy. Reconstructed-original
Time-Warp NR/NS failure probes confirm the specific representation defects,
not successful simulation.
The corrected NR/NS helpers pass 53 checks across the standard, NR and NS
protocols and two three-car simulations. These require completed controller
replies and all three assignment records, not merely intermediate printed work.
The changes invoke initializer functions and add NS's missing script case;
they do not redesign scheduling, timestamping, output histories, or rollback.

Staged Express queue probes check FIFO, priority, sender/reply preservation,
and empty completion using existing meta-level controls. They do not certify
general active preemption, context resumption, or `atomic` correctness.
Five separate checks run the original non-atomic Express phase twice,
including sampling, stop, resumed completion, and retained state. A separate
failure probe forwards all messages unchanged and records `:grab` before
an in-flight synchronous `:lock` arrives, with no lock reply before blocking.
The metaobject waits for a context that the evaluator cannot produce until
the same metaobject grants the lock. No coordination redesign was attempted.
Time-Warp checks cover selected histories and a staged rollback
cascade, not every cancellation order. Synthetic host events do not certify
operating-system integration. Copy, Watch, Report, Multicast, and historical
alternative generators retain the boundaries explained in the book.

The initial book acceptance left historical runtime/library/example bugs
untouched. A subsequent targeted correction refreshes the optimized
dispatch cache after script addition/removal; the unchanged original
user-script example now passes. Both dispatch paths pass the same 19
regressions, including queued replies, first-only removal and unchanged
state. A subsequent one-line Report correction fixes printed sender/reply
labels while retaining the original event triples and lifecycle. Ten new
regressions exercise actual formatter calls and output. Express `:end` now
dequeues and binds its selected message envelope in both queue branches;
active-clock interruption remains open. A narrow SBCL serialization correction
preserves macro-generated uninterned symbols and their shared identity in
translated Lisp/FASLs. Previously, a `case` temporary shadowed Express's
persistent mode; earlier odd samples and apparent bounded completion therefore
did not exercise the intended interruption path. Thirteen regressions check
the correction without changing scheduling or lock policy.
Copy's initializer helper now returns each captured state value through
the evaluator's callable representation. Twenty-three new checks verify
value preservation, separate bindings, retained aliases/references, shared
mutation, and construction options. The former numeric-state failure is
replaced by positive state checks, not retained as an expected success.
Deep copying, active-context cloning, and atomic snapshots remain outside
the library's contract.
Inherit now exposes its creation group through the canonical `Group` state
field while retaining `Group-manager` for execution. Eighteen checks verify
ownership, inherited context across groups, child creation, and default copy.
No core group-inference fallback or inheritance-policy change was introduced.
Five further reflective generators in MCP, Meta-Meta, Fault-Tolerant and
the Time-Warp application helper now expose the same identity metadata.
Twenty-seven checks verify groups, retained execution bindings, identities,
stateful replies, and recovery. Sixty-one additional checks cover nine
locally corrected NRM constructions: Heavy-Meta/current and old, Report,
Express/current and old, Interrupt, Pcontrol application/evaluator and the
Time-Warp evaluator. Generators assign their known GMgr to the new denoted
identity before application initialization. Identity links and meta-level
ownership remain stable; opaque state is not made reflective. These checks
do not certify the legacy guard/lock protocols or Express atomic preemption.
The narrow SBCL
parser correction is also part of the interpreter.
The compact [implementation and compatibility notes](../doc/implementation.md)
record these changes and remaining inconsistencies. The conservative repair
phase is closed. Unmatched-dispatch recovery, Express lock/grab coordination,
and Future `all-values` normalization remain outside that scope, not pending
mandatory repairs. The guide's discard rule does not by itself establish
a complete post-warning recovery protocol. Existing defect-characterization
assertions preserve these observations; their passing does not certify recovery.
Ownership corrections intentionally change formerly nil group metadata;
they do not establish general behavioral identity with the historical image.
Intentional design limits are not repair targets. Any later semantic changes
require a separate contract and explicit scope.
