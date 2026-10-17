# Implementation and compatibility notes

Status: 2026-10-17. This is a compact record of source changes, compatibility
boundaries, and known defects. For language instruction, use
[Programming in ABCL/R2](tutorial.pdf); for acceptance evidence,
see [the validation report](../tutorial/VALIDATION.md).

## Origin and preservation policy

The original implementation and user's guide are by **Hidehiko Masuhara**,
from the ABCL Group at the University of Tokyo's Yonezawa Laboratory
(Akinori Yonezawa). See the [original guide](../historical/doc/users-guide-e),
[distribution readme](../historical/doc/readme), and
[acknowledgements](../historical/doc/acknowledgements.text).

This port retains the original object/message protocols, cooperative
process machinery, and reflective architecture. Logical
concurrency does not imply one operating-system thread per object.
Twenty-four of the 34 example files and three libraries remain byte-identical
to the historical archive. Corrected sources retain the replaced original lines as comments at each
correction, with a short explanation. No duplicate source backups are distributed.
The historical directory contains documentation and platform tips.
New teaching programs
and test inputs are separate. Compatibility means preserving the model,
not silently treating historical bugs as intended language semantics.
Design limits are not repair targets: shared references in Copy, the
single pending host-event slot, and protocol assumptions remain as designed.
Corrections must restore an existing contract without adding new semantics.

## Changes from the historical source

The source comparison identifies eleven changed files under `src/`, plus the
root Makefile, six libraries (Report, Express/current and old, Copy, and
Heavy-Meta/current and old), and ten example helpers. Generated FASLs and the saved executable are build products,
not source changes. Most host-specific additions are guarded by `#+:SBCL`;
some shared pathname, translation, diagnostic, and cleanup changes are not.

| Files | Adaptation |
| --- | --- |
| [Makefile](../Makefile), [defabclr2.lsp](../src/defabclr2.lsp) | Build with SBCL's script mode; save a standalone executable with `sb-ext:save-lisp-and-die`; enter the ABCL/R2 top level on startup. Add `install-sbcl` and FASL cleanup. |
| [defabclr2.lsp](../src/defabclr2.lsp) | Select the CLtL2/Common Lisp path and bundled `defsystem30`; supply compatibility package names `LISP` and `USER`, isolating the build system's `provide`/`require` symbols; map `special-form-p` to `special-operator-p`. |
| [abclr2.system](../src/abclr2.system), [defsystem30.lsp](../src/defsystem30.lsp), [defabclr2.lsp](../src/defabclr2.lsp) | Use explicit `./` source/binary directories; handle current-directory, empty-directory, and empty pathname-name cases in the old build system; omit an unsuitable compile-variable declaration on SBCL. |
| [declare.lsp](../src/declare.lsp), [translate-nrm.lsp](../src/translate-nrm.lsp) | Select Common Lisp imports and `do`; use `defparameter` for string-valued version/source-extension settings on SBCL to permit reloading without string-constant redefinition conflicts. |
| [inc.lsp](../src/inc.lsp) | Emit `eval-when (:load-toplevel :execute)`; on SBCL preserve uninterned symbols and shared identity with gensym printing and reader labels; muffle host style warnings; check for a partial translation before deleting it on failure. File-selection and application protocols are retained. |
| [catch-error.lsp](../src/catch-error.lsp), [toplevel.lsp](../src/toplevel.lsp) | Select condition-handler error trapping; rebind the ABCL error stream at startup; muffle style warnings during top-level execution. This is diagnostic handling, not an operating-system interrupt implementation. |
| [low-message.lsp](../src/low-message.lsp) | Supply the missing argument to an existing warning's format directive. This corrects a historical diagnostic defect, not queue ordering. |
| [parse.lsp](../src/parse.lsp) | Add narrow SBCL handling of exactly `(THE T expression)` in `parse-special`; parse the operand with the original reply destination. |
| [primary-meta.lsp](../src/primary-meta.lsp) | Correct the historical optimized dispatcher: rebuild its cached dispatch closure after `:add-script` and `:remove-script`. Share the refresh operation with `:set-evaluator`. Update both the active loop binding and the completion callback's slot, without resetting evaluation or changing queue order, state, or reply protocols. |
| [report.abclr2](../lib/report.abclr2) | Pass `M R S`, not `M S R`, from `default-reporter` to `default-reporting-function`. Only printed sender/reply labels change; event triples and report lifecycle are unchanged. Original lines are commented at the correction. |
| [express.abclr2](../lib/express.abclr2) | In each queued-dispatch branch of `:end`, dequeue the selected queue and match `[Message Reply Sender]` before dispatch. Keep Express priority, FIFO order, mode transitions, and the suspended-context branch unchanged. Original lines are commented at the correction. |
| [copy.abclr2](../lib/copy.abclr2) | Wrap each state value in a zero-argument closure for the existing `:self-evaluate` protocol. Preserve initializer ordering, reference identity, outer environments, scripts, queued envelopes, and construction policies. Original lines are commented at the correction. |
| [tw-eval.abclr2](../examples/time-warp/tw-eval.abclr2) | In NR/NS call the `:self-evaluate` operand with `funcall`, not `eval`; add NS's missing `:script` branch using the existing `call-script-body` helper. No scheduling, timestamps, output histories, rollback, or controller algorithm changes. Original lines are commented at the correction. |
| [inherit.abclr2](../examples/misc/inherit.abclr2) | Expose the existing creation argument as `Group`, which `create-reifying-point-of` reads when creating the application identity. Retain `Group-manager` and its execution uses. No kernel fallback, script lookup, queue, or inheritance-policy changes. Original lines are commented at the correction. |
| [MCP.abclr2](../examples/misc/MCP.abclr2), [meta-meta.abclr2](../examples/misc/meta-meta.abclr2), [fault-tolerant.abclr2](../examples/misc/fault-tolerant.abclr2), [tw-meta.abclr2](../examples/time-warp/tw-meta.abclr2) | Add the same `Group` identity field to five further reflective generators, retaining `Group-manager` for execution. No dispatch, evaluator, retry, or rollback algorithm changes. Original lines are commented at each correction. |
| [heavy-meta.abclr2](../lib/heavy-meta.abclr2), [heavy-meta-old.abclr2](../lib/heavy-meta-old.abclr2), Report, Express/current and old; [reify-args.abclr2](../examples/misc/reify-args.abclr2), [priori-meta.abclr2](../examples/pcontrol/priori-meta.abclr2), [pqueue-eval.abclr2](../examples/pcontrol/pqueue-eval.abclr2), [tw-meta-eval.abclr2](../examples/time-warp/tw-meta-eval.abclr2) | Assign the known construction `GMgr` to the new denoted identity before application initializers run. Each local addition has a provenance comment; no original executable line is replaced. No reflective access to NRM state, group inference, or kernel changes. |

The parser addition accommodates modern host macro expansions encountered
by the original `provide` path. It does not implement general type assertions
or multiple values; other nonconstant `THE` type forms remain unsupported.
Sixteen parser regressions and the unchanged original future example check
the boundary, single evaluation, type-error behavior, and reply forwarding.

The banner still says version 0.59 (July 1995), while the historical readme
identifies version 0.60 (September 1998). The same banner string is in the
source; it is not evidence that the executable uses an older source tree.
Other historical Lisp-host branches have not been revalidated by this port.

## Conservative review and stopping boundary

The targeted repair phase is closed after review. Host/build adaptations
enable the existing implementation on SBCL; they also change diagnostics
and generated-file handling as listed above. Contract repairs correct
stale script caches, mislabeled formatter arguments, missing queue-envelope
bindings, lost symbol identity, and mismatched initializer/script
representations. They are not claims of identical observable behavior:
each deliberately changes the documented faulty result.

Ownership repairs need a separate qualification. Reflective generators add
a visible `Group` state field; NRM generators create/cache the denoted
identity earlier and set its GID before application initialization.
Consequently, `group-of` and initializers can now observe the known owner
where they previously observed nil. This is an intentional metadata
correction supported by the constructor's existing `GMgr`, not an SBCL-only
adaptation or a claim that nil was impossible historically. It does not
change method selection, the execution group, or NRM state opacity.

No retained correction requires a new reply, recovery, coordination or
scheduling policy. The unresolved cases below are preserved historical
inconsistencies, not certified intentional semantics. They are excluded
from this repair phase; any behavioral change requires a separate,
explicitly agreed contract and compatibility review.

The book teaches the current distributed implementation and example
protocols, without defect histories or before/after comparisons. Those
belong here and in the validation report. Its Express introduction uses
the checked idle-clock exchanges, not the blocked atomic experiment;
the reifying-control walkthroughs load separately prepared excerpts.

## Corrected defects and retained inconsistencies

The table distinguishes corrected historical defects from remaining findings
on the current image. Source-level defects can be identified without claiming to have
reproduced every older host's behavior. None establishes an SBCL regression
merely because it is observed on SBCL.

| Area | Evidence and present status |
| --- | --- |
| Dynamic scripts: corrected | The optimized dispatcher previously retained the old script list. The original user-script example now returns `Hansei` to Showman and `Scratch!` to Audience. Nineteen regressions per dispatch path check addition/removal, precedence, first-only removal, duplicates, nonmatching removal, reinstallation, retained state, and queued replies through the completion callback. The old path is unchanged and passes the same cases. Management requests still do not promise replies. |
| Express queues: corrected | Both `:end` branches previously used unbound envelope fields without dequeuing. Thirty-two staged regressions now check normal/Express FIFO, Express-before-normal priority, sender/reply preservation, and empty-queue completion without duplicate execution. They use existing meta-level lock/release/end controls, not an active-clock test. |
| Translation symbol identity: corrected | SBCL's `case` introduces an uninterned `#:MODE`; printing it as `MODE` made its local binding shadow Express's persistent state. Printing gensyms with shared reader labels preserves lexical identity, including distinct same-named symbols. Thirteen regressions check source/FASL loading, state updates, macro hygiene, distinct symbols, and repeated quoted identity. Other host branches are unchanged. |
| Express / `atomic`: open | An in-flight synchronous `:lock` reaches the metaobject after it has started waiting for `:grab`. The evaluator cannot reach its next capture point until that same metaobject grants the lock: a cyclic wait. A forwarding-only trace checks this order and the absence of a lock reply before blocking. Five positive checks run the unchanged non-atomic phase twice through sampling, stop, resumed completion, and retained state. Atomic ordering remains uncertified; no coordination or scheduler redesign was made. Earlier results before the symbol correction are not interruption evidence. |
| Reify example | The complete [reify-args file](../examples/misc/reify-args.abclr2) fails at the reader boundary around a commented alternative. The unchanged first excerpt and explicitly extracted interrupt forms can run; neither is a repaired complete original. |
| Copy initializers: corrected | The original helper used a raw value where the evaluator expects a function. Each closure now returns its captured value, without evaluating or copying the referenced data. Twenty-three state regressions cover numbers, nil, symbols, strings, list/object/function identity, aliases, independent slot assignment, shared mutation, and group/explicit-generator construction. No deep-copy or atomic-snapshot guarantee is added. |
| Time-Warp NR/NS representations: corrected | Both variants now return initializer values, not initializer functions; NS accepts the same script representation as the other evaluators. Fifteen checks per evaluator cover creation-policy capture, literals/function-valued payloads, single evaluation, compiled bodies, scripts, and LVT. Four checks per car-wash run plus transcript assertions require actual completion and all three assignments. Separate reconstructed-original sessions reproduce the original defects; small successful cases do not certify general rollback or performance. |
| Report formatter: corrected | Ten regressions check actual formatter arguments and printed labels for arrival, begin, end, nil reply, and nil sender. The existing structured-event and filter checks still pass; no payload, sender, destination, or lifecycle protocol was changed. |
| Unmatched dispatch: retained | Report and regular Heavy-Meta remove an unmatched request but can leave later work queued. Both primary dispatch paths also warn without scheduling completion; Heavy-Meta-old explicitly continues. The original guide's WHERE discussion says to discard unmatched messages, but does not specify the complete post-warning recovery contract. This supports a diagnosis, not an unambiguous universal `:end` repair. No completion, failure reply or recovery policy was added. |
| Inherit group metadata: corrected | The generator previously exposed only `Group-manager`; the kernel reads `Group` to initialize the application object's GID. The additional state field restores ownership without changing execution context. Eighteen regressions cover ownership versus meta-level groups, local overrides, inherited methods across groups, child creation, source ownership, and default copying. This does not infer groups for arbitrary custom metaobjects. |
| Further reflective group metadata: corrected | The same mismatch was reproduced in MCP, both Meta-Meta generators, Fault-Tolerant, and the Time-Warp application generator. Twenty-seven checks cover six identities, canonical and execution bindings, replies, state and recovery. |
| Non-reifying ownership: corrected locally | Nine identities now receive the generator's known `GMgr` before application initialization: Heavy-Meta/current and old, Report, Express/current and old, Interrupt, Pcontrol application/evaluator, and Time-Warp evaluator. Each generator assigns only the new denoted identity's GID via `(setf (r2sys::object-data-gid [den Metaobject]) GMgr)` (using its local object name). No kernel fallback or state inspection is added. Sixty-one regressions check ownership, unchanged meta-level ownership, stable identity links, opaque NRM state and, for the seven application cases, ownership during initialization. Legacy checks certify ownership only, not guard or lock compatibility. |
| Future buffer: retained | [future.abclr2](../lib/future.abclr2) replies with a scalar to an already waiting `all-values` consumer but with a list when a value is already buffered. Preserving the request kind would change the waiting-consumer representation/protocol; no normalization is introduced without an agreed contract. The book uses `next-value` plus explicit termination instead. |

The old [heavy-meta-old.abclr2](../lib/heavy-meta-old.abclr2) is a separate
historical protocol variant, not a drop-in replacement for the regular
library: its embedded-guard contract differs from the current translator's
separate guard representation. Its comparison tests do not certify an
older complete runtime.

## Final verification and scope

From a fresh source tree:

```sh
make
make -C tutorial check layout-check
make -C tutorial heavy-old-check  # optional historical comparison
```

The acceptance environment was SBCL 2.6.9-1.fc44 on Linux. The regular suite
passed 720 runtime assertions and 21 runner checks; the source map covers
all originals, five listing cases pass, and the optional comparison adds
20 assertions. The baseline deliberately retains `FAIL`/`TIMEOUT`
observations, including the now explicitly expected blocked Express teaching
experiment and the lock/grab race. The count includes five active non-atomic
checks, but excludes these failure observations. A successful test-driver
exit is not full port certification.
The original-example baseline reconstructs original example code from inline
`PORT-BEGIN` / `PORT-ACTIVE` / `PORT-END` notes (`;;|` prefixes original lines).
Libraries retain their documented corrections. This is not a fully historical
runtime or a byte-exact archival export. The variant suite separately tests
corrected and reconstructed-original helpers. The SBCL source-view utility is
used only for test copies and book listings, never by the interpreter.
Book line references use the code-only view, omitting these provenance notes.
Twenty-four source-view checks cover all sixteen corrected files and LF/CRLF,
insertions, replacements and a missing final newline.

No scheduler redesign, general copying guarantee, arbitrary Time-Warp
rollback guarantee, or OS-event integration is claimed. Express lock/grab,
unmatched dispatch and Future buffering remain documented and unchanged.
Closing this phase means the agreed conservative scope has been checked,
not that every historical error is repaired. Optional extensions or further
semantic repairs are separate work, not required distribution fixes.
