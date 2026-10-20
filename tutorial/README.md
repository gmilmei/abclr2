# Programming in ABCL/R2

A Guided Introduction to Objects, Concurrency, and Reflection

This English-language book is the primary user documentation for the current
SBCL port. Start with the included [book PDF](../doc/tutorial.pdf);
no PDF build is needed for reading.
Edit tutorial.tex and the files in
chapters/ to change it. Archived documents in historical/doc/ and Masuhara's
thesis in doc/Masuhara_1992.pdf are supplementary sources, not prerequisites
for starting the book's guided learning path.

From the repository root:

~~~sh
make -C tutorial
make -C tutorial check
make -C tutorial baseline
~~~

The PDF build needs SBCL, LuaLaTeX, and the LaTeX packages used in tutorial.tex.
It uses fontspec with the installed Libertinus Serif and Libertinus Sans
fonts, and Noto Sans Mono for code.
The example checks need SBCL and src/saved_abclr2 built from the
current SBCL source; no Python, external Lisp libraries, or package downloads
are required. The runner uses ASDF/UIOP supplied with SBCL. Run make at the
repository root to build the interpreter. Build logs,
translated examples, copied libraries, and test transcripts are kept in build/.
The final PDF is written to doc/tutorial.pdf.
The book includes an alphabetical concept and protocol index with linked
section and page references, alongside its syntax overview and source reading map.
See VALIDATION.md for the final acceptance checks, environment, and limits.
All required check scripts are distributed in tutorial/tests/.
For examples containing `require`, interactive sessions must add `lib/`
to `*library-directories*`; the default searches only the current directory.
The original-example tests explicitly set an absolute path to their copied
libraries under `build/original-tests/lib/` before loading the examples.
Builds and checks create their build/ directories automatically.
run-tests.lisp is the shared native test runner; cases.lisp holds transcript
contracts and sessions/ contains readable ABCL/R2 inputs. Source excerpts
are extracted from the distributed originals on each run. Every case starts
a fresh saved-image process with its original timeout and working directory.
The suite contains 720 runtime checks, plus five listing regression cases.
Run `make -C tutorial serialization-check` for 13 source/FASL checks of
macro hygiene and uninterned-symbol identity in translated files.
make -C tutorial runner-check adds 21 infrastructure checks, including process
timeouts, partial logs, error detection, source integrity, and byte-exact excerpts.
It is included in check. For a different parser-test image, run:

~~~sh
sbcl --script tutorial/tests/run-tests.lisp parser-tests --image /path/to/image --label candidate
~~~

The label must be an identifier and selects build/parser-tests/candidate/.

The original-source reading map links all 34 example files and nine libraries
to their roles and walkthroughs, including schematic, unfinished, and historical
sources. Run make -C tutorial coverage-check to validate the inventory,
uniqueness, and section labels; it is included in check. This structural check
does not certify program behavior or replace reading the explanations.

The opening chapters assume OOP and some Lisp, not Common Lisp, actors,
or reflection. A Lisp reading guide precedes scripts. Guided original-source
projects explain future-ex and inherit through bindings, message paths,
application replies, and execution continuations. The pcontrol walkthrough
explains recursive Fibonacci and quicksort, group evaluator queues, priority
propagation, and the limits of monitor measurements. Run
make -C tutorial pcontrol-check for twelve isolated original-source checks;
logs and generated files stay in build/pcontrol-tests/.
The Time Warp walkthrough introduces virtual time, the car-wash application,
rollback histories, evaluator context, and scheduler policies. Run
make -C tutorial timewarp-check for forty component/application checks;
logs stay in build/timewarp-tests/. A supplemental two-object application
forces a late input, cascading rollback, cancellation of an already executed
output, and corrected replay under both evaluator scheduler policies.
Other cancellation arrival orders and experimental evaluator variants are
not certified.
The NR/NS walkthrough distinguishes evaluator replacement from queue-policy
replacement and explains old/new objects' captured evaluators. Run
make -C tutorial timewarp-variants-check for 53 successful checks plus two
reconstructed-original failure observations. Corrected NR/NS initializer calls and
NS script handling now allow the three-car simulation to complete. Checks
cover returned values, single evaluation, script execution, controller replies,
and the assignment records. No simulation algorithm was changed; arbitrary
rollback and performance are not certified. The exact original helper is
preserved as comments beside the corrections. Logs stay in build/timewarp-variants/.
Run make -C tutorial reading-check for 40 checks of the reading guide and
these original-source projects. Translations and logs stay in build/reading-tests/.
The reflection walkthroughs explain the built-ins protocol sketches,
sender-sensitive user scripts, MCP's meta-circular interpreter, and meta-meta.
Run make -C tutorial reflection-check for 74 isolated checks using distributed
sources and supplemental inputs. The original script-installation
example now works; 38 regressions cover script changes and queued dispatch
and 18 cover corrected Inherit group metadata, cross-group execution, child
creation, and copying without altering inheritance policy. The exact Inherit
original lines are commented beside the correction. Script updates are checked
on both primary-metaobject paths. The optimized cache correction is recorded
in ../doc/implementation.md. Logs stay in build/reflection-tests/.
Run make -C tutorial group-check for 88 ownership checks: 27 for reflective
MCP, Meta-Meta, Fault-Tolerant and Time-Warp application generators, plus 61
for nine locally corrected NRM constructions. The latter check application
and meta-level ownership, identity links, opacity, and initializer ownership.
No hidden state is exposed or group guessed; legacy checks cover ownership only. Logs stay in build/group-tests/.
The multicast walkthrough replays the original library's commented example
and explains target templates, positional replies, group-local evaluator
setup, and submission versus aggregate completion. Run
make -C tutorial multicast-check for 24 checks. Missing and duplicate replies
and zero-object target lists expose collector limits, not supported recovery
semantics. Original sources remain unchanged; logs stay in build/multicast-tests/.
The copy walkthrough follows the original Fibonacci/copy experiment and
separates new object identity from shared lexical environments, scripts,
pending replies, group/evaluator policy, and execution startup. Run
make -C tutorial copy-check for 52 probes, including 23 state checks.
The corrected initializer representation preserves values in separate local
bindings while retaining referenced data, aliases, and outer environments.
This is not deep copying or an atomic snapshot. The unchanged original
lines are commented beside the correction.
Logs and extracted original examples stay in build/copy-tests/.
The watch walkthrough distinguishes outgoing sends from incoming requests,
object/group/top-level installation, structured and filtered reporters,
and evaluator restoration. Run make -C tutorial watch-check for 41 probes.
Repeated installation and preconfigured group wrappers have explicit
restoration limits; watched copying retains the copy-library restrictions.
Watch remains unchanged; Copy has its documented initializer correction.
Logs stay in build/watch-tests/.
The report walkthrough follows copied object identities, receiving-side
arrival/dispatch/completion events, all eight filter combinations, custom
reporters, and alias-preserving restoration. Run make -C tutorial report-check
for 59 probes. Ten formatter regressions check correct sender/reply labels
and arguments, including nil fields; structured records retain the actual
envelope fields. An unmatched request
exposes a stalled-dispatch boundary, not supported recovery behavior.
Copy's sharing and snapshot limits still apply. The one-line formatter correction is
documented in ../doc/implementation.md; the replaced original line is
commented beside the correction. Copies and transcripts stay in build/report-tests/.
The synthetic host-event exercise uses the existing signal interface without
installing OS callbacks. Run make -C tutorial host-events-check for 25 probes
of argument shapes, nil reply/sender fields, single-slot consumption and
overwrite, disabled delivery, and handler restoration. The helpers access
the actual host variable, not an ABCL environment binding. This does not
certify OS integration or Express preemption; inputs and transcripts stay
in build/host-event-tests/.
The loading chapter includes all public option defaults, source/Lisp/FASL
selection, local versus global execution context, and module search and
registration. Run make -C tutorial loading-check for 35 probes. Distinct
markers in generated scratch translations and compiled files identify the
selected cache path without changing ABCL source. The suite checks missing
source even with cached Lisp present, and restores its temporary library
search path. Inputs and generated caches stay in build/loading-tests/.
The heavy-meta walkthrough starts from the original library's commented
counter, then follows structural matching, guard evaluation, and fallback.
Run make -C tutorial guards-check for 24 probes, including group configuration,
sender/reply bindings, failed-guard effects, and the known stalled-dispatch
boundary after an unmatched request. Guard/dispatch behavior is unchanged;
copies and transcripts stay in build/guard-tests/.
An optional historical comparison, outside make check and the book's
learning path, is available as make -C tutorial heavy-old-check (20 probes).
It reproduces the old generator's protocol mismatch with the current
translator, not intended WHERE semantics or an older complete runtime.
Copies and transcripts stay in build/heavy-old-tests/. For actual guarded
programs use lib/heavy-meta.abclr2, not lib/heavy-meta-old.abclr2.
The retry walkthrough follows the original fault-tolerant experiment from
client/server initialization through meta-level rejection and retransmission.
Run make -C tutorial retry-check for six state/queue checks and printed-count
assertions on unchanged sources; logs stay in build/retry-tests/. This tests
one repaired acceptance condition, not general fault tolerance.
The breakpoint walkthrough replays break.abclr2's interactive editing
session and explains lexical frames, local updates, and resume replies.
Run make -C tutorial break-check for six isolated checks plus inspector
transcript assertions. Scratch sources and logs stay in build/break-tests/.
The original Express walkthrough follows both clock variants, the controller's
completion continuations, and the different interruption/locking policies.
Run make -C tutorial express-check for 47 checks: ten elementary
dispatch checks on the newer/older pairs and 32 staged queue checks on the
corrected newer library. The queue checks use existing meta-level controls
to stage normal/Express FIFO and mixed-priority dispatch, distinct senders
and reply targets, and empty completion. Five more checks run the unchanged
non-atomic phase twice through sampling, stop, resumed completion, and retained
state. A separate expected-failure probe records the cyclic wait when a
synchronous :lock reaches a metaobject already awaiting :grab. These results
do not certify general preemption, fairness, or atomic correctness.
The Express examples remain unchanged; the newer library's replaced original
lines are commented beside the corrections. Logs stay in build/express-tests/.
The interrupt walkthrough explains reify-args-to-meta, branch substitution,
the evaluator wrapper, and effects that survive interruption. Run
make -C tutorial interrupt-check for thirteen checks. The runner retains
the second reify-args example with its local ownership correction, omitting
a trailing commented alternative that triggers the current reader's
delimiter error. The complete file still fails the baseline reader check.
Translations and logs stay in build/interrupt-tests/.

Original examples are the behavioral baseline; corrections retain the replaced
original lines as local comments. The baseline reconstructs original example code and
uses distributed libraries with their documented corrections. check records observations in
build/original-tests/baseline.md, then runs 46 supplemental tutorial checks
and a separate expected-failure Express observation.
The baseline is observational: known failures remain in its report;
its exit status is not a declaration of full port compatibility.
Original source bytes are compared before and after the run; copied original
files are also checked directly, without requiring a hashing library.
The book includes original source excerpts and identifies teaching extensions.
For display, prepare-listings.lisp removes each excerpt's common outer indentation
and expands leading tabs; relative indentation and expressions are retained.
Inline port notes are omitted from display; book line references use this
code-only view. source-views.lisp also reconstructs original code for historical
probes, without changing files used by the interpreter. Run make -C tutorial
source-views-check for 24 checks of these projections.
This external SBCL build step generates ordinary LaTeX listing paths; the
TeX sources contain no embedded Lua. LuaLaTeX remains the default engine.
Presentation copies stay in the PDF build directory, never in examples/.
Run make -C tutorial layout-check to check this normalization.
Twenty-four of the 34 example files and three libraries still match the historical
archive. Corrected files keep their replaced original lines as comments beside
the documented corrections; no duplicate source backups are distributed. Checks use the distributed sources
and verify that neither those sources nor their test copies change during a run.

check also runs 16 parser regression checks and the original future example.
The conservative repair phase is closed. Unmatched-dispatch recovery,
Express lock/grab coordination, and the Future all-values inconsistency
remain unchanged; they are not mandatory fixes or certified intended semantics.
See ../doc/implementation.md for the change review and stopping boundary.
Six separate semantic probes check first-script selection, shared lexical
bindings, and the historical all-values empty-buffer inconsistency using
the unchanged future library. Run them independently with
make -C tutorial semantics-check. Rebuild with make at the repository root.

To run an example interactively, start ./src/saved_abclr2 from the repository
root and enter:

~~~lisp
(inc "tutorial/examples/01-basics" :use-source t)
[calculator <== [:square 7]]
~~~

inc creates a translated .lsp next to the source. The automated check
copies examples and libraries to build/tutorial-tests/ first, so its
generated files remain there.

The saved executable identifies itself as version 0.59 (July 1995), while
historical/doc/readme identifies the distribution as version 0.60 (September 1998).
The same banner is present in src/declare.lsp; it alone does not indicate
that the executable was built from older sources.
The PDF explains the current implementation and distributed examples.
Historical defect diagnoses and before/after comparisons are kept in
../doc/implementation.md and VALIDATION.md, not in the tutorial narrative.

The bounded Express experiment currently blocks on an evaluator-front
:grab request and is recorded as an expected failure, not a completion
success. A lock reply is needed to reach the next capture point, but the
metaobject is already waiting for that context. Earlier odd samples and apparent completion preceded a translation
correction that restored the library's mode updates. They do not establish
atomic preemption correctness. See ../doc/implementation.md. The book's
Express introduction instead uses checked short exchanges with an idle clock.
