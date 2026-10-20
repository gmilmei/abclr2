# ABCL/R2 for SBCL

A port of the historical ABCL/R2 implementation to Steel Bank Common Lisp
(SBCL), with an English-language book and executable example checks.

ABCL/R2 is an object-oriented, concurrent, reflective language built on
Common Lisp. Objects communicate through messages; programs can inspect
and customize their execution machinery, including evaluators, message
dispatch, and scheduling policies.

The original implementation is by **Hidehiko Masuhara**, developed in the
ABCL Group at the University of Tokyo's Yonezawa Laboratory, led by
Akinori Yonezawa. This repository preserves that work while adapting its
host integration to SBCL. Targeted example and library
corrections are documented with replaced original lines preserved as comments;
the port, new documentation, and teaching examples are later work.

## Start here

Read [Programming in ABCL/R2](doc/tutorial.pdf):
*A Guided Introduction to Objects, Concurrency, and Reflection*.

The PDF is included; no document build is needed to read it.
The book is the primary user documentation. It assumes object-oriented
programming and some Lisp, but not Common Lisp, actors, or reflection.
It leads from a first session through message protocols and reflection
to guided readings of all original examples, including Pcontrol and Time
Warp. It includes exercises, worked solutions, a syntax overview, an
original-source reading map, and a concept/protocol index.

For implementation changes and known defects, see the compact
[implementation and compatibility notes](doc/implementation.md).

## Build and try it

You need SBCL and Make. From the repository root:

```sh
make
./src/saved_abclr2
```

The build creates a standalone SBCL executable; no system-wide installation
is needed. Start it from the repository root for the book's relative paths.
At the ABCL/R2 prompt, try:

```lisp
[object calculator
  (script
    (=> [:square x] !(* x x)))]

[calculator <== [:square 7]]
; => 49
```

`<==` sends a message and waits for its reply; `!` sends that reply.
Type `:q` to quit or `:reset` to reset the session. Keep experiments in
your own files rather than editing the original examples.

Before loading examples that use `require`, add the distributed libraries
to the session's search path (with the interpreter started at the repository root):

```lisp
[*library-directories* := (cons #P"lib/" *library-directories*)]
(inc "examples/misc/future-ex" :use-source t)
```

By default, `require` searches only the current directory, not `lib/`.
The first matching `.abclr2` source in `*library-directories*` is selected.
Keep the trailing slash on directory entries; use an absolute directory
when starting elsewhere. Explicit `inc` and `inc-lib` paths do not use
this search list. After `:reset`, configure it again.

## Build the book and run checks

```sh
make -C tutorial pdf
make -C tutorial check layout-check
```

The PDF build additionally requires LuaLaTeX, the LaTeX packages listed in
[tutorial.tex](tutorial/tutorial.tex), and the installed fonts Libertinus
Serif, Libertinus Sans, and Noto Sans Mono. The resulting PDF is written
to `doc/tutorial.pdf`.

Build and test helpers use SBCL, including its bundled ASDF/UIOP; they do
not require Python or external Lisp packages. Generated files and test
transcripts stay in `build/`.

See the [book's build and test guide](tutorial/README.md) for individual
checks and [validation report](tutorial/VALIDATION.md) for the tested
environment, acceptance results, and limits.

## Status and compatibility

This is a historical research implementation, not a newly designed language
runtime. Preserving its protocols and execution model takes priority over
making every original example appear to succeed.

The current suite contains 720 regular runtime assertions, alongside
runner, source-map, and listing checks. However, some original examples
still expose reader errors or a blocked Express
interruption request. Those observations are recorded separately, not counted as
successful functionality. General Express preemption and Time-Warp
correctness are not certified. The historical dynamic script-dispatch cache
defect, Report labels, missing Express queue dequeues, and loss of uninterned
macro-symbol identity in translated SBCL files have been corrected.
Copy's state initializers now preserve values without changing reference sharing.
The Time-Warp NR/NS helpers now handle initializer and script representations;
their small car-wash checks pass without changing the simulation algorithms.
Reflective example generators now expose the group metadata required for
object ownership. Nine NRM constructions now assign that known owner locally,
without exposing their hidden state or changing the kernel.
See [implementation notes](doc/implementation.md)
for changes, intentional design limits, and remaining defects.
The conservative repair phase is closed. Unmatched-dispatch recovery,
Express lock/grab coordination, and Future `all-values` representation
remain unchanged historical inconsistencies, not planned automatic fixes.

## Source tree and historical material

| Path | Contents |
| --- | --- |
| [src/](src/) | Interpreter, translator, runtime, and SBCL integration |
| [examples/](examples/) | Original example programs |
| [lib/](lib/) | Historical libraries with documented corrections |
| [tutorial/](tutorial/) | Book sources, teaching examples, and checks |
| [doc/](doc/) | Ready-to-read book PDF, Masuhara's thesis, and implementation notes |
| [historical/](historical/) | Original documentation and platform tips |

Supplementary references are Masuhara's
[1992 thesis](doc/Masuhara_1992.pdf), the
[original English user's guide](historical/doc/users-guide-e), and the
[historical distribution readme](historical/doc/readme).
The [original acknowledgements](historical/doc/acknowledgements.text)
credit additional contributions and bundled utilities.
Historical installation instructions describe the original Lisp hosts,
not the current SBCL build.
