# Paper seeds for Ion

Starting points for literature search. Verify URLs and newer follow-up work at search time; titles are stable anchors.

## Ownership, moves, linearity

- **Tofte & Talpin** (POPL 1994) — region-based memory, stack-of-regions discipline — [POPL94 PDF](https://www.irisa.fr/prive/talpin/papers/popl94.pdf) — `spec`, `runtime`
- **Walker, Morrisett, Crary, Glew** (POPL 1999) — "Typed Memory Management in a Calculus of Capabilities" — [abstract](https://www.cs.cornell.edu/talc/papers/capabilities-abstract.html) — `tc`, `cgen`
- **Walker** (2002) — "Substructural Type Systems" chapter in Pierce, *Advanced Topics in Types and Programming Languages* — canonical tc survey — `tc`
- **Wadler** (1990) — "Linear Types Can Change the World!" — linear logic → resource-safe imperative code — `tc`
- **Hofmann** (ESOP 2000 / NJC 2000) — LFPL: linear types compile to malloc-free C with correctness proof — [NJC PDF](http://web.cs.ucla.edu/~palsberg/tba/papers/hofmann-njc00.pdf) — `cgen`, `tc`
- **Haller & Odersky** (ECOOP 2010) — "Capabilities for Uniqueness and Borrowing" — [PDF](http://lampwww.epfl.ch/~phaller/doc/capabilities-uniqueness2.pdf) — `tc`, concurrency
- **RustBelt** (Jung et al., POPL 2018) — λRust + Iris; formalizes ownership, borrows, Send/Sync — [project](https://plv.mpi-sws.org/rustbelt/popl18/) — `tc`, `spec`
- **Matsakis & Klock** — "The Rust Language" (ownership + borrow checker overview) — informal but implementation-relevant

Ion takeaway: move-only and borrow rules have formal precedents; Ion is stricter (no escape) than Rust.

## RustBelt follow-ons (cited-by / successor line)

- **Iris from the ground up** (Jung et al., JFP 2018) — modular concurrent separation logic underpinning RustBelt — [PDF](https://abizjak.github.io/documents/publications/iris-ground-up.pdf) — `tc` (vocabulary only for Ion)
- **Stacked Borrows** (Jung et al., POPL 2020) — operational aliasing model for Rust references/unsafe — [project](https://plv.mpi-sws.org/rustbelt/stacked-borrows/) — **skip** for Ion (no lifetimes, no raw-pointer aliasing calculus)
- **Tree Borrows** (Villani et al., PLDI 2025) — successor aliasing model — [ACM](https://dl.acm.org/doi/10.1145/3735592) — **skip** for Ion
- **RustBelt meets relaxed memory** (Dang et al., POPL 2020) — concurrent + weak memory — [ACM](https://dl.acm.org/doi/10.1145/3371102) — `runtime` (monitor; Ion has no formal memory model yet)
- **RustHornBelt** (Matsushita et al., PLDI 2022) — functional correctness via prophecy/lifetime logic — monitor for future Ion proofs
- **RefinedC** (Sammler et al., PLDI 2021) — refinement types + separation logic for C verification — [ACM](https://doi.org/10.1145/3453483.3454036) — `cgen`, translation-validation vocabulary
- **RefinedRust** (Gäher et al., PLDI 2024) — RustBelt + refinement types, foundational Coq proofs — [PDF](https://robbertkrebbers.nl/research/articles/refinedrust.pdf) — monitor (too heavy for Ion tc today)
- **VerusBelt** (Stern et al., PLDI 2026) — semantic model for Verus proof-oriented Rust — [PDF](https://iris-project.org/pdfs/2026-pldi-verusbelt.pdf) — monitor
- **Linear Effects, Exceptions, and Resource Safety** (Mellies & Vásquez, arXiv 2025) — destructors, moves, drop order in affine CBPV — [arXiv](https://arxiv.org/html/2510.23517v1) — `spec`, `tc`

Ion takeaway: borrow RustBelt's *Send-as-closed-world property* story, not lifetime logic or stacked/tree borrows. RefinedC/Cogent patterns matter more for C lowering assurance.

## Concurrency and channels

- **Hoare** — CSP (communicating sequential processes)
- **Milner** — pi-calculus
- **Kobayashi, Pierce & Turner** (POPL 1996 / TOPLAS 1999) — "Linearity and the Pi-Calculus" — linear channels, race freedom — [POPL](https://dl.acm.org/doi/10.1145/237721.237804) — `tc`, `Send`
- **Honda, Vasconcelos & Kubo** (ESOP 1998) — session-type primitives for structured communication — `tc`
- **Gay et al.** (POPL 2010) — "Modular Session Types for Distributed Object-Oriented Programming" — [ACM](https://dl.acm.org/doi/10.1145/1706299.1706335) — `tc`, typestate bridge
- **Padovani et al.** — "Typing Copyless Message Passing" (Singularity-style move into channels) — [arXiv:1202.2086](https://arxiv.org/abs/1202.2086) — `tc`, `spec`
- **Pierce & Turner** — concurrent objects in pi-calculus (LNCS 1995); see also Honda session line

Ion takeaway: structural Send is lighter than full session types; papers justify channel-centric design vs shared memory.

## Compiling to C

- **Necula** (PLDI 2000) — translation validation for GCC — [PDF](https://people.eecs.berkeley.edu/~necula/Papers/tv_pldi00.pdf) — `cgen`
- **Rinard & Marinov** (1999) — credible compilation — [PDF](https://people.csail.mit.edu/rinard/paper/sma03.pdf) — `cgen`
- **Sewell, Myreen & Klein** (PLDI 2013) — translation validation C→binary (seL4) — [PDF](https://www.trustworthy.systems/publications/nicta_full_text/6449.pdf) — `cgen`
- **O'Connor et al.** (ICFP 2016 / JFP 2021) — Cogent: uniqueness types, certifying compiler to C — [arXiv:1601.05520](https://arxiv.org/pdf/1601.05520) — `cgen`, `tc`, `spec`
- **CompCert** (Leroy et al.) — verified C compiler (heavyweight correctness vocabulary)
- **FFI semantics** papers (e.g. interfacing safe and unsafe code across language boundaries)

Ion takeaway: test-driven validation (integration + `cgen` grep) is the practical analog for this repo size; cite Cogent/Hofmann for move-only→C correctness *patterns*, not full Isabelle/Coq.

## Effect systems and Send-like traits

- **Gordon & Abadi** — subtyping calculi (behavioral types background)
- **Strom & Yemini** (IEEE TSE 1986) — typestate: legal operations change with state — `tc`, protocols
- **Gay et al.** (POPL 2010) — session types unify with typestate on channel objects
- **Rust Send/Sync** RFCs and RustBelt (structural closure of thread-safe types)

Ion takeaway: structural Send checking can cite capability/typestate + linear-channel tradition without adopting full effect inference.

## Typestate / protocols (OOPSLA, ESOP)

- **Strom & Yemini** (1986) — original typestate — [IBM Research](https://research.ibm.com/publications/typestate-a-programming-language-concept-for-enhancing-software-reliability)
- **Aldrich et al.** (OOPSLA 2009) — typestate-oriented programming — [ACM](https://dl.acm.org/doi/10.1145/1639950.1640073)
- **Garcia et al.** (TOPLAS 2014) — "Foundations of Typestate-Oriented Programming" — [ACM](https://dl.acm.org/doi/10.1145/2629609)
- **Honda et al.** (ESOP 1998) — session primitives — channel protocol typing
- **Dardha et al.** — Mungo/StMungo: session types → Java typestate — [PDF](https://www.dcs.gla.ac.uk/~ornela/publications/KDPG17.pdf)
- **Saffrich et al.** (OOPSLA 2024) — "Law and Order for Typestate with Borrowing" — Rust-flavored; monitor for Ion

Ion takeaway: borrow typestate *ideas* for optional protocol linting; do not adopt Java/Plaid object models.

## How to extend this list

When a paper proves useful:

1. Add one row with venue, year, and Ion relevance tag (`tc`, `cgen`, `spec`, `runtime`)
2. Link follow-up work from "cited by" on Google Scholar or Semantic Scholar
3. Do not duplicate full paper summaries here; keep SKILL.md workflow lean
