---
name: researching-pl-literature
description: >-
  Find and summarize academic programming language research (PhD theses, POPL,
  PLDI, ICFP, OOPSLA papers) relevant to Ion design and implementation. Use
  when the user asks for PL research, papers on ownership, borrow checking,
  linear types, channels, Send traits, transpilation, or evidence for language
  or compiler decisions in ion-lang.
---

# Researching PL Literature for Ion

Ion is move-only, no-GC, no-escape references, channels-only concurrency, C backend. Use research to inform design tradeoffs, not to justify scope creep.

Read `ION_SPEC.md` and `ion-lang` skill before recommending paper-driven changes.

## Research goal

For each request, clarify:

1. **Decision** - What Ion question needs evidence? (e.g. channel typing, drop semantics)
2. **Constraints** - Move-only, no GC, C ABI, structural Send (non-negotiable unless user says otherwise)
3. **Deliverable** - Annotated bibliography + "applies to Ion" notes + optional implementation sketch

## Search strategy

Use web search with precise terms. Prefer primary sources (PDF, ACM DL, arXiv, author pages).

**Venues:** POPL, PLDI, ICFP, OOPSLA, ESOP, ECOOP, SNAPL, TOPLAS, JFP.

**Query templates:**

```
<topic> ownership types survey
<topic> linear types programming languages
Rust borrow checker formalization
channel types session types concurrency
Send Sync type system message passing
C transpilation correctness compiler
move semantics formal semantics
```

**Authors often cited for Ion-adjacent work:** Matsakis (Rust), Jung et al. (RustBelt), Walker (linear types), Honda (session types), Wadler, Pierce (TAPL).

After finding a hit, search the author’s related work and "cited by" for follow-ons.

## Topic map for Ion

| Ion area | Search angles | Typical keywords |
|----------|---------------|------------------|
| Ownership / moves | affine/linear types, uniqueness | `ownership types`, `linear logic`, `use-after-move` |
| No-escape `&` | stack discipline, region types | `stack references`, `region-based memory`, `escape analysis` |
| Channels / spawn | message-passing, session types | `pi calculus`, `CSP`, `session types`, `channel algebra` |
| Structural Send | effect systems, capability | `Send trait`, `typestate`, `concurrency types` |
| No GC / explicit heap | manual memory, RAII lowering | `drop semantics`, `resource types`, `C codegen` |
| Transpilation to C | verified lowering, FFI | `compiling to C`, `FFI semantics`, `translation validation` |

Detailed reading notes: [references/paper-seeds.md](references/paper-seeds.md).

## Evaluate a paper for Ion

For each source, record:

| Field | Content |
|-------|---------|
| Citation | Authors, venue, year, URL |
| Problem | What they formalize or implement |
| Mechanism | Core technique in one paragraph |
| Ion fit | Direct (implementable), indirect (analogy), or out of scope |
| Cost | Compiler complexity, user friction, runtime needs |
| Verdict | Adopt / borrow idea / monitor / skip |

Skip papers that require GC, full effect inference, or runtime reflection unless the user explicitly wants exploratory notes.

## Output template

```markdown
## Question
<user's design question>

## Short answer
2-4 sentences, no hype.

## Sources
1. [Title](url) - one-line relevance
2. ...

## Recommended direction for Ion
Concrete next step: spec section, tc rule, or experiment.

## Out of scope
What popular PL ideas do not fit Ion constraints.
```

## Implementation bridge

When a paper suggests a rule:

1. Check `ION_SPEC.md` for existing semantics
2. Map to compiler stage (usually `src/tc/ownership.rs` or `src/tc/mod.rs`)
3. Propose a negative `.ion` test before positive tests
4. Flag if runtime (`runtime/`) or cgen must change

## Anti-patterns

- Citing surveys without reading abstracts or skimming sections
- Recommending GC, green threads, or full dependent types without user request
- Treating Rust or Swift papers as drop-in Ion specs (ABI and rules differ)
- Academic suggestions that violate Ion no-escape or channels-only model without calling out the conflict
- Long literature dumps without a recommended direction
