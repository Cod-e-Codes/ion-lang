---
name: creating-ion-skills
description: >-
  Author new Cursor Agent Skills for the ion-lang repository. Use when creating
  or editing skills in .cursor/skills/, writing SKILL.md frontmatter, adding
  references/ or scripts/, or asking how to structure project skills for Ion.
  Do not use for personal skills in ~/.cursor/skills/.
---

# Creating Ion Project Skills

Skills in this repo live under `.cursor/skills/<skill-name>/SKILL.md`. They are version-controlled and shared with anyone who clones the repo.

## When to add a project skill

Add a skill when the workflow is:

- Repeated across sessions (compiler stages, test harness, LSP)
- Ion-specific (ownership rules, `test_expectations.tsv`, cgen patterns)
- Too large for a one-off answer but too narrow for `ION_SPEC.md`

Keep one-off user preferences in Cursor user rules, not project skills.

## Layout

```
.cursor/skills/<skill-name>/
├── SKILL.md              # Required: frontmatter + core workflow (<500 lines)
├── references/           # Optional: loaded only when SKILL.md links them
└── scripts/              # Optional: repeatable CLI helpers
```

Folder name must match frontmatter `name` (lowercase, hyphens, max 64 chars). Use gerund names when possible: `adding-ion-features`, `finding-ion-bugs`.

Never write skills to `~/.cursor/skills-cursor/` (Cursor built-ins).

## Frontmatter

```yaml
---
name: skill-name
description: >-
  Third-person WHAT + WHEN. Include trigger terms users actually say.
paths:                    # Optional: scope to matching files
  - src/**
disable-model-invocation: true   # Only for manual /slash-command skills
---
```

| Field | Guidance |
|-------|----------|
| `description` | Third person. State capability and trigger phrases. Max 1024 chars. This is the routing key. |
| `paths` | Use for file-specific skills (`src/**`, `tests/**`). Omit for repo-wide orientation. |
| `disable-model-invocation` | Default unset (auto-invoke). Set `true` for destructive or explicit-only workflows. |

## Authoring rules

1. **Concise** - Assume the agent is competent. Only add Ion-specific facts not in general training.
2. **Progressive disclosure** - Core workflow in `SKILL.md`; details in `references/` one level deep.
3. **Forward slashes** - All paths in markdown use `/`, even on Windows.
4. **Verbatim user text** - If the user supplies exact wording for a skill, copy it unchanged.
5. **Link repo anchors** - Point to `ION_SPEC.md`, `README.md`, and existing skills instead of duplicating them.
6. **Verifiable steps** - End workflows with commands (`cargo test`, `test_runner.sh`) the agent can run.

## Workflow

```
- [ ] 1. Pick name; confirm it does not collide with existing .cursor/skills/
- [ ] 2. Draft description (WHAT + WHEN + trigger terms); read aloud as "The agent should use this when..."
- [ ] 3. Write SKILL.md: quick start, checklist or numbered workflow, anti-patterns
- [ ] 4. Add references/ only if SKILL.md would exceed ~200 lines
- [ ] 5. If ion-lang behavior is affected, add a row to ion-lang "Specialized skills" table
- [ ] 6. Verify: name matches folder, description has triggers, no Windows backslash paths
```

## Patterns that work here

**Specialized skill** (scoped): `adding-ion-features` uses `paths` for `src/**`, links `references/stage-checklist.md`.

**Orientation skill** (broad): `ion-lang` has no `paths`; indexes other skills in a table.

**Manual-only skill**: set `disable-model-invocation: true` when the user must type `/skill-name`.

## Anti-patterns

- Duplicating `ION_SPEC.md` or README build instructions in full
- Vague names (`helper`, `utils`, `tools`)
- Deep reference chains (`references/a/b/c.md`)
- Time-sensitive "before August 2025" notes without a deprecated section
- Creating personal skills in the repo when they are not Ion-specific

## Examples in this repo

| Skill | Pattern |
|-------|---------|
| `ion-lang` | Repo orientation, skill index |
| `adding-ion-features` | `paths` + stage checklist + anti-patterns |
| `ion-integration-tests` | `paths: tests/**` + manifest format |
| `creating-ion-skills` | Meta-skill for authoring |

For generic Cursor skill mechanics (description examples, script layout), read the user-level `create-skill` skill if available.
