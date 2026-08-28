# Agent Instructions

## Build

- Mill build: use `./mill` (version pinned in `build.mill`).
- Generator code is Scala 2.13; generated code is Scala 3 (scalafmt uses scala3 dialect).

## Commands

| Task | Command |
|------|---------|
| All tests | `./mill __.test` |
| Module tests | `./mill openapi4s.test` |
| Single suite | `./mill openapi4s.test.testOnly ba.sake.openapi4s.<Suite>` |
| Format | `scalafmt` (CLI, `.scalafmt.conf` at root) |
| Compile | `./mill __.compile` |
| Publish local | `./mill __.publishLocal` |
| Release | `./scripts/release.sh <version>` (commits, tags, pushes) |

Notes:

- `__.test` needs `scala-cli` on PATH (compilation suites compile generated Scala 3 sources). Override the binary with `SCALA_CLI_BIN`.
- `./mill __.reformat` from `DEV.md` does not exist in this build — use the `scalafmt` CLI instead.
- `GithubCompilationSuite` / `JiraCompilationSuite` are `.ignore`d by default; un-ignore to run locally (slow, needs big heap). Compiler JVM heap: `OPENAPI4S_COMPILE_XMX` (default `6g`).
- On release: bump versions in `README.md` (CLI example, backend dependency table) — see TODOs in `DEV.md`.

## Key Conventions

- Codegen stack: swagger-parser (parsing) + regenesca (Scala 3 source generation).
- Generated sources must stay compilable — update generator or compilation suites when changing codegen.
- Test specs live in `openapi4s/src/test/resources/`; refresh GitHub/Jira specs with `scripts/download-specs.sh`.
- Run `scalafmt` before committing.
- Commit messages use conventional prefixes (`feat:`, `fix:`, `docs:`, `test:`).
- Active worktrees live in `.worktrees/` — changes there are on separate branches, not `main`.
- Never commit spec/plan documents (e.g. `docs/superpowers/specs/`, `docs/superpowers/plans/`) to the repo — keep them out of git entirely.

## External References

| Need | File |
|------|------|
| User docs | `README.md` |
| Dev notes / TODOs / release | `DEV.md` |
| Design docs | `docs/superpowers/specs/` |

## Commit Attribution

AI commits MUST include:

```
Co-Authored-By: (the agent's name and attribution byline)
```
