# Agent Instructions

## Build

- Deder build: use `deder` (schema pinned in `deder.pkl`); JDK 21+.
- Generator code is Scala 2.13; generated code is Scala 3 (scalafmt uses scala3 dialect).

## Commands

| Task | Command |
|------|---------|
| All tests | `deder exec -t test` |
| Module tests | `deder exec -t test -m openapi4s-test` |
| Single suite | `deder exec -t test -m openapi4s-test ba.sake.openapi4s.<Suite>` |
| Format | `scalafmt` (CLI, `.scalafmt.conf` at root) |
| Compile | `deder exec -t compile` |
| Publish local | `deder exec -t publishLocal` |
| Release | `./scripts/release.sh <version>` (commits, tags, pushes) |

Notes:

- `deder exec -t test` needs `scala-cli` on PATH (compilation suites compile generated Scala 3 sources). Override the binary with `SCALA_CLI_BIN`.
- `GithubCompilationSuite` / `JiraCompilationSuite` are `.ignore`d by default; un-ignore to run locally (slow, needs big heap). Compiler JVM heap: `OPENAPI4S_COMPILE_XMX` (default `6g`).
- On release: bump versions in `README.md` (CLI example, backend dependency table) — see TODOs in `DEV.md`.

## Key Conventions

- Codegen stack: swagger-parser (parsing) + regenesca (Scala 3 source generation).
- Generated sources must stay compilable — update generator or compilation suites when changing codegen.
- Test specs live in `openapi4s/src/test/resources/`; refresh GitHub/Jira specs with `scripts/download-specs.sh`.
- Run `scalafmt` before committing.
- Commit messages use conventional prefixes (`feat:`, `fix:`, `docs:`, `test:`).
- Active worktrees live in `.worktrees/` — changes there are on separate branches, not `main`.
- Never commit any plan/spec documents to the repo (e.g. `docs/superpowers/` is gitignored) — keep them out of git entirely.

## External References

| Need | File |
|------|------|
| User docs | `README.md` |
| Dev notes / TODOs / release | `DEV.md` |

## Commit Attribution

AI commits MUST include:

```
Co-Authored-By: (the agent's name and attribution byline)
```
