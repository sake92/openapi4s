
TODOs:
- support default values
- add more tests https://github.com/swagger-api/swagger-parser/blob/master/modules/swagger-parser-v3/src/test/resources
- on release: update the CLI version in `docs/_data/project.yaml` and the fenced CLI examples in `docs/content/`
- mill-openapi4s plugin: expose `--client` / `--tags` flags (separate repo)

Build prerequisites: JDK 21+, [Deder](https://sake92.github.io/deder/), and `scala-cli`.
The build schema is pinned in `deder.pkl`.

NOTE: `deder exec -t test` requires `scala-cli` on PATH (used by the compilation suites
to compile generated sources: `IronValidationCompilationSuite`,
`GithubJiraCompilationSuite`). Override the binary with `SCALA_CLI_BIN` if needed.

```sh

deder clean

scalafmt

deder exec -t test

deder exec -t publishLocal
```

```sh

# RELEASE
./scripts/release.sh 0.6.4

```

## Context7

Context7 indexes `docs/content/` for [OpenAPI4s](https://context7.com/sake92/openapi4s).
Keep examples in fenced Markdown blocks so the same examples are available to
Context7 and on the documentation site.

Set the `CONTEXT7_API_KEY` repository secret to enable the refresh workflow.
Pushes to `main` that change indexed docs or `context7.json` trigger a refresh.
You can also run **Refresh Context7 Docs** manually in GitHub Actions.
