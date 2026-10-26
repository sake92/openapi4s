
TODOs:
- support default values
- add more tests https://github.com/swagger-api/swagger-parser/blob/master/modules/swagger-parser-v3/src/test/resources
- on release: bump README "Requirements" (Scala 3.9+, tupson >= 0.31.0, tupson-sttp for sttp clients)
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
