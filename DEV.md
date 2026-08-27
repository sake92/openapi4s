
TODOs:
- support default values
- add more tests https://github.com/swagger-api/swagger-parser/blob/master/modules/swagger-parser-v3/src/test/resources
- on release: bump README "Requirements" (Scala 3.7+, tupson >= 0.20.0)
- mill-openapi4s plugin: expose `--client` / `--tags` flags (separate repo)

NOTE: `./mill __.test` requires `scala-cli` on PATH (used by the compilation suites
to compile generated sources: `IronValidationCompilationSuite`,
`GithubJiraCompilationSuite`). Override the binary with `SCALA_CLI_BIN` if needed.


```sh

./mill clean

./mill __.reformat

./mill __.test

./mill __.publishLocal
```

```sh

# RELEASE
./scripts/release.sh 0.6.4

```
