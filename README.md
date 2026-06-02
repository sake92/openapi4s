# openapi4s

OpenApi generators for Scala.

Here is a small video demo: https://youtu.be/kf0vGrlKNb8

## Features and Benefits
- incremental generator
  - doesn't touch the code that you added manually
  - **additive only**, adds new properties/methods/classes
- lenient parser+generator
  - if something is not supported it will still (mostly) work
  - you can adapt your openapi spec to work gradually

## Limitations
- JSON only
- only *named* entities, no anonymous objects

---

## Generators


### Circe models backend
- discriminated models (sealed traits / enums)
- enums (singleton enums)

### Tupson models backend
- discriminated models (sealed traits / enums)
- enums (singleton enums)

### Sharaf framework backend
Supports almost all features:
- controllers
- validations
- query params

### Http4s framework backend
Supports some features:
- routes (controllers)

TODO: query params, validation..  
Contributions welcome!

---

## Usage

### Mill plugin

See https://github.com/sake92/mill-openapi4s

### CLI

You can use `openapi4s-cli` with Coursier launcher to generate your sources:

```shell
cs launch ba.sake::openapi4s-cli:0.7.0 -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models tupson \
  --framework sharaf \
  --url openapi.json \
  --baseFolder src \
  --basePackage com.example
```

You can now combine model and framework generation independently:

```shell
# circe models + http4s routes
--models circe --framework http4s

# circe models + sharaf controllers
--models circe --framework sharaf

# models only
--models tupson --framework none

# framework only (expects existing com.example.models)
--models none --framework http4s
```
