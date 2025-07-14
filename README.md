# openapi4s

OpenApi generators for Scala.

Here is a small video demo: https://youtu.be/kf0vGrlKNb8

## Features and Benefits
- incremental generator
  - doesn't touch the code that you added manually
  - only adds new properties/methods/classes
- lenient parser+generator
  - if something is not supported it will still (mostly) work
  - you can adapt your openapi spec to work gradually

## Limitations
- JSON only
- only *named* entities, no anonymous objects

---

## Generators

### Sharaf backend
Supports almost all features:
- controllers
- discriminated models (sealed traits)
- enums (scala3 singleton enums)
- validations
- query params

### Http4s backend
Supports some features:
- routes (controllers)
- discriminated models (sealed traits)
- enums (scala3 singleton enums)

TODO: query params, validation..  
Contributions welcome!

---

## Plugins

### Mill plugin

See https://github.com/sake92/mill-openapi4s




