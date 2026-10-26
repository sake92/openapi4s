---
layout: explanation.html
title: Why OpenAPI4s?
description: When contract-first incremental Scala generation is the right fit
---

# {{ page.title }}

**The OpenAPI document stays the source of truth.** OpenAPI4s generates idiomatic Scala 3 models, validation types, server routes and clients, then semantically merges later contract changes into those sources without replacing handwritten implementations. New fields, constraints, polymorphic cases and endpoints arrive through regeneration; application logic stays where you wrote it.

**The generated code is the integration layer, not another runtime abstraction.** http4s output is ordinary http4s code and Sharaf output is ordinary Sharaf code. There is no additional endpoint DSL or layer of schema, codec and server-interpreter implicits to understand. That keeps the result simple for maintainers and makes it especially approachable for coding agents: inspect normal Scala files, fill the explicit `???`, compile, test and regenerate.

**Regeneration preserves the code you wrote.** Guardrail and template generators such as OpenAPI Generator or Swagger Codegen keep regeneration safe by treating generated transport code as disposable and putting user logic behind a separate interface. OpenAPI4s instead updates readable project sources in place. You can review a contract change and its direct Scala impact together.

Tapir is an excellent Scala-first choice: Scala owns the endpoint description and OpenAPI is derived from it. OpenAPI4s is usually simpler when OpenAPI already owns the contract because it does not require recreating that contract in a Scala endpoint DSL. If Scala should own the contract, choose a Scala-first library. If OpenAPI should own it and generated code should remain editable, **OpenAPI4s offers the shortest path from contract to native framework code.**

The trade-off is deliberate: merging is additive, so removals and renames need human cleanup, and unsupported features may produce an explicit TODO instead of a complete abstraction.
