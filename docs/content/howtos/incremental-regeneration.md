---
layout: howto.html
title: Regenerate an evolving API
description: Safely apply contract changes without losing handwritten implementations
---

# {{ page.title }}

Run the same OpenAPI4s command whenever the contract changes. The generator parses the existing Scala source and merges additive changes into it.

For a realistic change:

1. Generate the initial API.
2. Replace generated `???` route bodies with application logic.
3. Add a schema field and an operation to the OpenAPI document.
4. Run the original generation command again.
5. Compile and exercise both the old and new routes.

The existing method body remains yours. New model fields, cases and route methods are added. A new endpoint initially contains `???`, making the missing implementation explicit at compile or runtime rather than silently inventing behavior.

Incremental generation is additive. Renames and removals are not destructive migrations: review and remove obsolete Scala code yourself. Commit generated sources so an ordinary code review shows both contract evolution and its Scala impact.
