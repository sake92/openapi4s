---
title: OpenAPI4s
description: Contract-first OpenAPI generation for Scala 3
pagination:
  enabled: false
---

# {{ page.title }}

Generate idiomatic Scala 3 models, validation types, HTTP server routes and clients from OpenAPI or JSON Schema documents.

Use OpenAPI4s with the [CLI](/reference/cli.html), [sbt plugin](https://github.com/sake92/sbt-openapi4s), [Mill plugin](https://github.com/sake92/mill-openapi4s) or [Deder plugin](https://github.com/sake92/deder-plugins/tree/main/deder-openapi4s)

- [Start with the quickstart](/tutorials/quickstart.html)
- [Solve a specific integration task](/howtos)
- [Look up CLI and backend details](/reference)
- [Understand when to choose OpenAPI4s](/explanation/why-openapi4s.html)

## Site map

- [Tutorials](/tutorials)
  {% for item in site.data.project.tutorials %}- [{{ item.label }}]({{ item.url}})
  {% endfor %}
- [How-Tos](/howtos)
  {% for item in site.data.project.howtos %}- [{{ item.label }}]({{ item.url}})
  {% endfor %}
- [Reference](/reference)
  {% for item in site.data.project.references %}- [{{ item.label }}]({{ item.url}})
  {% endfor %}
- [Explanation](/explanation)
  {% for item in site.data.project.explanations %}- [{{ item.label }}]({{ item.url}})
  {% endfor %}
