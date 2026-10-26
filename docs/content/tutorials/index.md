---
title: Tutorials
description: Learn OpenAPI4s by generating a small API
pagination:
  enabled: false
---

# {{ page.title }}

{% for item in site.data.project.tutorials %}- [{{ item.label }}]({{ item.url}})
{% endfor %}
