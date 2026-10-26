---
layout: tutorial.html
title: Quickstart
description: Generate Scala 3 models and http4s routes from OpenAPI
---

# {{ page.title }}

This tutorial generates Scala 3 models and http4s routes from a small OpenAPI contract.

## 1. Create the contract

Save this as `openapi.yaml`:

```yaml
openapi: 3.1.0
info:
  title: Users API
  version: 1.0.0
paths:
  /users/{id}:
    get:
      tags: [Users]
      operationId: getUser
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: integer
            format: int64
      responses:
        "200":
          description: A user
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/User"
components:
  schemas:
    User:
      type: object
      required: [id, name]
      properties:
        id:
          type: integer
          format: int64
        name:
          type: string
```

## 2. Run the generator

```shell
cs launch --scala-version 2.13.16 ba.sake::openapi4s-cli:0.9.0 \
  -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models circe \
  --framework http4s \
  --url openapi.yaml \
  --baseFolder src/main/scala \
  --basePackage com.example.api
```

OpenAPI4s creates the `models` and `routes` packages below `com.example.api`. Route methods deliberately contain `???`: you fill in the implementation.

## 3. Implement and regenerate

Replace the generated placeholder in `UsersRoutes` with your application logic. When the contract gains a field or endpoint, run the same command again. OpenAPI4s adds the new generated structure while preserving your implementation.

Next, [generate an http4s API with Iron validation](/howtos/http4s-iron.html), or consult the [CLI reference](/reference/cli.html) for other backends.
