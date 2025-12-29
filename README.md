# django-check

Static N+1 query detection for Django (LSP-based)

`django-check` is a static analyzer and Language Server that detects N+1 query patterns in Django code before runtime. It inspects queryset construction and related-field access to warn when relations are accessed without proper prefetching (`select_related`, `prefetch_related`).

<img width="1353" height="415" alt="image" src="https://github.com/user-attachments/assets/99bb6566-6372-4fef-a765-a2d4247a0311" />


The goal is not to replace runtime profilers, but to shift performance feedback left, into the editor.

> [!WARNING]
> This project is in active development and **not yet production-ready**.
>
> - APIs are unstable
> - Diagnostics may be incomplete or incorrect
> - Expect breaking changes without notice

## Why this exists

Django’s ORM makes it easy to accidentally introduce N+1 queries that:

* pass tests,
* look correct in code review,
* only show up under load.

Runtime tools (django-silk, nplusone) are reactive. `django-check` is proactive.

## Key properties

* Static analysis only (no runtime hooks)
* Zero runtime overhead
* LSP-based diagnostics at edit time
* No code instrumentation required
* Works with any editor that supports LSP

## What it detects (today)

* Iteration over QuerySets followed by related-field access
* Missing `select_related` / `prefetch_related` for:

  * ForeignKey
  * OneToOne
  * ManyToMany
  * reverse relations (basic cases)

Example pattern:

```python
users = User.objects.all()
for user in users:
    user.profile.bio
```

## What it does NOT do (yet)

* Does not execute Django code
* Does not observe real queries
* Does not infer dynamic queryset construction
* Limited support for custom queryset methods
* No deep analysis of properties that hide ORM access

## Demo

_WORK IN PROGRESS_

## Installation

### Binary

_WORK IN PROGRESS_

This installs the `django-check` binary, which also acts as an LSP server.

## Editor integration

### Neovim 0.11+ (native LSP)

Neovim 0.11 ships with a stable built-in LSP client.

Minimal setup:

```lua
-- lua/init.lua
vim.lsp.enable("djch")
```

```lua
-- lsp/djch.lua
return {
  cmd = { "djch", "server" },
  filetypes = { "python" },
  root_markers = { 'manage.py', 'pyproject.toml', '.git' }
}
```

This registers `django-check` as a first-class LSP server.

If you are already attaching multiple LSPs to Python buffers (e.g. Pyright), Neovim will merge diagnostics correctly.

### VS Code (Extension)

_WORK IN PROGRESS_
<!-- ```bash -->
<!-- code --install-extension django-check -->
<!-- ``` -->

## Example

### Problematic code

```python
# N+1 query detected
users = User.objects.all()
for user in users:
    print(user.profile.bio)
```

Explanation:

* `users` is evaluated once
* `user.profile` triggers one query per iteration

### Corrected code

```python
users = User.objects.select_related("profile").all()
for user in users:
    print(user.profile.bio)
```

`django-check` will clear the diagnostic once the relation is prefetched.

## How it works (high level)

1. Parse Python source into an AST
2. Identify Django model classes and relationships
3. Track QuerySet-producing expressions
4. Track iteration boundaries
5. Detect attribute access that implies ORM resolution
6. Verify whether the required relation is prefetched

This is a static over-approximation. False negatives are preferred over false positives.

## Design philosophy

* Conservative diagnostics
* No guessing about runtime state
* Prefer “I don’t know” over incorrect warnings
* Editor feedback must be actionable, not noisy

## Limitations (work in progress)

* Django 3.2+ only
* Interprocedural analysis requires type hints on QuerySet parameters
* Limited understanding of:
  * `Prefetch` objects
  * `annotate`, `aggregate`
  * complex custom managers
* No SQL-level cost estimation

## Roadmap

* Prefetch object support
* Custom queryset method summaries
* Better integration with `django-stubs`
* Templates integration

## Contributing

This project lives at the intersection of:

* Python AST
* Django ORM semantics
* LSP protocol design

If you are interested in any of those, contributions are welcome. Start by reading the analyzer passes; they are intentionally kept small and explicit.

Documentation contributions are welcome, the goal is make this tool easy to use.

## License

MIT
