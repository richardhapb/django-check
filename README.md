# django-check

Static N+1 query detection for Django (LSP-based)

`django-check` is a static analyzer and Language Server that detects N+1 query patterns in Django code before runtime. It inspects queryset construction and related-field access to warn when relations are accessed without proper prefetching (`select_related`, `prefetch_related`).

<img width="1049" height="287" alt="image" src="https://github.com/user-attachments/assets/34103185-f6a1-4bc5-950a-ebf2b46b7d9f" />

It works inside the editor or directly from CLI.

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

Runtime tools (django-silk, nplusone) are focuesd in runtime optimiezation. `django-check` make static analysis before the runtime.

## Key properties

* Compute the graph of the `Model`s in the app
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
  * Inheritance
  * Reverse relations (with `related_name` explicit or not)

## Installation

Not available yet, should be compiled from source.


## CLI

```
Usage: djch <COMMAND>

Commands:
  server  Start as a Language Server (normally handled by the IDE)
  check   Analyze the current directory tree for N+1 queries
  help    Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help (see more with '--help')
  -V, --version  Print version
```

**Check from CLI using `djch check`. You will get an output like this:**

```
app/foo/bar/views/tier1.py:210:22: [N+1] rp.ticker_benchmark
Potential N+1 query: accessing `rp.ticker_benchmark` inside loop

app/apps/crawler/tasks.py:48:17: [N+1] ticker.industry
Potential N+1 query: accessing `ticker.industry` inside loop

app/apps/crawler/views.py:62:20: [N+1] stream.streamer
Potential N+1 query: accessing `stream.streamer` inside loop

app/apps/foo/selectors/anointed.py:43:19: [N+1] anointed.pattern
Potential N+1 query: accessing `anointed.pattern` inside loop
```

## Editor integration (LSP)

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

You can install the VSCode extension from, or directly in VSCode Extensions Market Place.

[https://marketplace.visualstudio.com/items?itemName=richardhapb.Django-Check](https://marketplace.visualstudio.com/items?itemName=richardhapb.Django-Check)

## Examples

### Problematic code

```python
# N+1 query detected
users = User.objects.all()
profiles = [user.profile in user for users]  # N+1
```

Explanation:

* `users` is evaluated once
* `user.profile` triggers one query per iteration

### Corrected code

```python
users = User.objects.select_related("profile").all()
profiles = [user.profile in user for users]
```

---

### Problematic code

```python
users = User.objects.all()
for user in users:
    user.profile.bio # N+1
```

Explanation:

* `users` is evaluated once
* `user.profile` triggers one query per iteration

### Corrected code

```python
users = User.objects.select_related("profile").all()
for user in users:
    user.profile.bio
```

`django-check` will clear the diagnostic once the relation is prefetched.

## How it works (high level)

1. Parse Python source into an AST
2. Identify Django model classes and relationships
3. Track QuerySet-producing expressions
4. Track iteration boundaries
5. Detect attribute access that implies ORM resolution
6. Verify whether the required relation is prefetched

## Design philosophy

* Zero config
* Just works out of the box
* Editor feedback must be actionable, not noisy

## Limitations (work in progress)

* Interprocedural analysis requires type hints on QuerySet parameters
* Limited understanding of:
  * `Prefetch` objects
  * `annotate`, `aggregate`
  * complex custom managers
* No complete capture of implicit prefetchs in django chains when has many. e.g.
  `prefetch_releated(chat.users__profile)`, then iterate over `chat.users` and
  access to `profile`. This will raise a warning.

## Roadmap

* Prefetch object support
* Capture of implicits prefetchs in a root instance
* Custom queryset method summaries
* Templates integration

## Contributing

This project lives at the intersection of:

* Python AST
* Django ORM semantics
* LSP protocol design

If you are interested in any of those, contributions are welcome.

Documentation contributions are welcome, the goal is make this tool easy to use.

## License

MIT
