# Development

## Linting

Run Rhino linters from the repo root:

```bash
Rscript -e "rhino::lint_r()"
Rscript -e "rhino::lint_js()"
Rscript -e "rhino::lint_sass()"
```

## Tests

```bash
Rscript -e "rhino::test_r()"
```

## Pre-commit hook

Enable hooks once per clone:

```bash
bash scripts/setup-git-hooks.sh
```

The pre-commit hook runs `rhino::lint_r()` automatically before each commit.
